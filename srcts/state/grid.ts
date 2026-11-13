// Helpers for a view's grid, which the `update`, the `migrate` and the dock's
// commands share (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.1#grids).
// They give a grid's canonical form, check a grid, place a panel at its
// default place, take panels out, and give the default grid over a view's
// panels.
// They are ported from today's canonicalize_grid(), validate_dock_grid(),
// prune_tree(), append_default_leaves() and default_grid() in R/dock-grid.R,
// at 655e323.
//
// In a grid in its canonical form, the shares of each split sum to 1.
// No tab group, split or rail is empty, and each tab group and rail shows one
// of its own panels.
// The dock's commands write grids in that form.
// The top split may be empty: that is the grid of a view that shows nothing.

import type { Grid, Node, Panel, Rail, Split, TabGroup } from "./types.ts";

// The widths of a rail in pixels where nothing sets them: open, and
// collapsed.
export const RAIL_SIZE = 260;
export const RAIL_COLLAPSED_SIZE = 35;

// The edges that a view has a rail on, in reading order.
export const EDGES = ["left", "right"] as const;
export type Edge = (typeof EDGES)[number];

// How far from 1 the shares of a split may sum, to allow for rounding, as
// R's all.equal() allows.
const TOLERANCE = 1.5e-8;

export function isTabGroup(node: Node): node is TabGroup {
  return "panels" in node;
}

// Whether two values name the same panel.
export function samePanel(a: Panel, b: Panel): boolean {
  return "block" in a ? "block" in b && a.block === b.block : "plugin" in b && a.plugin === b.plugin;
}

// The panels that a grid places, in reading order: its tab groups, depth
// first, then its rails, left before right.
export function gridPanels(grid: Grid): Panel[] {
  return [...tabGroups(grid).flatMap((group) => group.panels), ...rails(grid).flatMap(([, rail]) => rail.panels)];
}

// Whether a grid places a panel, in a tab group or on a rail.
export function places(grid: Grid, panel: Panel): boolean {
  return gridPanels(grid).some((placed) => samePanel(placed, panel));
}

// The tab groups of a grid's splits, in reading order.
function tabGroups(grid: Grid): TabGroup[] {
  const groups: TabGroup[] = [];
  const walk = (node: Node) => {
    if (isTabGroup(node)) groups.push(node);
    else node.children.forEach(walk);
  };
  grid.children.forEach(walk);
  return groups;
}

// The rails that a grid holds, by edge, in reading order.
function rails(grid: Grid): [Edge, Rail][] {
  return EDGES.flatMap((edge) => {
    const rail = grid.rails?.[edge];
    return rail === undefined ? [] : [[edge, rail] as [Edge, Rail]];
  });
}

// The canonical form of a grid.
// Each split's shares are scaled to sum to 1, or split evenly where the sizes
// do not give each child a positive share.
// Tab groups, splits and rails without panels are dropped.
// A tab group or rail whose `active` is none of its panels shows its first
// panel.
export function canonicalGrid(grid: Grid): Grid {
  return prunePanels(grid, () => true);
}

// The grid without the panels that `keep` refuses.
// Each tab group, split and rail that this empties is dropped, and its share
// goes to its siblings in proportion.
// A tab group or rail whose shown panel went shows its first panel.
// The grid that it gives is in its canonical form.
export function prunePanels(grid: Grid, keep: (panel: Panel) => boolean): Grid {
  const top = prunedSplit(grid, keep) ?? { children: [], sizes: [] };
  const kept = rails(grid).flatMap(([edge, rail]) => {
    const pruned = prunedGroup(rail, keep);
    return pruned === undefined ? [] : [[edge, { ...rail, ...pruned }] as const];
  });
  return {
    orientation: grid.orientation,
    ...top,
    ...(kept.length === 0 ? {} : { rails: Object.fromEntries(kept) }),
  };
}

function prunedNode(node: Node, keep: (panel: Panel) => boolean): Node | undefined {
  return isTabGroup(node) ? prunedGroup(node, keep) : prunedSplit(node, keep);
}

function prunedSplit(split: Split, keep: (panel: Panel) => boolean): Split | undefined {
  const shares = fitted(split.sizes, split.children.length);
  const kept = split.children.flatMap((child, i) => {
    const pruned = prunedNode(child, keep);
    return pruned === undefined ? [] : [{ node: pruned, share: shares[i] ?? 0 }];
  });
  if (kept.length === 0) return undefined;
  return { children: kept.map(({ node }) => node), sizes: normalized(kept.map(({ share }) => share)) };
}

function prunedGroup(group: TabGroup, keep: (panel: Panel) => boolean): TabGroup | undefined {
  const panels = group.panels.filter(keep);
  const [first] = panels;
  if (first === undefined) return undefined;
  const active = panels.find((panel) => samePanel(panel, group.active)) ?? first;
  return { panels, active };
}

// The shares of `n` children: `sizes` scaled to sum to 1, or an even split
// where `sizes` does not give each child a positive share.
function fitted(sizes: readonly number[], n: number): number[] {
  const fits = sizes.length === n && sizes.every((size) => Number.isFinite(size) && size > 0);
  return fits ? normalized(sizes) : Array.from({ length: n }, () => 1 / n);
}

// The shares scaled to sum to 1.
// Shares that sum to 1 already stay exactly as they are, so that a grid in
// its canonical form keeps its shares to the last bit.
function normalized(shares: readonly number[]): number[] {
  const total = sum(shares);
  return Math.abs(total - 1) <= TOLERANCE ? [...shares] : shares.map((share) => share / total);
}

function sum(values: readonly number[]): number {
  return values.reduce((total, value) => total + value, 0);
}

// The checks of a grid that someone writes, which the client and R share.
// Each failure is a sentence that says where in the grid it is, as a JSON
// Pointer from the grid.
// A grid fails where:
// - A split's sizes do not give each child one positive share.
// - A split's shares do not sum to 1.
// - A tab group, split or rail holds no panel.
// - A tab group or rail shows a panel that it does not hold.
// - The grid places a panel more than once.
export function gridFailures(grid: Grid): string[] {
  const failures: string[] = [];

  const split = (node: Split, at: string, top: boolean) => {
    if (!top && node.children.length === 0) failures.push(`The split at "${at}" holds nothing.`);
    if (node.sizes.length !== node.children.length) {
      failures.push(`The split at "${at || "/"}" has ${count(node.children.length, "child", "children")} and ${count(node.sizes.length, "share", "shares")}.`);
    } else if (!node.sizes.every((size) => Number.isFinite(size) && size > 0)) {
      failures.push(`The split at "${at || "/"}" gives a child a share that is not above 0.`);
    } else if (node.children.length > 0 && Math.abs(sum(node.sizes) - 1) > TOLERANCE) {
      failures.push(`The shares of the split at "${at || "/"}" do not sum to 1.`);
    }
    node.children.forEach((child, i) => {
      if (isTabGroup(child)) group(child, `${at}/children/${String(i)}`, "tab group");
      else split(child, `${at}/children/${String(i)}`, false);
    });
  };

  const group = (node: TabGroup, at: string, what: string) => {
    if (node.panels.length === 0) failures.push(`The ${what} at "${at}" holds no panel.`);
    else if (!node.panels.some((panel) => samePanel(panel, node.active))) {
      failures.push(`The ${what} at "${at}" shows a panel that it does not hold.`);
    }
  };

  split(grid, "", true);
  for (const [edge, rail] of rails(grid)) group(rail, `/rails/${edge}`, "rail");

  const seen: Panel[] = [];
  const twice: Panel[] = [];
  for (const panel of gridPanels(grid)) {
    if (seen.some((other) => samePanel(other, panel))) {
      if (!twice.some((other) => samePanel(other, panel))) twice.push(panel);
    } else seen.push(panel);
  }
  for (const panel of twice) failures.push(`The grid places ${panelName(panel)} more than once.`);

  return failures;
}

function count(n: number, one: string, many: string): string {
  return `${String(n)} ${n === 1 ? one : many}`;
}

// How a sentence names a panel.
export function panelName(panel: Panel): string {
  return "block" in panel ? `the panel of the block "${panel.block}"` : `the panel of the plugin "${panel.plugin}"`;
}

// The grid with a panel at its default place.
// The panel goes in a new tab, which the tab group shows, in the last tab
// group in reading order that holds no plugin's panel.
// Where every tab group holds a plugin's panel, the panel gets a tab group of
// its own, as appendTabGroup() adds one.
// So a block's panel never covers the DAG that it was added from.
// A grid that places the panel already stays as it is.
export function placePanel(grid: Grid, panel: Panel): Grid {
  if (places(grid, panel)) return grid;

  const target = tabGroups(grid)
    .reverse()
    .find((group) => group.panels.every((placed) => "block" in placed));
  if (target !== undefined) {
    const add = (node: Node): Node => {
      if (node === target) return { panels: [...target.panels, panel], active: panel };
      return isTabGroup(node) ? node : { ...node, children: node.children.map(add) };
    };
    return { ...grid, children: grid.children.map(add) };
  }

  return appendTabGroup(grid, panel);
}

// The grid with a new tab group at the end of the top split, which holds the
// panel alone.
// The new tab group gets the mean of its siblings' shares, as in today's
// append_default_leaves().
export function appendTabGroup(grid: Grid, panel: Panel): Grid {
  const shares = fitted(grid.sizes, grid.children.length);
  const mean = shares.length === 0 ? 1 : 1 / shares.length;
  return {
    ...grid,
    children: [...grid.children, { panels: [panel], active: panel }],
    sizes: normalized([...shares, mean]),
  };
}

// The default grid over a view's panels.
// The plugins' panels share one tab group, and the blocks' panels share
// another, beside it, in an even split.
// Each tab group shows its first panel, and a grid without panels has no tab
// group.
export function defaultGrid(panels: readonly Panel[]): Grid {
  const groups = [panels.filter((panel) => "plugin" in panel), panels.filter((panel) => "block" in panel)].flatMap(
    (held) => {
      const [first] = held;
      return first === undefined ? [] : [{ panels: held, active: first }];
    },
  );
  return { orientation: "horizontal", children: groups, sizes: fitted([], groups.length) };
}
