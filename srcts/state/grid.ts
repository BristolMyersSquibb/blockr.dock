// Helpers for a view's grid, which the `update` and the dock's commands share
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.1#grids).
// They give a grid's canonical form, check a grid, place a panel at its
// default place or where a command names, take panels out, resize the tab
// group or rail that holds a panel, and tell what a new grid changes of a
// stored one, which a locked board keeps.
// They are ported from today's canonicalize_grid(), validate_dock_grid(),
// prune_tree() and append_default_leaves() in R/dock-grid.R, and from
// hint_to_position() in R/panel-ops.R, at 655e323.
//
// In a grid in its canonical form, the shares of each split sum to 1.
// No tab group, split or rail is empty, and each tab group and rail shows one
// of its own panels.
// The dock's commands write grids in that form.
// The top split may be empty: that is the grid of a view that shows nothing.

import type { Json } from "@blockr/core";
import type { Grid, Node, Panel, Rail, Split, TabGroup } from "./types.ts";

// The edges that a view has a rail on, in reading order.
export const EDGES = ["left", "right"] as const;
export type Edge = (typeof EDGES)[number];

// Where a panel goes beside the tab group of another panel: in it, as a new
// tab, or in a new tab group on one of its sides.
export const SIDES = ["within", "left", "right", "above", "below"] as const;
export type Side = (typeof SIDES)[number];

// The widths in pixels of a rail that a panel opens, open and collapsed, as
// today's rail() has them.
const RAIL_SIZE = 260;
const RAIL_COLLAPSED_SIZE = 35;

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
function appendTabGroup(grid: Grid, panel: Panel): Grid {
  return { ...grid, ...inserted(grid, grid.children.length, tabGroup(panel)) };
}

// The split with a node among its children at `at`, which gets the mean of
// its siblings' shares.
// The siblings share the rest in proportion.
function inserted(split: Split, at: number, node: Node): Split {
  const shares = fitted(split.sizes, split.children.length);
  const mean = shares.length === 0 ? 1 : 1 / shares.length;
  return {
    children: [...split.children.slice(0, at), node, ...split.children.slice(at)],
    sizes: normalized([...shares.slice(0, at), mean, ...shares.slice(at)]),
  };
}

// The split with a node among its children at `at`, where each child gets the
// same share, as dockview's Sizing.Distribute gives them.
function evened(split: Split, at: number, node: Node): Split {
  const children = [...split.children.slice(0, at), node, ...split.children.slice(at)];
  return { children, sizes: children.map(() => 1 / children.length) };
}

// The split with a node before or after its child at `beside`, which takes
// half of that child's share, as dockview's Sizing.Split gives it.
function halved(split: Split, beside: number, before: boolean, node: Node): Split {
  const shares = fitted(split.sizes, split.children.length);
  const half = (shares[beside] ?? 0) / 2;
  const sizes = shares.map((share, i) => (i === beside ? half : share));
  const at = before ? beside : beside + 1;
  return {
    children: [...split.children.slice(0, at), node, ...split.children.slice(at)],
    sizes: [...sizes.slice(0, at), half, ...sizes.slice(at)],
  };
}

function tabGroup(panel: Panel): TabGroup {
  return { panels: [panel], active: panel };
}

// Where a command places a panel in a view: in or beside the tab group of the
// panel that `near` names, on the side that `side` names, within unless
// given, or on the rail that `rail` names.
// A rail takes precedence over `near` and `side`, as in today's
// hint_to_position().
// A `side` without `near` places the panel at that edge of the whole grid.
// Without any, the panel goes to its default place.
export interface Place {
  readonly near?: Panel;
  readonly side?: Side;
  readonly rail?: Edge;
}

// Where a grid holds a panel: in the tab group at the path of child indices
// from the top split, or on the rail of an edge.
export type Location = { readonly path: readonly number[] } | { readonly edge: Edge };

// Where a grid holds a panel, or undefined where it holds none.
export function locate(grid: Grid, panel: Panel): Location | undefined {
  const walk = (node: Node, path: readonly number[]): readonly number[] | undefined => {
    if (isTabGroup(node)) return node.panels.some((placed) => samePanel(placed, panel)) ? path : undefined;
    for (const [i, child] of node.children.entries()) {
      const found = walk(child, [...path, i]);
      if (found !== undefined) return found;
    }
    return undefined;
  };
  for (const [i, child] of grid.children.entries()) {
    const path = walk(child, [i]);
    if (path !== undefined) return { path };
  }
  const held = rails(grid).find(([, rail]) => rail.panels.some((placed) => samePanel(placed, panel)));
  return held === undefined ? undefined : { edge: held[0] };
}

// Why a grid cannot take a panel at a place, in a sentence, or undefined
// where it can: the panel that `near` names has to be in the grid, and one
// on a rail takes a panel only within.
export function placeFailure(grid: Grid, place: Place): string | undefined {
  if (place.rail !== undefined || place.near === undefined) return undefined;
  const at = locate(grid, place.near);
  if (at === undefined) return `The view does not show ${panelName(place.near)}, which near names.`;
  if ("edge" in at && (place.side ?? "within") !== "within") {
    return `The view shows ${panelName(place.near)}, which near names, on a rail, which takes a panel only within.`;
  }
  return undefined;
}

// The grid with a panel placed where `place` says, which the grid holds
// nowhere yet, as the tab group or rail that shows it.
// A panel beside a tab group, or at an edge of the whole grid, goes in a new
// tab group, with the share that dockview 8.9.0 gives a group that it adds
// there.
// Beside a tab group in the split that holds it, the new tab group gets an
// even share of the split, as dockview's addPanel() gives it, with `sizing`
// "distribute".
// With "split", it takes half of the tab group's share instead, as
// dockview's moveTo() and a drag give it under its default dropSizing.
// Where that split divides the other way, the new tab group takes half of the
// tab group's place with either.
// At an edge of the whole grid, it gets an even share of the top split, as a
// drop on the dock's edge gives it, or half of the whole grid where the top
// split divides the other way, with either.
// A panel on a rail that the view lacks opens the rail, at the default
// widths.
// It throws where placeFailure() names a failure.
export function placedAt(grid: Grid, panel: Panel, place: Place, sizing: "split" | "distribute"): Grid {
  const failure = placeFailure(grid, place);
  if (failure !== undefined) throw new Error(failure);
  if (place.rail !== undefined) return onRail(grid, place.rail, panel);
  const side = place.side ?? "within";
  if (place.near === undefined) return side === "within" ? placePanel(grid, panel) : atEdge(grid, panel, side);
  const at = locate(grid, place.near);
  if (at === undefined) return grid;
  if ("edge" in at) return onRail(grid, at.edge, panel);
  if (side === "within") {
    return withNode(grid, at.path, (node) => (isTabGroup(node) ? { panels: [...node.panels, panel], active: panel } : node));
  }
  return besideGroup(grid, at.path, panel, side, sizing);
}

// The grid with a panel on the rail of an edge, as its last tab, which the
// rail shows, and with a rail there where it has none.
function onRail(grid: Grid, edge: Edge, panel: Panel): Grid {
  const rail = grid.rails?.[edge];
  const next: Rail =
    rail === undefined
      ? { ...tabGroup(panel), collapsed: false, size: RAIL_SIZE, collapsed_size: RAIL_COLLAPSED_SIZE }
      : { ...rail, panels: [...rail.panels, panel], active: panel };
  return { ...grid, rails: { ...grid.rails, [edge]: next } };
}

// The orientation in which a side divides: left and right side by side,
// above and below one over the other.
function orientationOf(side: Exclude<Side, "within">): Grid["orientation"] {
  return side === "left" || side === "right" ? "horizontal" : "vertical";
}

// Whether a side comes before what it is beside, in reading order.
function leads(side: Exclude<Side, "within">): boolean {
  return side === "left" || side === "above";
}

// The grid with a panel in a new tab group beside the tab group at `path`.
function besideGroup(grid: Grid, path: readonly number[], panel: Panel, side: Exclude<Side, "within">, sizing: "split" | "distribute"): Grid {
  const holder = path.slice(0, -1);
  const at = path.at(-1) ?? 0;
  // The top split divides as the grid's orientation says, and each split
  // inside divides the other way from the split that holds it.
  const divides = holder.length % 2 === 0 ? grid.orientation : flipped(grid.orientation);
  if (divides === orientationOf(side)) {
    return withSplit(grid, holder, (split) =>
      sizing === "split" ? halved(split, at, leads(side), tabGroup(panel)) : evened(split, leads(side) ? at : at + 1, tabGroup(panel)),
    );
  }
  return withNode(grid, path, (node) => ({
    children: leads(side) ? [tabGroup(panel), node] : [node, tabGroup(panel)],
    sizes: [0.5, 0.5],
  }));
}

// The grid with a panel in a new tab group at an edge of the whole grid.
// Where the top split divides the other way, the top split goes beside the
// new tab group, inside a new top split.
function atEdge(grid: Grid, panel: Panel, side: Exclude<Side, "within">): Grid {
  const orientation = orientationOf(side);
  if (grid.children.length === 0) return { ...grid, orientation, children: [tabGroup(panel)], sizes: [1] };
  if (grid.orientation === orientation) {
    return { ...grid, ...evened(grid, leads(side) ? 0 : grid.children.length, tabGroup(panel)) };
  }
  // A top split that holds one tab group alone divides nothing, so the tab
  // group goes beside the new one without it.
  const [only] = grid.children;
  const top: Node =
    grid.children.length === 1 && only !== undefined && isTabGroup(only)
      ? only
      : { children: grid.children, sizes: grid.sizes };
  return {
    ...grid,
    orientation,
    children: leads(side) ? [tabGroup(panel), top] : [top, tabGroup(panel)],
    sizes: [0.5, 0.5],
  };
}

function flipped(orientation: Grid["orientation"]): Grid["orientation"] {
  return orientation === "horizontal" ? "vertical" : "horizontal";
}

// The grid with the node at `path` replaced by what `fn` gives for it.
function withNode(grid: Grid, path: readonly number[], fn: (node: Node) => Node): Grid {
  const walk = (split: Split, rest: readonly number[]): Split => {
    const [i, ...deeper] = rest;
    return {
      ...split,
      children: split.children.map((child, j) => {
        if (j !== i) return child;
        if (deeper.length === 0) return fn(child);
        return isTabGroup(child) ? child : walk(child, deeper);
      }),
    };
  };
  return { ...grid, ...walk(grid, path) };
}

// The grid with the split at `path`, the top split for an empty path,
// replaced by what `fn` gives for it.
function withSplit(grid: Grid, path: readonly number[], fn: (split: Split) => Split): Grid {
  if (path.length === 0) return { ...grid, ...fn({ children: grid.children, sizes: grid.sizes }) };
  return withNode(grid, path, (node) => (isTabGroup(node) ? node : fn(node)));
}

// The grid with the tab group or rail that holds a panel showing it.
export function showing(grid: Grid, panel: Panel): Grid {
  const at = locate(grid, panel);
  if (at === undefined) return grid;
  if ("path" in at) return withNode(grid, at.path, (node) => (isTabGroup(node) ? { ...node, active: panel } : node));
  const rail = grid.rails?.[at.edge];
  return rail === undefined ? grid : { ...grid, rails: { ...grid.rails, [at.edge]: { ...rail, active: panel } } };
}

// The grid with the rail of an edge collapsed, or open.
export function collapsedRail(grid: Grid, edge: Edge, collapsed: boolean): Grid {
  const rail = grid.rails?.[edge];
  return rail === undefined ? grid : { ...grid, rails: { ...grid.rails, [edge]: { ...rail, collapsed } } };
}

// The grid with the tab group that holds a panel taking `size` of the split
// that holds it, a ratio between 0 and 1, while the other children share the
// rest in proportion, or with the rail that holds it `size` pixels wide while
// it is open.
// It throws where the tab group fills its split alone, and where `size` is
// not below 1 for a tab group.
export function resized(grid: Grid, panel: Panel, size: number): Grid {
  const at = locate(grid, panel);
  if (at === undefined) return grid;
  if ("edge" in at) {
    const rail = grid.rails?.[at.edge];
    return rail === undefined ? grid : { ...grid, rails: { ...grid.rails, [at.edge]: { ...rail, size } } };
  }
  if (size >= 1) {
    throw new Error(`A tab group takes a share of its split below 1, which ${String(size)} is not.`);
  }
  const i = at.path.at(-1) ?? 0;
  return withSplit(grid, at.path.slice(0, -1), (split) => {
    if (split.children.length < 2) {
      throw new Error(`The tab group that holds ${panelName(panel)} fills its split alone, so its share stays 1.`);
    }
    const shares = fitted(split.sizes, split.children.length);
    const rest = sum(shares) - (shares[i] ?? 0);
    return { ...split, sizes: shares.map((share, j) => (j === i ? size : (share / rest) * (1 - size))) };
  });
}

// What a new grid changes of a stored one, in the parts that a locked board
// tells apart:
// - Its panels, in `panels`, where it holds other panels, which a reader
//   never adds or removes.
// - Their arrangement, in `arrangement`, which the blockr option
//   lock_rearrange keeps: its orientation, its splits and tab groups, the
//   rails it holds, and which panels each tab group and rail holds, in order.
// - Their sizes, in `sizes`, which the blockr option lock_resize keeps: the
//   shares of a split, where the new grid has the same splits and tab groups,
//   and the widths of a rail that both grids hold.
// None counts which panel a tab group or rail shows, or whether a rail is
// collapsed, which a reader always changes.
export interface GridChanges {
  readonly panels: boolean;
  readonly arrangement: boolean;
  readonly sizes: boolean;
}

export function gridChanges(stored: Grid, grid: Grid): GridChanges {
  const before = gridPanels(stored);
  const after = gridPanels(grid);
  return {
    panels: before.length !== after.length || !after.every((panel) => before.some((other) => samePanel(other, panel))),
    arrangement: JSON.stringify(arrangement(grid)) !== JSON.stringify(arrangement(stored)),
    sizes: resizedFrom(stored, grid),
  };
}

// The arrangement of a grid, as gridChanges() compares it, with its panels,
// or its shape alone, without them.
function arrangement(grid: Grid, panels = true): Json {
  const node = (child: Node): Json =>
    isTabGroup(child) ? (panels ? child.panels : "tab group") : child.children.map(node);
  const edges = rails(grid).map(([edge, rail]): [Edge, Json] => [edge, panels ? rail.panels : "rail"]);
  return { orientation: grid.orientation, children: grid.children.map(node), rails: Object.fromEntries(edges) };
}

// Whether a new grid changes the sizes of a stored one.
function resizedFrom(stored: Grid, grid: Grid): boolean {
  const widths = EDGES.some((edge) => {
    const [a, b] = [stored.rails?.[edge], grid.rails?.[edge]];
    return a !== undefined && b !== undefined && (a.size !== b.size || a.collapsed_size !== b.collapsed_size);
  });
  if (widths) return true;
  if (JSON.stringify(arrangement(stored, false)) !== JSON.stringify(arrangement(grid, false))) return false;
  const given = shares(grid);
  return shares(stored).some((share, i) => Math.abs(share - (given[i] ?? share)) > TOLERANCE);
}

// The shares of every split of a grid, the top split first, depth first.
function shares(grid: Grid): number[] {
  const walk = (split: Split): number[] => [
    ...split.sizes,
    ...split.children.flatMap((child) => (isTabGroup(child) ? [] : walk(child))),
  ];
  return walk(grid);
}
