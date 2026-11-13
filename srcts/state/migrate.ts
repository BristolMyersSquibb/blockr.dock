// The `migrate` of the dock plugin's class
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.1#the-migration-from-version-1).
// It takes the dock's state from the version that a board records to version
// 2, the shape of ./types.ts.
//
// Version 1 is today's shape, as the conversion in blockr.convert writes it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.2#the-dock-board).
// It holds the views, each with its name and members, the grids of the views
// beside them, the active view, and the sections that each card shows.
// A panel there is a string: block_panel-a for the panel of the block a, and
// ext_panel-blockr.dag-dag_plugin for the panel of that plugin.
// A value of length one may be bare, as typedjson read it, as in "sizes": 1
// and "panels": "block_panel-a".
//
// The migration follows the rules of the page:
// 1. It reads each value of length one as an array where version 2 has one,
//    and "rails": [] as a grid without rails.
// 2. It turns each view's members and grid into one grid, as today's
//    view_grid() reads a view. The grid is restricted to the members, and
//    each member that the grid lacks gets a tab group of its own at the end
//    of the top split. A view without a grid, or without members, gets the
//    default grid over its members.
// 3. It names each panel by an object, by the prefix of its id, and drops a
//    rail's `position`, since the rail's edge gives it.
// 4. It puts every view at the top, with a key in `index` in the order of the
//    views, and gives the state no folders.
// 5. It names a view without a name after its id, as today's views menu
//    labels one. It keeps `active` and `visible`, with each value of
//    `visible` as an array.
// It reads a grid as today's dock does: a panel that a rail holds leaves the
// tree, and a panel that the grid places twice stays where it comes first.
// It also drops what version 2 does not keep:
// - A rail without panels.
// - The section `ctrl` of today's cards.
// - An entry of `visible` that lists both sections, since a card without an
//   entry shows both.

import type { Json } from "@blockr/core";
import { generateNKeysBetween } from "fractional-indexing";
import {
  appendTabGroup,
  defaultGrid,
  EDGES,
  places,
  prunePanels,
  RAIL_COLLAPSED_SIZE,
  RAIL_SIZE,
  samePanel,
} from "./grid.ts";
import { isList, isObject } from "./json.ts";
import type { DockState, Grid, Node, Panel, Rail, Section, Split, View } from "./types.ts";

export function migrate(state: Json, version: number): Json {
  return version < 2 ? fromVersion1(state) : state;
}

function fromVersion1(state: Json): DockState {
  const read = record(state);
  const views = record(read.views);
  const grids = record(read.grids);
  const ids = Object.keys(views);
  const keys = generateNKeysBetween(null, null, ids.length);

  const migrated: Record<string, View> = {};
  ids.forEach((id, i) => {
    const view = record(views[id]);
    const members = unique(array(view.members).flatMap(panelOf));
    migrated[id] = {
      name: typeof view.name === "string" && view.name.trim() !== "" ? view.name : nameFromId(id),
      folder: null,
      index: keys[i] ?? "",
      grid: viewGrid(grids[id], members),
    };
  });

  const active = typeof read.active === "string" && Object.hasOwn(migrated, read.active) ? read.active : (ids[0] ?? null);
  return { views: migrated, folders: {}, active, visible: sections(record(read.visible)) };
}

// A view's grid from its grid at version 1 and its members.
function viewGrid(stored: Json | undefined, members: readonly Panel[]): Grid {
  if (!isObject(stored) || members.length === 0) return defaultGrid(members);

  // The rails claim their panels first, so that the tree loses them, and the
  // tree then keeps the first place of each panel.
  const placed: Panel[] = [];
  const rails = railsOf(stored.rails, placed);
  const tree = splitOf(stored, placed);
  const grid: Grid = {
    orientation: stored.orientation === "vertical" ? "vertical" : "horizontal",
    ...tree,
    ...(rails === undefined ? {} : { rails }),
  };

  const restricted = prunePanels(grid, (panel) => members.some((member) => samePanel(member, panel)));
  return members.filter((member) => !places(restricted, member)).reduce(appendTabGroup, restricted);
}

// A split at version 2 from one at version 1.
// Its tab groups leave out the panels that `placed` holds already, and add
// the panels that they keep to `placed`.
// A child that it cannot read becomes an empty split, which prunePanels()
// drops, giving its share to its siblings.
function splitOf(node: Readonly<Record<string, Json>>, placed: Panel[]): Split {
  const children = array(node.children).map((child): Node => {
    if (!isObject(child)) return { children: [], sizes: [] };
    if (Object.hasOwn(child, "panels")) return tabGroupOf(child, placed) ?? { children: [], sizes: [] };
    return splitOf(child, placed);
  });
  const sizes = array(node.sizes).map((size) => (typeof size === "number" ? size : Number.NaN));
  return { children, sizes };
}

function tabGroupOf(node: Readonly<Record<string, Json>>, placed: Panel[]): { panels: Panel[]; active: Panel } | undefined {
  const panels = unique(array(node.panels).flatMap(panelOf)).filter(
    (panel) => !placed.some((other) => samePanel(other, panel)),
  );
  const [first] = panels;
  if (first === undefined) return undefined;
  placed.push(...panels);
  const [shown] = typeof node.active === "string" ? panelOf(node.active) : [];
  return { panels, active: panels.find((panel) => shown !== undefined && samePanel(panel, shown)) ?? first };
}

// A grid's rails at version 2, from its rails at version 1.
// Version 1 holds them by edge, or as a list in which each rail names its
// `position`.
// It gives undefined where no rail holds a panel.
function railsOf(stored: Json | undefined, placed: Panel[]): Grid["rails"] {
  const list = isList(stored) ? stored.filter(isObject) : [];
  const byEdge = isObject(stored) ? stored : {};
  const rails = EDGES.flatMap((edge) => {
    const rail = byEdge[edge] ?? list.find((other) => other.position === edge);
    if (!isObject(rail)) return [];
    const group = tabGroupOf(rail, placed);
    if (group === undefined) return [];
    const read: Rail = {
      ...group,
      collapsed: rail.collapsed === true,
      size: pixels(rail.size, RAIL_SIZE),
      collapsed_size: pixels(rail.collapsed_size, RAIL_COLLAPSED_SIZE),
    };
    return [[edge, read] as const];
  });
  return rails.length === 0 ? undefined : Object.fromEntries(rails);
}

// A width in pixels as version 1 holds it, or `fallback` where it holds none.
function pixels(value: Json | undefined, fallback: number): number {
  const [width] = array(value);
  return typeof width === "number" && Number.isFinite(width) && width >= 0 ? width : fallback;
}

// The panel that a panel's id names, by its prefix: one panel in an array,
// or none.
function panelOf(id: Json): Panel[] {
  if (typeof id !== "string") return [];
  for (const [prefix, panel] of [
    ["block_panel-", (rest: string): Panel => ({ block: rest })],
    ["ext_panel-", (rest: string): Panel => ({ plugin: rest })],
  ] as const) {
    if (id.startsWith(prefix) && id.length > prefix.length) return [panel(id.slice(prefix.length))];
  }
  return [];
}

// The sections that each card shows, as arrays, with each entry that lists
// both sections dropped.
function sections(visible: Readonly<Record<string, Json>>): Record<string, Section[]> {
  const kept: Record<string, Section[]> = {};
  for (const [block, value] of Object.entries(visible)) {
    const shown = (["inputs", "outputs"] as const).filter((section) => array(value).includes(section));
    if (shown.length < 2) kept[block] = shown;
  }
  return kept;
}

// A view's name from its id, as today's name_from_id() gives it: with each
// ".", "_" and "-" as a space, and its first letter in upper case.
function nameFromId(id: string): string {
  const spaced = id.replace(/[._-]/g, " ");
  return spaced.charAt(0).toUpperCase() + spaced.slice(1);
}

// The panels without repeats, each where it first comes.
function unique(panels: readonly Panel[]): Panel[] {
  return panels.filter((panel, i) => panels.findIndex((other) => samePanel(other, panel)) === i);
}

// A value as an array: an array as it is, a value of length one in an array
// of its own, and nothing as an empty array.
function array(value: Json | undefined): readonly Json[] {
  if (value === undefined || value === null) return [];
  return isList(value) ? value : [value];
}


// A value as an object, or an empty object where it is none, as typedjson
// writes an empty list as [].
function record(value: Json | undefined): Readonly<Record<string, Json>> {
  return isObject(value) ? value : {};
}

