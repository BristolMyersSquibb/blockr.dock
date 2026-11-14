// The step that the dock takes as it starts on a board, before it first
// draws, within its frontend's render()
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.1#starting-on-a-board):
// 1. Where the dock's state holds no view, it adds the view "Page", under the
//    id page, and makes it active. The view has the panels of the app's
//    plugins whose class has a `ui` on its left rail, open and at the default
//    width, and one tab group with the panel of each of the board's blocks,
//    which shows the first, as today's default_layout() in R/dock-grid.R lays
//    out a fresh board, at 655e323.
// 2. It removes from every view each panel of a plugin that the app does not
//    run or whose class has no `ui`, since the dock cannot draw it.
// 3. Where the URL's query parameter `view` names one of the views by id, it
//    makes that view active, while an id that names no view changes nothing.
// It applies all of it as one delta with the origin of the dock plugin's
// class and `undo: false`, so that undo never takes the user back to an
// empty page or to a panel that the app cannot draw.
// The view has a fixed id, so that a fresh start of the same app on the same
// board gives the same id, and a link with ?view=page reaches it.
// The ids of the views that the dock adds through its commands are minted, but
// render() has nothing that mints one.

import type { Board, CoreApi, Patch, PluginClassId } from "@blockr/core";
import { DOCK, stateOf } from "../../commands/index.ts";
import { gridPanels, keyIn, pointer, prunePanels, RAIL_COLLAPSED_SIZE, RAIL_SIZE } from "../../state/index.ts";
import type { Grid, Panel, View, ViewId } from "../../state/index.ts";

// The id and the name of the view that the dock adds to a state that holds
// none.
export const PAGE_ID = "page";
export const PAGE_NAME = "Page";

// The patches of the step on starting, on a board whose dock state is of the
// state's shape, given the ids of the app's plugins whose class has a `ui`, in
// the order of the app's plugins, and the view that the URL names.
export function startingPatches(board: Board, drawn: readonly PluginClassId[], view: string | undefined): Patch[] {
  const state = stateOf(board);
  if (state === undefined) return [];
  const at = (...segments: string[]) => pointer(["plugins", DOCK, ...segments]);
  const patches: Patch[] = [];

  const added = Object.keys(state.views).length === 0;
  let views: Readonly<Record<ViewId, View>> = state.views;
  if (added) {
    const page: View = { name: PAGE_NAME, folder: null, index: keyIn(state, null, undefined), grid: startingGrid(board, drawn) };
    patches.push({ op: "add", path: at("views", PAGE_ID), value: page });
    views = { [PAGE_ID]: page };
  }

  const draws = (panel: Panel) => "block" in panel || drawn.includes(panel.plugin);
  for (const [id, held] of Object.entries(state.views)) {
    if (gridPanels(held.grid).every(draws)) continue;
    patches.push({ op: "replace", path: at("views", id, "grid"), value: prunePanels(held.grid, draws) });
  }

  const named = view !== undefined && Object.hasOwn(views, view) ? view : undefined;
  const active = named ?? (added ? PAGE_ID : state.active);
  if (active !== state.active) patches.push({ op: "replace", path: at("active"), value: active });
  return patches;
}

// The grid of the view that the dock adds to a state that holds none.
function startingGrid(board: Board, drawn: readonly PluginClassId[]): Grid {
  const blocks: Panel[] = Object.keys(board.blocks).map((block) => ({ block }));
  const plugins: Panel[] = drawn.map((plugin) => ({ plugin }));
  const [block] = blocks;
  const [plugin] = plugins;
  return {
    orientation: "horizontal",
    children: block === undefined ? [] : [{ panels: blocks, active: block }],
    sizes: block === undefined ? [] : [1],
    ...(plugin === undefined
      ? {}
      : { rails: { left: { panels: plugins, active: plugin, collapsed: false, size: RAIL_SIZE, collapsed_size: RAIL_COLLAPSED_SIZE } } }),
  };
}

// The ids of the app's plugins whose class has a `ui`, which the dock places
// in panels, each with its name, in the order of the app's plugins.
export function drawnPlugins(core: Pick<CoreApi, "plugins">): ReadonlyMap<PluginClassId, string> {
  const plugins = core.plugins.appPlugins();
  return new Map(
    Object.entries(plugins).flatMap(([id, plugin]) => (core.plugins.pluginClass(id).ui === undefined ? [] : [[id, plugin.name] as const])),
  );
}

// The view that the URL of the page names in its query parameter `view`, or
// undefined where it names none.
export function urlView(location: Pick<Location, "search"> | undefined): string | undefined {
  const view = new URLSearchParams(location?.search ?? "").get("view");
  return view === null || view === "" ? undefined : view;
}

// Takes the step on starting, as one delta of the dock plugin's class, which
// undo does not take back.
export function startOnBoard(core: Pick<CoreApi, "store">, plugin: PluginClassId, drawn: readonly PluginClassId[], view: string | undefined): void {
  const patches = startingPatches(core.store.board(), drawn, view);
  if (patches.length > 0) core.store.apply({ patches }, plugin, { undo: false });
}
