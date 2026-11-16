// What the dock demands
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.7#demand):
// the blocks whose cards are on screen, at the evaluation priority 1, above
// the default.
// A card is on screen where its panel is the one that a tab group of the
// active view shows, or that a rail of the active view shows while the rail
// is open, as dockview draws the view's grid.
// In the narrow column, every rail is a row, so the panel that a rail shows
// is on screen whether the rail is collapsed or not.
// A card whose sections are both closed is on screen too, so that its status
// dot stays current.
// The demand is a function of the board, which the scheduler calls again
// after every change to the board, so it follows the active view and its grid
// without a subscription of its own.
// The dock makes it within its frontend's render(), so that the scheduler
// evaluates the blocks of the opening view first.
//
// As the demand follows the board, the dock sets CoreApi's onScreen to the
// blocks whose cards are on screen and to the plugins whose panels are, by
// the same rule
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.12#what-a-frontend-offers-other-plugins).
// It sets it within render() too, and again after every change to the board
// and as the window crosses the breakpoint of the narrow column.
// Where the dock sets the same blocks and plugins again, core changes nothing
// and tells no plugin.

import type { BlockId, Board, CoreApi, PluginClassId } from "@blockr/core";
import { dockState } from "../../commands/index.ts";
import { gridToLayout, layoutToGrid } from "../../layout/index.ts";
import { EDGES, tabGroups } from "../../state/index.ts";
import type { Panel } from "../../state/index.ts";
import type { Narrow } from "./narrow.ts";

// The owner of the demand, and its evaluation priority.
export const OWNER = "blockr.dock";
export const SHOWN = 1;

// The blocks and the plugins whose panels are on screen, as CoreApi's
// onScreen takes them, on a board where the dock draws the panels of the
// plugins given, by the ids of their classes.
export function onScreen(
  board: Board,
  drawn: ReadonlyMap<PluginClassId, string>,
  narrow: boolean,
): { blocks: BlockId[]; plugins: PluginClassId[] } {
  const state = dockState(board);
  const active = state?.active ?? null;
  const view = active === null ? undefined : state?.views[active];
  if (view === undefined) return { blocks: [], plugins: [] };

  const title = (panel: Panel) => ("block" in panel ? board.blocks[panel.block]?.name : drawn.get(panel.plugin));
  const grid = layoutToGrid(gridToLayout(view.grid, title, { width: 0, height: 0 }));
  const shown = [
    ...tabGroups(grid).map((group) => group.active),
    ...EDGES.flatMap((edge) => {
      const rail = grid.rails?.[edge];
      return rail === undefined || (rail.collapsed && !narrow) ? [] : [rail.active];
    }),
  ];
  return {
    blocks: shown.flatMap((panel) => ("block" in panel ? [panel.block] : [])),
    plugins: shown.flatMap((panel) => ("plugin" in panel ? [panel.plugin] : [])),
  };
}

// Demands the blocks whose cards are on screen, and sets CoreApi's onScreen,
// until the returned function runs.
// The narrow column shows the panels of collapsed rails too, so the demand
// takes the blocks again as the window crosses the breakpoint.
export function demandShown(
  core: Pick<CoreApi, "scheduler" | "store" | "onScreen">,
  drawn: ReadonlyMap<PluginClassId, string>,
  narrow: Narrow,
): () => void {
  const blocks = () => (board: Board) => onScreen(board, drawn, narrow.get()).blocks;
  const demand = core.scheduler.demand(OWNER, blocks(), { priority: SHOWN });
  const tell = (board: Board) => {
    core.onScreen.set(onScreen(board, drawn, narrow.get()));
  };
  tell(core.store.board());
  const offBoard = core.store.subscribe((_change, board) => {
    tell(board);
  });
  const offNarrow = narrow.subscribe(() => {
    demand.set(blocks(), { priority: SHOWN });
    tell(core.store.board());
  });
  return () => {
    offNarrow();
    offBoard();
    demand.release();
  };
}
