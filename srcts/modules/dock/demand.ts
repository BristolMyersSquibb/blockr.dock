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

import type { BlockId, Board, CoreApi, PluginClassId } from "@blockr/core";
import { stateOf } from "../../commands/index.ts";
import { gridToLayout, layoutToGrid } from "../../layout/index.ts";
import { EDGES, tabGroups } from "../../state/index.ts";
import type { Panel } from "../../state/index.ts";
import type { Narrow } from "./narrow.ts";

// The owner of the demand, and its evaluation priority.
export const OWNER = "blockr.dock";
export const SHOWN = 1;

// The blocks whose cards are on screen, on a board where the dock draws the
// panels of the plugins given, by the ids of their classes.
export function shownBlocks(board: Board, drawn: ReadonlyMap<PluginClassId, string>, narrow: boolean): BlockId[] {
  const state = stateOf(board);
  const active = state?.active ?? null;
  const view = active === null ? undefined : state?.views[active];
  if (view === undefined) return [];

  const title = (panel: Panel) => ("block" in panel ? board.blocks[panel.block]?.name : drawn.get(panel.plugin));
  const grid = layoutToGrid(gridToLayout(view.grid, title, { width: 0, height: 0 }));
  const shown = [
    ...tabGroups(grid).map((group) => group.active),
    ...EDGES.flatMap((edge) => {
      const rail = grid.rails?.[edge];
      return rail === undefined || (rail.collapsed && !narrow) ? [] : [rail.active];
    }),
  ];
  return shown.flatMap((panel) => ("block" in panel ? [panel.block] : []));
}

// Demands the blocks whose cards are on screen, until the returned function
// runs.
// The narrow column shows the panels of collapsed rails too, so the demand
// takes the blocks again as the window crosses the breakpoint.
export function demandShown(core: Pick<CoreApi, "scheduler">, drawn: ReadonlyMap<PluginClassId, string>, narrow: Narrow): () => void {
  const blocks = () => (board: Board) => shownBlocks(board, drawn, narrow.get());
  const demand = core.scheduler.demand(OWNER, blocks(), { priority: SHOWN });
  const off = narrow.subscribe(() => {
    demand.set(blocks(), { priority: SHOWN });
  });
  return () => {
    off();
    demand.release();
  };
}
