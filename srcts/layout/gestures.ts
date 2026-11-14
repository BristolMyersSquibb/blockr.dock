// What the dock runs after a gesture, as Drawing a view has it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.1#drawing-a-view).
// The dock reads dockview's layout back into a grid once the user lets go,
// and compares it with the grid that dockview showed before the gesture:
// - Where only the panel that a tab group or rail shows changed, it runs
//   blockr.dock-activate_panel, which takes no undo step.
// - Where only whether a rail is collapsed changed, it runs
//   blockr.dock-collapse_rail, which takes none either.
// - Where anything else changed, it runs blockr.dock-set_grid, which takes
//   one.
// - Where nothing changed, it runs nothing.
// It compares with what dockview showed, rather than with the stored grid, so
// that a stored grid out of its canonical form, which dockview draws in that
// form, changes nothing until the user does.
//
// Dockview derives its shares from whole pixels, and a window resize moves
// them, so the shares that dockview gives move a little where nobody resized
// anything.
// So a split keeps the shares that dockview showed before, unless the gesture
// was a drag that moved one of them by more than the blockr option
// dock_grid_size_tol.
// Dockview also narrows a rail that a narrow dock cannot hold, and leaves it
// narrow, so a rail keeps its stored widths unless the gesture was a drag
// that left the dock's own width alone and moved the rail's edge, as a drag
// of the rail's sash does, as today's grid mirror in R/board-server.R has it,
// at 655e323.
// A drag of another sash leaves the stored width of a rail that dockview
// narrowed before.
//
// In the narrow column, only which panel a row shows and which panels it
// holds count, since the column's arrangement and sizes are the column's
// alone.
// There, the dock runs blockr.dock-activate_panel for a panel that a row
// shows now and blockr.dock-remove_panel for a panel that a row lost.

import type { Json } from "@blockr/core";
import { EDGES, gridChanges, gridPanels, samePanel, tabGroups, withStoredShares } from "../state/index.ts";
import type { Edge, Grid, Panel, Rail, ViewId } from "../state/index.ts";

// A command that the dock runs, with its args.
export interface Run {
  readonly id: string;
  readonly args: Json;
}

// What a gesture could have changed besides the arrangement.
export interface Reading {
  // Whether it could have moved the shares of a split: a drag.
  readonly shares: boolean;
  // Whether it could have changed the width of a rail: a drag that left the
  // dock's own width alone.
  readonly widths: boolean;
  // The blockr option dock_grid_size_tol.
  readonly tolerance: number;
}

// What to run in the view after a gesture, given the grid that dockview
// showed before it, the grid that dockview shows now and the stored grid.
export function gestureRuns(view: ViewId, shown: Grid, read: Grid, stored: Grid, reading: Reading): Run[] {
  const before = withStoredRails(shown, stored);
  const after = withStoredRails(
    withStoredShares(shown, read, reading.shares ? reading.tolerance : Infinity),
    stored,
    reading.widths ? shown : undefined,
  );
  const changes = gridChanges(before, after);
  if (changes.panels || changes.arrangement || changes.sizes) {
    return [{ id: "blockr.dock-set_grid", args: { view, grid: after } }];
  }

  const runs: Run[] = [];
  const activate = (panel: Panel) => {
    runs.push({ id: "blockr.dock-activate_panel", args: { view, panel } });
  };
  const groups = tabGroups(before);
  tabGroups(after).forEach((group, i) => {
    const was = groups[i];
    if (was !== undefined && !samePanel(was.active, group.active)) activate(group.active);
  });
  for (const edge of EDGES) {
    const [was, rail] = [before.rails?.[edge], after.rails?.[edge]];
    if (was === undefined || rail === undefined) continue;
    if (!samePanel(was.active, rail.active)) activate(rail.active);
    if (was.collapsed !== rail.collapsed) {
      runs.push({ id: "blockr.dock-collapse_rail", args: { view, edge, collapsed: rail.collapsed } });
    }
  }
  return runs;
}

// What to run in the view after a gesture in the narrow column, given the
// column that dockview showed before it and the one that it shows now.
export function columnRuns(view: ViewId, shown: Grid, read: Grid): Run[] {
  const held = gridPanels(read);
  const removed = gridPanels(shown).filter((panel) => !held.some((other) => samePanel(other, panel)));
  const showing = tabGroups(shown).map((group) => group.active);
  const shows = tabGroups(read)
    .map((group) => group.active)
    .filter((panel) => !showing.some((other) => samePanel(other, panel)));
  return [
    ...removed.map((panel) => ({ id: "blockr.dock-remove_panel", args: { view, panel } })),
    ...shows.map((panel) => ({ id: "blockr.dock-activate_panel", args: { view, panel } })),
  ];
}

// The grid with the stored widths of each rail that the stored grid holds on
// the same edge.
// Where `moved` gives the grid that dockview showed before a drag, a rail
// whose width the drag changed keeps its new width while it is open.
function withStoredRails(grid: Grid, stored: Grid, moved?: Grid): Grid {
  if (grid.rails === undefined) return grid;
  const rails: Partial<Record<Edge, Rail>> = {};
  for (const edge of EDGES) {
    const rail = grid.rails[edge];
    if (rail === undefined) continue;
    const kept = stored.rails?.[edge];
    const was = moved?.rails?.[edge];
    const dragged = was !== undefined && was.size !== rail.size;
    rails[edge] = kept === undefined ? rail : { ...rail, size: dragged ? rail.size : kept.size, collapsed_size: kept.collapsed_size };
  }
  return { ...grid, rails };
}
