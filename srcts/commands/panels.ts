// The dock's commands for the panels of its views and their rails
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.1#commands).
// Each replaces a view's grid whole, so that undo puts back the grid that was
// there, where a patch inside it would name an index that may hold something
// else by then.
// A panel goes where `near`, `side` and `rail` say, as today's
// hint_to_position() places one, or at its default place, and the grid that a
// command writes is in its canonical form.
// A new tab group beside another gets the share that dockview gives a group
// it adds or moves there: adding one evens out the split, and moving one
// halves the tab group that it goes beside.
// Setting a view's grid keeps the stored shares of each split that the grid
// moves by no more than the blockr option dock_grid_size_tol, as the grid
// that the dock reads back from dockview moves each share a little.
// Showing another tab and opening or collapsing a rail take no undo step,
// since they change only what the user looks at.
//
// On a locked board, a reader still has a tab group show another panel and
// opens or collapses a rail, while no command adds or removes a panel.
// Moving or resizing one, and setting a view's grid, are left to readers only
// where a deployment sets the blockr options lock_rearrange or lock_resize to
// FALSE.
// Blockr options come as a promise, so these commands read them in their
// prepare(), and refuse in their run().

import type { Board } from "@blockr/core";
import {
  collapsedRail,
  gridChanges,
  locate,
  placedAt,
  placePanel,
  places,
  prunePanels,
  resized,
  samePanel,
  showing,
  withStoredShares,
} from "../state/index.ts";
import type { DockState, Edge, Grid, GridChanges, Panel, Place, View, ViewId } from "../state/index.ts";
import { at, command, givenGridFailures, gridSizeTolerance, holdsBlock, layoutLocks, refuse, stateOf } from "./common.ts";
import type { LayoutLocks } from "./common.ts";
import * as schemas from "./schemas.ts";

interface PanelArgs {
  readonly view: ViewId;
  readonly panel: Panel;
}

type Placed = PanelArgs & Place;

interface Resize extends PanelArgs {
  readonly size: number;
}

interface SetGrid {
  readonly view: ViewId;
  readonly grid: Grid;
}

interface CollapseRail {
  readonly view: ViewId;
  readonly edge: Edge;
  readonly collapsed: boolean;
}

// The view that the board's dock state holds under the id, or undefined.
function viewOf(board: Board, id: ViewId): View | undefined {
  return stateOf(board)?.views[id];
}

// Whether the view holds the panel, in a tab group or on a rail.
function shows(board: Board, view: ViewId, panel: Panel): boolean {
  const held = viewOf(board, view);
  return held !== undefined && places(held.grid, panel);
}

// The patch that replaces a view's grid whole.
function gridPatch(view: ViewId, grid: Grid) {
  return { op: "replace" as const, path: at("views", view, "grid"), value: grid };
}

// The grid without the panel.
function withoutPanel(grid: Grid, panel: Panel): Grid {
  return prunePanels(grid, (placed) => !samePanel(placed, panel));
}

// The args of a command that places a panel.
function placeArgs(what: string) {
  return schemas.object(
    {
      view: schemas.view,
      panel: schemas.panel,
      near: schemas.near(what),
      side: schemas.side,
      rail: schemas.rail,
    },
    ["view", "panel"],
  );
}

// Why a locked board keeps a view's grid as it is, in the parts that it
// keeps.
const KEPT = {
  panels: "The board is locked, which keeps the panels that each view shows.",
  arrangement: "The board is locked, and the blockr option lock_rearrange keeps where each panel sits.",
  sizes: "The board is locked, and the blockr option lock_resize keeps the sizes of the splits and rails.",
};

// Refuses where a locked board keeps a part that a command changes.
function keep(changes: Partial<GridChanges>, locks: LayoutLocks, locked: boolean): void {
  if (!locked) return;
  refuse([
    ...(changes.panels === true ? [KEPT.panels] : []),
    ...(changes.arrangement === true && locks.rearrange ? [KEPT.arrangement] : []),
    ...(changes.sizes === true && locks.resize ? [KEPT.sizes] : []),
  ]);
}

export const panelCommands = [
  command<Placed>({
    id: "blockr.dock-add_panel",
    label: "Add panel",
    description:
      "Places a panel in a view that does not show it, as the tab that its tab group or rail shows: in or beside the tab group of the panel that `near` names, on the side that `side` names, on the rail that `rail` names, or at the default place, a new tab in the last tab group that holds no plugin's panel.",
    args: placeArgs("goes"),
    enabled(board, { view, panel }, { lock }) {
      return !lock.board && viewOf(board, view) !== undefined && !shows(board, view, panel) && holdsBlock(board, panel);
    },
    run(board, { view, panel, near, side, rail }) {
      const held = viewOf(board, view);
      if (held === undefined) return [];
      return { patches: [gridPatch(view, placedAt(held.grid, panel, { near, side, rail }, "distribute"))] };
    },
  }),

  command<PanelArgs>({
    id: "blockr.dock-remove_panel",
    label: "Remove panel",
    description:
      "Takes a panel out of a view, as closing its tab does. It removes no block, and the panel stays in the other views that show it.",
    args: schemas.object({ view: schemas.view, panel: schemas.panel }, ["view", "panel"]),
    enabled(board, { view, panel }, { lock }) {
      return !lock.board && shows(board, view, panel);
    },
    run(board, { view, panel }) {
      const held = viewOf(board, view);
      if (held === undefined) return [];
      return { patches: [gridPatch(view, withoutPanel(held.grid, panel))] };
    },
  }),

  command<Placed, Placed & { readonly locks: LayoutLocks }>({
    id: "blockr.dock-move_panel",
    label: "Move panel",
    description:
      "Moves a panel within a view that shows it, placed as blockr.dock-add_panel places one: in or beside the tab group of the panel that `near` names, on the side that `side` names, on the rail that `rail` names, or at the default place.",
    args: placeArgs("moves"),
    enabled(board, { view, panel, near }) {
      return shows(board, view, panel) && (near === undefined || !samePanel(near, panel));
    },
    async prepare(_board, args, context) {
      return { ...args, locks: await layoutLocks(context) };
    },
    run(board, { view, panel, near, side, rail, locks }, { lock }) {
      keep({ arrangement: true }, locks, lock.board);
      const held = viewOf(board, view);
      if (held === undefined) return [];
      // A panel alone on the rail that it moves to stays there, and the rail
      // keeps its widths, which it would forget once it held no panel.
      const from = locate(held.grid, panel);
      if (rail !== undefined && from !== undefined && "edge" in from && from.edge === rail) {
        if (held.grid.rails?.[rail]?.panels.length === 1) return [];
      }
      return { patches: [gridPatch(view, placedAt(withoutPanel(held.grid, panel), panel, { near, side, rail }, "split"))] };
    },
  }),

  command<Resize, Resize & { readonly locks: LayoutLocks }>({
    id: "blockr.dock-resize_panel",
    label: "Resize panel",
    description:
      "Sets the size of the tab group that holds a panel: its share of the split that holds it, a ratio between 0 and 1, while the other children of the split share the rest in proportion. For a panel on a rail, it sets the rail's width in pixels while it is open.",
    args: schemas.object(
      {
        view: schemas.view,
        panel: schemas.panel,
        size: {
          type: "number",
          exclusiveMinimum: 0,
          description:
            "The share of its split that the tab group takes, above 0 and below 1, or the width in pixels of the rail.",
        },
      },
      ["view", "panel", "size"],
    ),
    enabled(board, { view, panel }) {
      return shows(board, view, panel);
    },
    async prepare(_board, args, context) {
      return { ...args, locks: await layoutLocks(context) };
    },
    run(board, { view, panel, size, locks }, { lock }) {
      keep({ sizes: true }, locks, lock.board);
      const held = viewOf(board, view);
      if (held === undefined) return [];
      return { patches: [gridPatch(view, resized(held.grid, panel, size))] };
    },
  }),

  command<PanelArgs>({
    id: "blockr.dock-activate_panel",
    label: "Activate panel",
    description:
      "Has the tab group or rail that holds a panel show it, as clicking its tab does. It takes no undo step, since it changes only what the user looks at.",
    args: schemas.object({ view: schemas.view, panel: schemas.panel }, ["view", "panel"]),
    undo: false,
    enabled(board, { view, panel }) {
      return shows(board, view, panel);
    },
    run(board, { view, panel }) {
      const held = viewOf(board, view);
      if (held === undefined) return [];
      return { patches: [gridPatch(view, showing(held.grid, panel))] };
    },
  }),

  command<{ readonly panel: Panel }>({
    id: "blockr.dock-show_panel",
    label: "Show panel",
    description:
      "Shows a panel in the active view: has the tab group or rail that holds it show it, where the view shows it, and places it at the default place where the view does not, a new tab in the last tab group that holds no plugin's panel. On a locked board, it only has a tab group or rail show a panel that the view shows.",
    args: schemas.object({ panel: schemas.panel }, ["panel"]),
    enabled(board, { panel }, { lock }) {
      const active = stateOf(board)?.active ?? null;
      if (active === null || !holdsBlock(board, panel)) return false;
      return !lock.board || shows(board, active, panel);
    },
    run(board, { panel }) {
      const state = stateOf(board);
      const active = state?.active ?? null;
      const held = active === null ? undefined : state?.views[active];
      if (active === null || held === undefined) return [];
      const grid = places(held.grid, panel) ? showing(held.grid, panel) : placePanel(held.grid, panel);
      return { patches: [gridPatch(active, grid)] };
    },
  }),

  command<SetGrid, SetGrid & { readonly locks: LayoutLocks; readonly tolerance: number }>({
    id: "blockr.dock-set_grid",
    label: "Set grid",
    description:
      "Sets a view's whole grid, which says which panels the view shows and where, as the dock does after the user drags a panel or a border. A grid places each panel once, and the shares of each split are above 0 and sum to 1. Where the grid has the same splits and tab groups as the view's, a split keeps its shares unless the grid moves one by more than the blockr option dock_grid_size_tol, 0.005 unless set.",
    args: schemas.object({ view: schemas.view, grid: schemas.grid }, ["view", "grid"]),
    enabled(board, { view }) {
      return viewOf(board, view) !== undefined;
    },
    async prepare(_board, args, context) {
      const [locks, tolerance] = await Promise.all([layoutLocks(context), gridSizeTolerance(context)]);
      return { ...args, locks, tolerance };
    },
    run(board, { view, grid, locks, tolerance }, { lock }) {
      const held = viewOf(board, view);
      if (held === undefined) return [];
      refuse(givenGridFailures(board, grid));
      const kept = withStoredShares(held.grid, grid, tolerance);
      keep(gridChanges(held.grid, kept), locks, lock.board);
      return { patches: [gridPatch(view, kept)] };
    },
  }),

  command<CollapseRail>({
    id: "blockr.dock-collapse_rail",
    label: "Collapse rail",
    description:
      "Collapses the rail on an edge of a view, which then shows its tabs alone, or opens it. While the board option sync_rails is on, it does so on every view that has a rail on that edge. It takes no undo step, since it changes only what the user looks at.",
    args: schemas.object(
      {
        view: schemas.view,
        edge: schemas.edge,
        collapsed: { type: "boolean", description: "Whether the rail is collapsed, or open." },
      },
      ["view", "edge", "collapsed"],
    ),
    undo: false,
    enabled(board, { view, edge }) {
      return viewOf(board, view)?.grid.rails?.[edge] !== undefined;
    },
    run(board, { view, edge, collapsed }) {
      const state = stateOf(board);
      if (state === undefined) return [];
      const synced = board.options.sync_rails !== false;
      return { patches: railViews(state, synced ? undefined : view, edge).map(([id, grid]) => gridPatch(id, collapsedRail(grid, edge, collapsed))) };
    },
  }),
];

// The views whose rail on the edge a command collapses or opens, with their
// grids: the one given, or every view that has a rail there.
function railViews(state: DockState, view: ViewId | undefined, edge: Edge): [ViewId, Grid][] {
  return Object.entries(state.views).flatMap(([id, held]): [ViewId, Grid][] =>
    (view === undefined || id === view) && held.grid.rails?.[edge] !== undefined ? [[id, held.grid]] : [],
  );
}
