// How the dock draws the active view with dockview, brings dockview to the
// stored grid as the grid changes, and writes what a gesture changes into the
// dock's state through the dock's commands, so that nothing mirrors the
// layout in R
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.1#drawing-a-view).
//
// Drawing:
// - The dock draws the active view's grid with gridToLayout() and dockview's
//   fromJSON(), as dockview's layout.
// - It passes fromJSON() `reuseExistingPanels`, under which dockview moves each
//   panel that stays with moveGroupOrPanel(), so that what core mounted in it
//   stays mounted, and disposes only the panels that leave.
// - In a window narrower than the blockr option narrow_breakpoint, it draws
//   the narrow column of ../../layout in place of the grid, as tall as its
//   rows, with dragging and resizing off.
// - Under the board lock, it passes dockview `locked` while the blockr option
//   lock_resize is on, and `disableDnd` while lock_rearrange is on.
//
// Following the board:
// - The dock draws again where the active view changes, or where the window
//   crosses the breakpoint.
// - Where the active view's grid changes otherwise, through an undo or
//   another origin's command, it compares the grid with what dockview shows.
// - It draws again where the panels, the arrangement or a share differ, as
//   dockview's shares may differ from the stored ones by
//   dock_grid_size_tol.
// - Otherwise it has dockview show the panels and collapse or open the rails
//   that the grid says, and renames the tabs whose block was renamed.
//
// Reading back:
// - After each change of dockview's layout, the dock reads the layout back
//   with layoutToGrid() and runs what ../../layout's gestureRuns() gives, or
//   columnRuns() in the narrow column.
// - While a pointer is down in the dock, as while the user drags a sash, it
//   waits, so that a gesture commits once, when the user lets go.
// - Its own drawing changes nothing, since it compares with what dockview
//   showed, which it reads again after each of its own changes.
// - While a command that a gesture sent has not yet applied, the dock leaves
//   dockview as the user left it, and once every such command has applied or
//   been refused, it brings dockview to the stored grid.
// - A refused command raises a notification of the code edit_refused, which
//   concerns the dock plugin, as core's plugins raise one for a refused edit.
//
// Rails:
// - Dockview always holds the edge groups of both rails, and the dock shows
//   one only while it holds panels.
// - During a drag toward an edge, the dock reveals the hidden rail there,
//   collapsed, after a dwell, so that a tab can be dropped onto it, and opens
//   it once a panel lands in it, as today's dock-rail.js does, at 655e323.
// - Dockview narrows a rail that a narrow dock cannot hold, and leaves it
//   narrow once the dock has room again, so the dock gives each rail its
//   stored width back whenever the dock's own size changes.
//   It follows the dock's root with a ResizeObserver, and waits a frame, so
//   that dockview, which follows the same element, has laid the dock out
//   first.

import type { CoreApi, PluginClassId } from "@blockr/core";
import type { DockviewApi, DockviewIDisposable } from "dockview-react";
import { stateOf } from "../../commands/index.ts";
import { columnRuns, gestureRuns, gridToLayout, layoutToGrid, panelId, panelOf, railId, stackGrid } from "../../layout/index.ts";
import type { Run, Size } from "../../layout/index.ts";
import { EDGES, gridChanges, RAIL_COLLAPSED_SIZE, samePanel, tabGroups, withStoredShares } from "../../state/index.ts";
import type { Edge, Grid, Panel, ViewId } from "../../state/index.ts";
import type { Narrow } from "./narrow.ts";
import type { Settings } from "./settings.ts";

export interface DrawingOptions {
  readonly core: CoreApi;
  // The id of the dock plugin's class, which raises the dock's notifications.
  readonly plugin: PluginClassId;
  readonly api: DockviewApi;
  // The dock's root, which holds dockview, and in which gestures start.
  readonly el: HTMLElement;
  readonly settings: Settings;
  readonly narrow: Narrow;
  // The app's plugins whose class has a `ui`, each with its name, whose panels
  // the dock draws.
  readonly drawn: ReadonlyMap<PluginClassId, string>;
}

// The grid of a view that shows nothing, which the dock draws where the state
// has no active view.
const EMPTY: Grid = { orientation: "horizontal", children: [], sizes: [] };

// How long a drag stays in an edge's band before the hidden rail there shows,
// so that a drag that sweeps past an edge does not flash the rail open.
const DWELL_MS = 250;

// Draws the active view in dockview, and follows the board, until the
// returned function runs.
export function drawDock(options: DrawingOptions): () => void {
  const { core, plugin, api, el, settings, narrow, drawn } = options;
  const doc = el.ownerDocument;
  const win = doc.defaultView ?? window;

  // The view that dockview draws, null for none, or undefined before the
  // first draw, and whether it draws the narrow column.
  let view: ViewId | null | undefined;
  let column = narrow.get();
  // The grid that dockview showed at its last change, as layoutToGrid() reads
  // it.
  let shown: Grid | undefined;
  // How many commands that gestures sent have not yet applied or been
  // refused.
  let pending = 0;
  // The pointer that is down in the dock, with the dock's width as it went
  // down.
  let press: { readonly width: number } | undefined;
  // Whether dockview had laid the dock out at a size as the dock last
  // checked.
  let laidOut = api.width > 0 && api.height > 0;
  // Whether a tab is being dragged, the hidden rails that the drag revealed,
  // and the edge whose band the drag dwells in.
  let dragging = false;
  const revealed = new Set<Edge>();
  let dwell: { readonly edge: Edge; readonly timer: ReturnType<typeof setTimeout> } | undefined;
  let stopped = false;

  // The active view, with its stored grid.
  const active = (): { readonly id: ViewId; readonly grid: Grid } | undefined => {
    const state = stateOf(core.store.board());
    const id = state?.active ?? null;
    const held = id === null ? undefined : state?.views[id];
    return id === null || held === undefined ? undefined : { id, grid: held.grid };
  };

  // The title of a panel's tab, or undefined where the dock cannot draw it.
  const title = (panel: Panel): string | undefined =>
    "block" in panel ? core.store.board().blocks[panel.block]?.name : drawn.get(panel.plugin);

  const size = (): Size => ({ width: api.width, height: api.height });
  const read = (): Grid => layoutToGrid(api.toJSON());

  // What dockview draws of a stored grid: the grid, or its narrow column.
  const target = (grid: Grid): Grid => (column ? stackGrid(grid, settings.fraction).grid : grid);

  function draw(): void {
    const current = active();
    view = current?.id ?? null;
    const was = column;
    column = narrow.get();
    if (was !== column) lockDockview();
    const grid = current?.grid ?? EMPTY;
    el.dataset.layout = column ? "narrow" : "wide";
    el.style.height = column ? `${String(Math.round(stackGrid(grid, settings.fraction).height * 1e4) / 1e2)}vh` : "";
    api.fromJSON(gridToLayout(target(grid), title, size()), { reuseExistingPanels: true });
    syncRails();
    shown = read();
  }

  // Tells dockview what to lock: under the board lock, the sizes and the
  // arrangement, as the blockr options say, and in the narrow column both.
  function lockDockview(): void {
    const locked = core.lock.board;
    api.updateOptions({
      locked: column || (locked && settings.lockResize),
      disableDnd: column || (locked && settings.lockRearrange),
    });
  }

  // Brings dockview to the stored grid.
  function reconcile(): void {
    if (stopped || pending > 0) return;
    const current = active();
    if (view === undefined || (current?.id ?? null) !== view || narrow.get() !== column) {
      draw();
      return;
    }
    if (current === undefined) return;

    const want = layoutToGrid(gridToLayout(target(current.grid), title, size()));
    const now = read();
    const changes = gridChanges(want, now);
    const sized = api.width > 0 && api.height > 0;
    if (changes.panels || changes.arrangement || (sized && !sharesNear(want, now, settings.tolerance))) {
      draw();
      return;
    }

    const wantedGroups = tabGroups(want);
    tabGroups(now).forEach((group, i) => {
      const wanted = wantedGroups[i];
      if (wanted !== undefined && !samePanel(wanted.active, group.active)) showPanel(wanted.active);
    });
    for (const edge of EDGES) {
      const [wanted, rail] = [want.rails?.[edge], now.rails?.[edge]];
      const group = api.getEdgeGroup(edge);
      if (wanted === undefined || rail === undefined || group === undefined) continue;
      if (!samePanel(wanted.active, rail.active)) showPanel(wanted.active);
      if (wanted.collapsed !== rail.collapsed) {
        if (wanted.collapsed) group.collapse();
        else group.expand();
      }
      if (wanted.size !== rail.size) group.setSize({ width: wanted.size });
    }
    retitle();
    shown = read();
  }

  function showPanel(panel: Panel): void {
    api.getPanel(panelId(panel))?.api.setActive();
  }

  // Gives each tab the name of its block, which a rename changes.
  function retitle(): void {
    for (const held of api.panels) {
      const panel = panelOf(held.id);
      const name = panel === undefined ? undefined : title(panel);
      if (name !== undefined && held.api.title !== name) held.api.setTitle(name);
    }
  }

  // The dock's own width, which a drag of a rail's sash leaves alone.
  const dockWidth = () => el.getBoundingClientRect().width;

  // Reads dockview's layout back after a gesture, and runs what it changed.
  // A pointer gesture is one that started with a pointer down in the dock.
  function settle(pointer?: { readonly width: number }): void {
    if (stopped) return;
    syncRails();
    const current = active();
    const before = shown;
    if (before === undefined || current === undefined || current.id !== view) return;
    const now = read();
    shown = now;
    const runs = column
      ? columnRuns(current.id, before, now)
      : gestureRuns(current.id, before, now, current.grid, {
          shares: pointer !== undefined,
          widths: pointer?.width === dockWidth(),
          tolerance: settings.tolerance,
        });
    for (const run of runs) send(run);
  }

  function send(run: Run): void {
    pending += 1;
    void core.commands
      .run(run.id, run.args)
      .catch((error: unknown) => {
        if (stopped) return;
        core.notifications.notify(plugin, {
          severity: "error",
          code: "edit_refused",
          message: error instanceof Error ? error.message : String(error),
        });
      })
      .finally(() => {
        pending -= 1;
        reconcile();
      });
  }

  // Shows each rail that holds panels or that a drag revealed, and hides the
  // others.
  // A rail that a drag revealed opens once a panel lands in it.
  function syncRails(): void {
    for (const edge of EDGES) {
      const group = api.getGroup(railId(edge));
      if (group === undefined) continue;
      const holds = group.panels.length > 0;
      if (holds && revealed.has(edge) && group.api.isCollapsed()) group.api.expand();
      if (!dragging) revealed.delete(edge);
      const wanted = !column && (holds || revealed.has(edge));
      if (api.isEdgeGroupVisible(edge) !== wanted) api.setEdgeGroupVisible(edge, wanted);
    }
  }

  // Gives each open rail of the active view its stored width back.
  function restoreRails(): void {
    if (column) return;
    const current = active();
    const sizes = api.toJSON().edgeGroups;
    for (const edge of EDGES) {
      const rail = current?.grid.rails?.[edge];
      const group = api.getEdgeGroup(edge);
      const now = sizes?.[edge]?.size;
      if (rail === undefined || group === undefined || rail.collapsed || now === undefined) continue;
      if (Math.round(now) !== rail.size) group.setSize({ width: rail.size });
    }
  }

  // The band of an edge in which a drag reveals the hidden rail there: the
  // width of the rail's collapsed strip, as the active view stores it or 35
  // pixels, so that what the user aims at and what appears are about the same
  // band.
  function inBand(edge: Edge, event: DragEvent): boolean {
    const rect = el.getBoundingClientRect();
    const { clientX: x, clientY: y } = event;
    if (x < rect.left || x > rect.right || y < rect.top || y > rect.bottom) return false;
    const band = active()?.grid.rails?.[edge]?.collapsed_size ?? RAIL_COLLAPSED_SIZE;
    return edge === "left" ? x - rect.left <= band : rect.right - x <= band;
  }

  function clearDwell(): void {
    if (dwell !== undefined) clearTimeout(dwell.timer);
    dwell = undefined;
  }

  function reveal(edge: Edge): void {
    if (api.isEdgeGroupVisible(edge)) return;
    revealed.add(edge);
    api.setEdgeGroupVisible(edge, true);
    const group = api.getEdgeGroup(edge);
    if (group !== undefined && !group.isCollapsed()) group.collapse();
  }

  const onDragOver = (event: DragEvent) => {
    if (!dragging || column) return;
    const edge = EDGES.find((candidate) => inBand(candidate, event));
    if (edge === undefined) {
      clearDwell();
      return;
    }
    if (dwell?.edge === edge) return;
    clearDwell();
    dwell = {
      edge,
      timer: setTimeout(() => {
        dwell = undefined;
        if (dragging) reveal(edge);
      }, DWELL_MS),
    };
  };

  // A drag that ends, by a drop or otherwise, hides each revealed rail that
  // no panel landed in.
  const endDrag = () => {
    if (!dragging) return;
    dragging = false;
    clearDwell();
    syncRails();
  };
  const onKey = (event: KeyboardEvent) => {
    if (event.key === "Escape") endDrag();
  };

  const onPointerDown = () => {
    press = { width: dockWidth() };
  };
  const onPointerUp = () => {
    const pressed = press;
    if (pressed === undefined) return;
    press = undefined;
    settle(pressed);
  };

  // The dock's size changed, as the window's did.
  // A dock that dockview lays out for the first time, as one that was hidden,
  // compares its shares with the stored ones only now.
  const onResize = () => {
    if (stopped) return;
    if (press === undefined) restoreRails();
    const was = laidOut;
    laidOut = api.width > 0 && api.height > 0;
    if (!was && laidOut) reconcile();
  };
  const resizes =
    typeof win.ResizeObserver === "function"
      ? new win.ResizeObserver(() => {
          win.requestAnimationFrame(onResize);
        })
      : undefined;

  // A panel that lands resolves the drag, whether or not dragend reaches the
  // document, since dockview detaches the dragged tab as the panel moves.
  const landed = () => {
    dragging = false;
    clearDwell();
  };

  const disposables: DockviewIDisposable[] = [
    api.onDidLayoutChange(() => {
      if (press === undefined) settle();
    }),
    api.onWillDragPanel(() => {
      dragging = true;
    }),
    api.onWillDragGroup(() => {
      dragging = true;
    }),
    api.onDidMovePanel(landed),
    api.onDidAddPanel(landed),
    api.onDidRemovePanel(landed),
  ];

  resizes?.observe(el);
  el.addEventListener("pointerdown", onPointerDown, true);
  win.addEventListener("pointerup", onPointerUp, true);
  win.addEventListener("pointercancel", onPointerUp, true);
  el.addEventListener("dragover", onDragOver);
  doc.addEventListener("drop", endDrag);
  doc.addEventListener("dragend", endDrag);
  doc.addEventListener("keydown", onKey);
  // A change that leaves the dock's state as it was, as a block's state that
  // the user edits, can change a tab's name alone.
  let dockState = core.store.board().plugins[plugin];
  const offBoard = core.store.subscribe((_change, board) => {
    if (board.plugins[plugin] === dockState) {
      retitle();
      return;
    }
    dockState = board.plugins[plugin];
    reconcile();
  });
  const offNarrow = narrow.subscribe(() => {
    reconcile();
  });

  lockDockview();
  draw();

  // A click on a rail's tab collapses or opens the rail as the click ends,
  // after the pointer went up.
  // The first draw adds the rails' edge groups, which dockview keeps from then
  // on.
  for (const edge of EDGES) {
    const group = api.getEdgeGroup(edge);
    if (group === undefined) continue;
    disposables.push(
      group.onDidCollapsedChange(() => {
        queueMicrotask(() => {
          if (press === undefined) settle();
        });
      }),
    );
  }

  return () => {
    stopped = true;
    clearDwell();
    resizes?.disconnect();
    offBoard();
    offNarrow();
    for (const disposable of disposables) disposable.dispose();
    el.removeEventListener("pointerdown", onPointerDown, true);
    win.removeEventListener("pointerup", onPointerUp, true);
    win.removeEventListener("pointercancel", onPointerUp, true);
    el.removeEventListener("dragover", onDragOver);
    doc.removeEventListener("drop", endDrag);
    doc.removeEventListener("dragend", endDrag);
    doc.removeEventListener("keydown", onKey);
  };
}

// Whether every share of a grid is within `tolerance` of the same share of
// another grid of the same shape, as withStoredShares() keeps every share of
// the one where it is.
// The rails are left out, since their widths are no shares.
function sharesNear(a: Grid, b: Grid, tolerance: number): boolean {
  const bare = (grid: Grid): Grid => ({ orientation: grid.orientation, children: grid.children, sizes: grid.sizes });
  return !gridChanges(bare(a), bare(withStoredShares(a, b, tolerance))).sizes;
}
