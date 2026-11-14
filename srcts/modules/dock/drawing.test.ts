// How the dock draws the active view with dockview, follows the board and
// reads a gesture back, with drawDock() on dockview itself, which happy-dom
// lays out at the size that a test gives, and on blockr.core's CoreApi for
// tests.
// A test plays a gesture through dockview's api, as a drop or a click ends in
// one, with a pointer down in the dock where the gesture is a drag.

import type { Json, JsonBoard } from "@blockr/core";
import { HELLO, stubCore } from "@blockr/core/stubs";
import { createDockview } from "dockview-react";
import type { DockviewApi, IContentRenderer } from "dockview-react";
import { describe, expect, onTestFinished, test, vi } from "vitest";
import { layoutToGrid, panelId, stackGrid } from "../../layout/index.ts";
import type { DockState, Grid, Panel, Rail, TabGroup } from "../../state/index.ts";
import * as compactOption from "../compact-option/index.ts";
import * as syncRailsOption from "../sync-rails-option/index.ts";
import * as viewTabsOption from "../view-tabs-option/index.ts";
import { drawDock } from "./drawing.ts";
import * as dock from "./index.tsx";
import { trackNarrow } from "./narrow.ts";
import { readSettings } from "./settings.ts";
import { drawnPlugins } from "./start.ts";

// Resizes happy-dom's window, which tells its listeners as a browser does.
function setWindowWidth(width: number): void {
  (window as unknown as { readonly happyDOM: { setInnerWidth(width: number): void } }).happyDOM.setInnerWidth(width);
}

// A ResizeObserver that a test tells of a resize of an element, since
// happy-dom lays nothing out.
// Dockview observes elements of its own, which it lays out as the test calls
// its layout() instead.
class Resizes {
  static readonly observers = new Set<Resizes>();
  private readonly targets = new Set<Element>();
  private readonly fn: () => void;
  constructor(fn: () => void) {
    this.fn = fn;
    Resizes.observers.add(this);
  }
  observe(target: Element): void {
    this.targets.add(target);
  }
  unobserve(target: Element): void {
    this.targets.delete(target);
  }
  disconnect(): void {
    this.targets.clear();
    Resizes.observers.delete(this);
  }
  // Tells each observer of the element of a resize, and waits for the frame
  // in which the dock acts on it.
  static async resized(target: Element): Promise<void> {
    for (const observer of [...Resizes.observers]) {
      if (observer.targets.has(target)) observer.fn();
    }
    await new Promise<void>((resolve) => {
      requestAnimationFrame(() => {
        resolve();
      });
    });
  }
}

// Whether a grid that dockview shows is the grid given, with each share
// within the tolerance that dockview's whole pixels leave.
function expectShows(shown: Grid, grid: Grid): void {
  expect({ ...shown, sizes: [] }).toEqual({ ...grid, sizes: [] });
  grid.sizes.forEach((size, i) => {
    expect(shown.sizes[i]).toBeCloseTo(size, 2);
  });
}

const DOCK = "blockr.dock-dock_plugin";

const a = { block: "a" };
const b = { block: "b" };
const c = { block: "c" };
const edit = { plugin: "blockr.core-edit_board_plugin" };

function group(...panels: Panel[]): TabGroup {
  const [first] = panels;
  if (first === undefined) throw new Error("A tab group holds a panel.");
  return { panels, active: first };
}

function rail(...panels: Panel[]): Rail {
  return { ...group(...panels), collapsed: false, size: 260, collapsed_size: 35 };
}

// The view Main shows a tab group of a and b, which shows b, beside c, with
// core's Edit board plugin on a rail at its left edge, and the view Side
// shows c, with the plugin on a rail too.
const main: Grid = {
  orientation: "horizontal",
  children: [{ panels: [a, b], active: b }, group(c)],
  sizes: [0.4, 0.6],
  rails: { left: rail(edit) },
};

const side: Grid = { orientation: "horizontal", children: [group(c)], sizes: [1], rails: { left: rail(edit) } };

const state: DockState = {
  views: {
    main: { name: "Main", folder: null, index: "a0", grid: main },
    side: { name: "Side", folder: null, index: "a1", grid: side },
  },
  folders: {},
  active: "main",
  visible: {},
};

// R's reply to hello in an app that runs the dock in place of core's basic
// frontend, with the dock's board options, on a board with three blocks and
// the dock's state given, under the board lock where `locked` says so.
function helloWith(dockState: DockState, locked: boolean) {
  const board: JsonBoard = {
    format: 1,
    packages: { "blockr.core": "0.2.0", "blockr.dock": "0.2.1" },
    board: {
      id: "curious_otter",
      versions: { [DOCK]: 1 },
      name: "Curious otter",
      blocks: {
        a: { registry_id: "blockr.core-dataset_block", version: 1, state: { dataset: "iris", package: "datasets" }, name: "Data", locked: null },
        b: { registry_id: "blockr.core-head_block", version: 1, state: { n: 3, direction: "head" }, name: "Head", locked: null },
        c: { registry_id: "blockr.core-head_block", version: 1, state: { n: 5, direction: "tail" }, name: "Tail", locked: null },
      },
      links: { ab: { from: "a", to: "b", input: "data" }, ac: { from: "a", to: "c", input: "data" } },
      stacks: {},
      options: {},
      plugins: { [DOCK]: dockState },
    },
  };
  return {
    ...HELLO,
    board,
    lock: { board: locked, load: false },
    packages: { ...HELLO.packages, "blockr.dock": "0.2.1" },
    plugins: {
      ...Object.fromEntries(Object.entries(HELLO.plugins).filter(([id]) => id !== "blockr.core-basic_frontend_plugin")),
      [DOCK]: { name: "Dock", version: 1, module: "blockr.dock/js/dock.js" },
    },
    options: {
      ...HELLO.options,
      compact: { package: "blockr.dock", module: "blockr.dock/compact-option.js", version: 1 },
      view_tabs: { package: "blockr.dock", module: "blockr.dock/view-tabs-option.js", version: 1 },
      sync_rails: { package: "blockr.dock", module: "blockr.dock/sync-rails-option.js", version: 1 },
    },
  };
}

const modules = { [DOCK]: dock, compact: compactOption, view_tabs: viewTabsOption, sync_rails: syncRailsOption };

interface StartOptions {
  readonly dockState?: DockState;
  readonly locked?: boolean;
  readonly blockrOptions?: Readonly<Record<string, Json>>;
  // The breakpoint of the narrow column, none unless given.
  readonly breakpoint?: number;
}

// Starts core on a board that holds the dock's state given, the one above
// unless given, and draws its active view with dockview in an element of its
// own, laid out at 1000 by 800 pixels.
async function start(options: StartOptions = {}) {
  const { core, settled, stop } = await stubCore({
    hello: helloWith(options.dockState ?? state, options.locked ?? false),
    modules,
    blockrOptions: options.blockrOptions ?? {},
  });
  onTestFinished(stop);
  await settled();

  const el = document.createElement("div");
  document.body.append(el);
  const made: string[] = [];
  const disposed: string[] = [];
  const api: DockviewApi = createDockview(el, {
    createComponent: (component): IContentRenderer => {
      made.push(component.id);
      return {
        element: document.createElement("div"),
        init: () => undefined,
        dispose: () => {
          disposed.push(component.id);
        },
      };
    },
  });
  api.layout(1000, 800, true);
  const updateOptions = vi.spyOn(api, "updateOptions");

  const settings = await readSettings(core);
  const narrow = trackNarrow(window, options.breakpoint);
  const stopDrawing = drawDock({ core, plugin: DOCK, api, el, settings, narrow, drawn: drawnPlugins(core) });
  onTestFinished(() => {
    stopDrawing();
    narrow.stop();
    api.dispose();
    el.remove();
  });

  // Waits for the microtask in which dockview tells of a change to its
  // layout, and for every command that it runs.
  const after = async () => {
    await Promise.resolve();
    await settled();
  };
  return {
    core,
    api,
    el,
    made,
    disposed,
    updateOptions,
    after,
    dock: () => core.store.board().plugins[DOCK] as DockState,
    grid: (view = "main") => (core.store.board().plugins[DOCK] as DockState).views[view]?.grid,
    shown: () => layoutToGrid(api.toJSON()),
    panel: (panel: Panel) => {
      const held = api.getPanel(panelId(panel));
      if (held === undefined) throw new Error(`Dockview holds no panel ${panelId(panel)}.`);
      return held;
    },
    // Plays a drag: a pointer goes down in the dock, the gesture changes the
    // layout, and the pointer goes up.
    drag: async (gesture: () => void) => {
      el.dispatchEvent(new PointerEvent("pointerdown", { bubbles: true }));
      gesture();
      await Promise.resolve();
      window.dispatchEvent(new PointerEvent("pointerup"));
      await settled();
    },
  };
}

describe("drawing a view", () => {
  test("draws the active view's grid with dockview, each tab named after its block or plugin", async () => {
    const s = await start();

    expect(s.shown()).toEqual(main);
    expect(s.panel(b).title).toBe("Head");
    expect(s.panel(edit).title).toBe("Edit board");
    expect(s.api.isEdgeGroupVisible("left")).toBe(true);
    expect(s.api.isEdgeGroupVisible("right")).toBe(false);
    expect(s.core.store.canUndo()).toBe(false);
  });

  test("draws the view that becomes active, keeping the panel of a block that both views show", async () => {
    const s = await start();
    const panel = s.panel(c);

    await s.core.commands.run("blockr.dock-activate_view", { view: "side" });
    await s.after();

    expect(s.shown()).toEqual(side);
    expect(s.panel(c)).toBe(panel);
    expect(s.disposed).toEqual([a, b].map(panelId));
  });

  test("leaves out a panel that it cannot draw, and writes nothing back for it", async () => {
    const dag = { plugin: "blockr.dag-dag_plugin" };
    const grid: Grid = { ...main, rails: { left: rail(edit), right: rail(dag) } };
    const s = await start({ dockState: { ...state, views: { ...state.views, main: { name: "Main", folder: null, index: "a0", grid } } } });

    expect(s.shown()).toEqual(main);
    expect(s.grid()?.rails?.right).toEqual(rail(dag));
  });
});

describe("a gesture", () => {
  test("that shows another tab runs blockr.dock-activate_panel, which takes no undo step", async () => {
    const s = await start();

    s.panel(a).api.setActive();
    await s.after();

    expect(s.grid()).toEqual({ ...main, children: [group(a, b), group(c)] });
    expect(s.core.store.canUndo()).toBe(false);
  });

  test("that drops a panel into another tab group runs blockr.dock-set_grid, one undo step that undo brings dockview back from", async () => {
    const s = await start();
    const panel = s.panel(b);

    panel.api.moveTo({ group: s.panel(c).group, position: "center" });
    await s.after();

    expect(s.grid()).toEqual({ ...main, children: [group(a), { panels: [c, b], active: b }] });
    expect(s.core.store.canUndo()).toBe(true);

    s.core.store.undo();
    await s.after();

    expect(s.grid()).toEqual(main);
    expect(s.shown()).toEqual(main);
    expect(s.panel(b)).toBe(panel);
    expect(s.disposed).toEqual([]);
  });

  test("that closes a tab runs blockr.dock-set_grid without the panel", async () => {
    const s = await start();

    s.panel(a).api.close();
    await s.after();

    expect(s.grid()).toEqual({ ...main, children: [{ panels: [b], active: b }, group(c)] });
  });

  test("that drags a sash runs blockr.dock-set_grid with the new shares", async () => {
    const s = await start();

    await s.drag(() => {
      s.panel(a).group.api.setSize({ width: 0.5 * s.panel(a).group.api.width + 0.5 * s.panel(c).group.api.width });
    });

    const sizes = s.grid()?.sizes ?? [];
    expect(sizes[0]).toBeCloseTo(0.5, 2);
    expect(s.core.store.canUndo()).toBe(true);
  });

  test("that is no drag leaves the shares as they are stored, as a window resize moves them", async () => {
    const s = await start();

    s.api.layout(1003, 800, true);
    s.panel(a).group.api.setSize({ width: 400 });
    await s.after();

    expect(s.grid()).toEqual(main);
    expect(s.core.store.canUndo()).toBe(false);
  });

  test("waits while a pointer is down, and commits once as it goes up", async () => {
    const s = await start();
    const run = vi.spyOn(s.core.commands, "run");

    s.el.dispatchEvent(new PointerEvent("pointerdown", { bubbles: true }));
    s.panel(a).api.setActive();
    s.panel(b).api.setActive();
    s.panel(a).api.setActive();
    await s.after();
    expect(run).not.toHaveBeenCalled();

    window.dispatchEvent(new PointerEvent("pointerup"));
    await s.after();
    expect(run.mock.calls).toEqual([["blockr.dock-activate_panel", { view: "main", panel: a }]]);
  });

  test("that collapses a rail runs blockr.dock-collapse_rail, on every view while sync_rails is on, with no undo step", async () => {
    const s = await start();

    s.api.getEdgeGroup("left")?.collapse();
    await s.after();

    expect(s.grid()?.rails?.left?.collapsed).toBe(true);
    expect(s.grid("side")?.rails?.left?.collapsed).toBe(true);
    expect(s.core.store.canUndo()).toBe(false);
  });

  test("that the board lock refuses brings dockview back to the stored grid, and tells why", async () => {
    const s = await start({ locked: true });

    s.panel(b).api.moveTo({ group: s.panel(c).group, position: "center" });
    await s.after();

    expect(s.grid()).toEqual(main);
    expect(s.shown()).toEqual(main);
    expect(s.core.notifications.get()).toMatchObject([
      { severity: "error", source: { kind: "module", object: { plugin: DOCK }, code: "edit_refused" } },
    ]);
  });
});

describe("a grid that changes otherwise", () => {
  test("has dockview show the panel that another origin's command shows, keeping every panel", async () => {
    const s = await start();

    await s.core.commands.run("blockr.dock-show_panel", { panel: a });
    await s.after();

    expect(s.shown()).toEqual({ ...main, children: [group(a, b), group(c)] });
    expect(s.made).toEqual([a, b, c, edit].map(panelId));
  });

  test("moves dockview's panels where another origin's command moves one, keeping every panel", async () => {
    const s = await start();
    const panel = s.panel(a);

    await s.core.commands.run("blockr.dock-move_panel", { view: "main", panel: a, near: c, side: "below" });
    await s.after();

    expect(s.shown()).toEqual(s.grid());
    expect(s.panel(a)).toBe(panel);
    expect(s.disposed).toEqual([]);
  });

  test("collapses or opens a rail that another origin's command collapses or opens", async () => {
    const s = await start();

    await s.core.commands.run("blockr.dock-collapse_rail", { view: "main", edge: "left", collapsed: true });
    await s.after();
    expect(s.api.getEdgeGroup("left")?.isCollapsed()).toBe(true);

    await s.core.commands.run("blockr.dock-collapse_rail", { view: "main", edge: "left", collapsed: false });
    await s.after();
    expect(s.api.getEdgeGroup("left")?.isCollapsed()).toBe(false);
  });

  test("renames the tab of a block that is renamed", async () => {
    const s = await start();

    await s.core.commands.run("blockr.core-rename_block", { id: "b", name: "Top rows" });
    await s.after();

    expect(s.panel(b).title).toBe("Top rows");
  });

  test("drops the panel of a block that leaves the board", async () => {
    const s = await start();

    await s.core.commands.run("blockr.core-remove_block", { id: "b" });
    await s.after();

    expect(s.api.getPanel(panelId(b))).toBeUndefined();
    expect(s.shown()).toEqual({ ...main, children: [group(a), group(c)] });
  });
});

describe("the rails", () => {
  test("give a rail its stored width back once the dock has room again", async () => {
    vi.stubGlobal("ResizeObserver", Resizes);
    onTestFinished(() => {
      vi.unstubAllGlobals();
    });
    const wide: Grid = { ...main, rails: { left: { ...rail(edit), size: 400 } } };
    const s = await start({ dockState: { ...state, views: { ...state.views, main: { name: "Main", folder: null, index: "a0", grid: wide } } } });
    const width = () => s.api.toJSON().edgeGroups?.left?.size;
    expect(width()).toBe(400);

    s.api.layout(300, 800, true);
    await Resizes.resized(s.el);
    expect(width()).toBeLessThan(400);

    s.api.layout(1000, 800, true);
    await Resizes.resized(s.el);
    await s.after();

    expect(width()).toBe(400);
    expect(s.grid()).toEqual(wide);
    expect(s.core.store.canUndo()).toBe(false);
  });

  test("show a rail only while it holds panels", async () => {
    const s = await start();

    s.panel(edit).api.moveTo({ group: s.panel(c).group, position: "center" });
    await s.after();

    expect(s.api.isEdgeGroupVisible("left")).toBe(false);
    expect(s.grid()?.rails).toBeUndefined();
  });
});

describe("the board lock", () => {
  test("has dockview lock the sizes and the arrangement, unless a deployment leaves them to readers", async () => {
    expect((await start({ locked: true })).updateOptions).toHaveBeenLastCalledWith({ locked: true, disableDnd: true });
    expect((await start({ locked: true, blockrOptions: { lock_resize: false } })).updateOptions).toHaveBeenLastCalledWith({
      locked: false,
      disableDnd: true,
    });
    expect((await start()).updateOptions).toHaveBeenLastCalledWith({ locked: false, disableDnd: false });
  });
});

describe("the narrow column", () => {
  test("stacks the tab groups and rails, as tall as the rows together, with dragging and resizing off", async () => {
    const s = await start({ breakpoint: Infinity });

    expectShows(s.shown(), stackGrid(main, 0.8).grid);
    expect(s.el.dataset.layout).toBe("narrow");
    expect(s.el.style.height).toBe("240vh");
    expect(s.updateOptions).toHaveBeenLastCalledWith({ locked: true, disableDnd: true });
  });

  test("has a tab switch run blockr.dock-activate_panel and a closed tab blockr.dock-remove_panel, on the stored grid", async () => {
    const s = await start({ breakpoint: Infinity });

    s.panel(a).api.setActive();
    await s.after();
    expect(s.grid()).toEqual({ ...main, children: [group(a, b), group(c)] });

    s.panel(c).api.close();
    await s.after();
    expect(s.grid()).toEqual({ ...main, children: [group(a, b)], sizes: [1] });
  });

  test("reads back no share of a row", async () => {
    const s = await start({ breakpoint: Infinity });

    await s.drag(() => {
      s.panel(a).group.api.setSize({ height: 100 });
    });

    expect(s.grid()).toEqual(main);
  });

  test("draws as the window crosses the breakpoint, and the wide grid again as it widens", async () => {
    setWindowWidth(1024);
    const s = await start({ breakpoint: 600 });
    expect(s.shown()).toEqual(main);

    setWindowWidth(500);
    await s.after();
    expectShows(s.shown(), stackGrid(main, 0.8).grid);

    setWindowWidth(1024);
    await s.after();
    expect(s.shown()).toEqual(main);
    expect(s.el.dataset.layout).toBe("wide");
    expect(s.el.style.height).toBe("");
  });
});
