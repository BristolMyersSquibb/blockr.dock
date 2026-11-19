// The dock's frontend on blockr.core's CoreApi for tests, which starts core
// as the page of an app that runs the dock would, with R played from the
// fixtures of core's wire protocol, so that the tests need neither R nor a
// browser (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#testing).

import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import type { CoreApi, JsonBoard, PluginModule } from "@blockr/core";
import { HELLO, stubCore } from "@blockr/core/stubs";
import { flushSync } from "react-dom";
import { describe, expect, onTestFinished, test, vi } from "vitest";
import { defaultState } from "../../state/index.ts";
import type { DockState, Grid, Panel } from "../../state/index.ts";
import * as dock from "./index.tsx";
import { DOCK, hello, modules, startDock } from "./testing.ts";
import { FADES_FOR, SHOWN_FOR } from "./toasts.tsx";
// The fixture of a board that R writes for a dock app script is a file of the
// R package, which R's suite checks too.
// eslint-disable-next-line no-restricted-imports
import dockFixture from "../../../inst/board/fixtures/dock.json" with { type: "json" };

describe("the dock's frontend", () => {
  test("is the frontend of the dock plugin's class", async () => {
    const { core, stop } = await startDock();

    const cls = core.plugins.pluginClass(DOCK);
    expect(cls.frontend).toBe(dock.default.frontend);
    stop();
  });

  test("gives a board that opens without their values the defaults of the board options that its class requires", async () => {
    const { core, settled, stop } = await startDock(document.createElement("div"));
    await settled();

    // The fixture's board holds values for n_rows and show_conditions alone.
    const options = core.store.board().options;
    expect(options).toMatchObject({ compact: false, view_tabs: false, sync_rails: true, show_conditions: ["warning", "error"] });
    expect(Object.keys(options).sort()).toEqual([
      "compact",
      "dark_mode",
      "n_rows",
      "show_conditions",
      "sync_rails",
      "view_tabs",
    ]);
    stop();
  });

  test("takes those defaults from the blockr options of their names, which R holds", async () => {
    const blockrOptions = { compact: "TRUE", view_tabs: true, sync_rails: "F" };
    const { core, settled, stop } = await startDock(document.createElement("div"), blockrOptions);
    await settled();

    expect(core.store.board().options).toMatchObject({ compact: true, view_tabs: true, sync_rails: false });
    stop();
  });

  test("keeps the values that the board holds, which no default replaces", async () => {
    const el = document.createElement("div");
    const { core, settled, stop } = await startDock(el);
    await settled();

    await core.commands.run("blockr.core-set_option", { id: "sync_rails", value: false });
    await core.commands.run("blockr.core-set_option", { id: "compact", value: true });
    await settled();

    expect(core.store.board().options).toMatchObject({ compact: true, view_tabs: false, sync_rails: false });
    stop();
  });

  test("draws the active view with dockview, in the page's element, on dockview's light-spaced theme", async () => {
    const el = document.createElement("div");
    const { settled, stop } = await startDock(el);
    await settled();

    const shell = el.querySelector(".blockr-dock .dv-shell");
    expect(shell?.classList).toContain("dockview-theme-light-spaced");
    // The rail on the left edge comes first.
    expect(tabs(el)).toEqual(["Edit board", "Iris", "Head", "Merge", "Wide sepals", "Bind", "Scatter"]);
    expect(el.querySelectorAll(".blockr-dock-panel[data-block]")).not.toHaveLength(0);
    stop();
  });

  test("keeps dockview's light-spaced theme while dark_mode is dark, as the stylesheet's bridge follows dark_mode", async () => {
    const el = document.createElement("div");
    const { core, settled, stop } = await startDock(el);
    await settled();

    await core.commands.run("blockr.core-set_option", { id: "dark_mode", value: "dark" });
    await settled();
    expect(el.querySelector(".blockr-dock .dv-shell")?.classList).toContain("dockview-theme-light-spaced");
    expect(document.documentElement.getAttribute("data-bc-theme")).toBe("dark");
    stop();
  });

  test("removes what it drew in its cleanup", async () => {
    const s = await startDock();
    const el = document.createElement("div");
    const cleanup = await dock.default.frontend.render({
      el,
      core: s.core,
      plugin: DOCK,
      signal: new AbortController().signal,
      args: {},
    });
    expect(el.querySelector(".blockr-dock")).not.toBeNull();

    cleanup?.();
    expect(el.childElementCount).toBe(0);
    s.stop();
  });

  test("draws nothing where its signal aborts while it reads the blockr options", async () => {
    const s = await startDock();
    const el = document.createElement("div");
    const controller = new AbortController();
    const rendering = dock.default.frontend.render({ el, core: s.core, plugin: DOCK, signal: controller.signal, args: {} });
    controller.abort();

    expect(await rendering).toBeUndefined();
    expect(el.childElementCount).toBe(0);
    s.stop();
  });

  test("fails to draw, naming the value, where R holds a blockr option that the dock does not take", async () => {
    const el = document.createElement("div");
    const { settled, stop } = await startDock(el, { narrow_breakpoint: "wide" });
    await settled();

    expect(el.textContent).toContain('The blockr option narrow_breakpoint is "wide"');
    expect(el.querySelector(".blockr-dock")).toBeNull();
    stop();
  });

  test("shows no close button on a tab under the board lock", async () => {
    const el = document.createElement("div");
    const { settled, stop } = await stubCore({ hello: { ...hello, lock: { board: true, load: false } }, modules, el });
    await settled();

    expect(tabs(el)).not.toHaveLength(0);
    expect(el.querySelectorAll(".dv-default-tab-action")).toHaveLength(0);
    stop();
  });

  test("shows a close button on each tab of a board without the board lock", async () => {
    const el = document.createElement("div");
    const { settled, stop } = await startDock(el);
    await settled();

    expect(el.querySelectorAll(".dv-default-tab-action")).toHaveLength(tabs(el).length);
    stop();
  });

  test("draws the page again as another board opens", async () => {
    const el = document.createElement("div");
    const s = await startDock(el);
    await s.settled();
    const before = el.querySelector(".blockr-dock");

    await s.core.store.open(JSON.stringify(HELLO.board));
    await s.settled();

    const after = el.querySelector(".blockr-dock");
    expect(after).not.toBeNull();
    expect(after).not.toBe(before);
    expect(before?.isConnected).toBe(false);
    s.stop();
  });
});

// The titles of the tabs that the dock draws in the element, in the order of
// the page.
function tabs(el: HTMLElement): string[] {
  return [...el.querySelectorAll(".dv-tab")].map((tab) => tab.textContent.trim());
}

// A board with two blocks, a stack, and the dock's state at version 1, given.
function boardWith(dockState: DockState): JsonBoard {
  return {
    format: 1,
    packages: { "blockr.core": "0.2.0", "blockr.dock": "0.2.1" },
    board: {
      id: "curious_otter",
      name: "Curious otter",
      blocks: {
        a: { registry_id: "blockr.core-dataset_block", version: 1, state: { dataset: "iris", package: "datasets" }, name: "Dataset", locked: null },
        b: { registry_id: "blockr.core-head_block", version: 1, state: { n: 3, direction: "head" }, name: "Head", locked: null },
      },
      links: { ab: { from: "a", to: "b", input: "data" } },
      stacks: { s1: { name: "Input", blocks: ["a", "b"], color: "#ffcc88" } },
      options: {
        dark_mode: { value: "light", version: 1 },
        compact: { value: false, version: 1 },
        view_tabs: { value: false, version: 1 },
        sync_rails: { value: true, version: 1 },
      },
      plugins: { [DOCK]: { state: dockState, version: 1 } },
    },
  };
}

// The dock's state with two views: the first shows the blocks side by side,
// with the DAG on a rail at its right edge, collapsed, and the second, which
// is active, shows `b` alone, whose card shows its output alone.
const twoViews: DockState = {
  views: {
    main: {
      name: "Main",
      folder: null,
      index: "a0",
      grid: {
        orientation: "horizontal",
        children: [
          {
            children: [
              { panels: [{ block: "a" }], active: { block: "a" } },
              { panels: [{ block: "b" }], active: { block: "b" } },
            ],
            sizes: [0.5, 0.5],
          },
        ],
        sizes: [1],
        rails: {
          right: {
            panels: [{ plugin: "blockr.dag-dag_plugin" }],
            active: { plugin: "blockr.dag-dag_plugin" },
            collapsed: true,
            size: 300,
          },
        },
      },
    },
    side: {
      name: "Side",
      folder: null,
      index: "a1",
      grid: { orientation: "horizontal", children: [{ panels: [{ block: "b" }], active: { block: "b" } }], sizes: [1] },
    },
  },
  folders: {},
  active: "side",
  visible: { b: ["outputs"] },
};

// The fixture in inst/board/fixtures/dock.json, without its description: the
// JSON board that R writes for a board that a dock app script builds.
// R's suite checks that blockr_ser() writes it as the file holds it
// (tests/testthat/helper-board-fixtures.R).
function rBoard(): JsonBoard {
  const { description, ...board } = dockFixture as unknown as JsonBoard & { description: string };
  expect(description).not.toBe("");
  return board;
}

describe("the dock plugin's state", () => {
  test("is what R writes for a board that an app script builds, which opens as R writes it", async () => {
    const board = rBoard();
    const { core, settled, stop } = await startDock(undefined, {}, board);
    await settled();

    expect(core.store.board().plugins[DOCK]).toEqual(board.board.plugins[DOCK]?.state);
    expect(core.store.jsonBoard().board.plugins[DOCK]?.version).toBe(1);
    stop();
  });

  test("is the class's default, which holds no view, on a board that holds none for the dock", async () => {
    const { core, settled, stop } = await startDock();
    await settled();

    expect(core.plugins.pluginClass(DOCK).state?.default).toEqual(defaultState);
    expect(core.store.board().plugins[DOCK]).toEqual(defaultState);
    stop();
  });

  test("is what a board holds at version 1, as the board opens", async () => {
    const { core, settled, stop } = await startDock(undefined, {}, boardWith(twoViews));
    await settled();

    expect(core.store.board().plugins[DOCK]).toEqual(twoViews);
    expect(core.store.jsonBoard().board.plugins[DOCK]?.version).toBe(1);
    stop();
  });

  test("loses a removed block's panel from every view and its entry in visible, which one undo takes back", async () => {
    const { core, settled, stop } = await startDock(undefined, {}, boardWith(twoViews));
    await settled();

    await core.commands.run("blockr.core-remove_block", { id: "b" });
    expect(core.store.board().plugins[DOCK]).toMatchObject({
      views: {
        main: { grid: { children: [{ children: [{ panels: [{ block: "a" }] }], sizes: [1] }] } },
        side: { grid: { children: [], sizes: [] } },
      },
      visible: {},
    });

    core.store.undo();
    expect(core.store.board().plugins[DOCK]).toEqual(twoViews);
    stop();
  });

  test("gains an added block's panel in the active view, at the default place", async () => {
    const { core, settled, stop } = await startDock(undefined, {}, boardWith(twoViews));
    await settled();

    await core.commands.run("blockr.core-add_block", { registry_id: "blockr.core-head_block", id: "c" });
    expect(core.store.board().plugins[DOCK]).toMatchObject({
      views: { side: { grid: { children: [{ panels: [{ block: "b" }, { block: "c" }], active: { block: "c" } }] } } },
    });
    stop();
  });

  test("makes the first view of the navigation active where core's command removes the active view", async () => {
    const { core, settled, stop } = await startDock(undefined, {}, boardWith(twoViews));
    await settled();

    await core.commands.run("blockr.core-set_plugin_state", { plugin: DOCK, patches: [{ op: "remove", path: "/views/side" }] });
    expect(core.store.board().plugins[DOCK]).toMatchObject({ active: "main" });
    stop();
  });

  test("refuses a state that its schema refuses, as a share of 0", async () => {
    const { core, settled, stop } = await startDock(undefined, {}, boardWith(twoViews));
    await settled();

    const run = core.commands.run("blockr.core-set_plugin_state", {
      plugin: DOCK,
      patches: [{ op: "replace", path: "/views/main/grid/children/0/sizes", value: [0, 1] }],
    });
    await expect(run).rejects.toThrow();
    expect(core.store.board().plugins[DOCK]).toEqual(twoViews);
    stop();
  });
});

describe("the step on starting", () => {
  test("adds the view Page to a board that holds no view, with the plugins on its left rail, which undo does not take back", async () => {
    const { core, settled, stop } = await startDock(document.createElement("div"));
    await settled();

    const blocks = ["a", "b", "c", "d", "e", "f"].map((block) => ({ block }));
    expect(core.store.board().plugins[DOCK]).toEqual({
      views: {
        page: {
          name: "Page",
          folder: null,
          index: "a0",
          grid: {
            orientation: "horizontal",
            children: [{ panels: blocks, active: blocks[0] }],
            sizes: [1],
            rails: {
              left: {
                panels: [{ plugin: "blockr.core-edit_board_plugin" }],
                active: { plugin: "blockr.core-edit_board_plugin" },
                collapsed: false,
                size: 500,
              },
            },
          },
        },
      },
      folders: {},
      active: "page",
      visible: {},
    });
    expect(core.store.canUndo()).toBe(false);
    stop();
  });

  test("removes the panels of plugins that the app does not run, as one step that undo does not take back", async () => {
    const { core, settled, stop } = await startDock(document.createElement("div"), {}, boardWith(twoViews));
    await settled();

    const state = core.store.board().plugins[DOCK] as DockState;
    expect(state.views.main?.grid).toEqual({ ...twoViews.views.main?.grid, rails: undefined });
    expect(state.views.main?.grid.rails).toBeUndefined();
    expect(state.views.side).toEqual(twoViews.views.side);
    expect(core.store.canUndo()).toBe(false);
    stop();
  });

  test("makes active the view that the URL's query parameter view names", async () => {
    window.history.replaceState(null, "", "/?view=main");
    onTestFinished(() => {
      window.history.replaceState(null, "", "/");
    });
    const { core, settled, stop } = await startDock(document.createElement("div"), {}, boardWith(twoViews));
    await settled();

    expect(core.store.board().plugins[DOCK]).toMatchObject({ active: "main" });
    stop();
  });

  test("changes nothing for a URL that names no view", async () => {
    window.history.replaceState(null, "", "/?view=gone");
    onTestFinished(() => {
      window.history.replaceState(null, "", "/");
    });
    const { core, settled, stop } = await startDock(document.createElement("div"), {}, boardWith(twoViews));
    await settled();

    expect(core.store.board().plugins[DOCK]).toMatchObject({ active: "side" });
    stop();
  });
});

describe("the demand", () => {
  // A board whose view shows b in front of c, beside nothing else, while a
  // is upstream of b.
  const board: JsonBoard = {
    ...boardWith({
      views: {
        main: {
          name: "Main",
          folder: null,
          index: "a0",
          grid: { orientation: "horizontal", children: [{ panels: [{ block: "b" }, { block: "c" }], active: { block: "b" } }], sizes: [1] },
        },
      },
      folders: {},
      active: "main",
      visible: {},
    }),
  };
  const withC: JsonBoard = {
    ...board,
    board: {
      ...board.board,
      blocks: {
        ...board.board.blocks,
        c: { registry_id: "blockr.core-dataset_block", version: 1, state: { dataset: "mtcars", package: "datasets" }, name: "Cars", locked: null },
      },
    },
  };

  // The blocks that core sent R to evaluate, in order.
  function evaluated(sent: readonly { readonly kind: string }[]): string[] {
    return sent.flatMap((request) => (request.kind === "evaluate" ? [(request as unknown as { block: string }).block] : []));
  }

  test("has the blocks whose cards are on screen evaluated, with what is upstream of them, and no other block", async () => {
    const { sent, settled, stop } = await startDock(document.createElement("div"), {}, withC);
    await settled();

    expect(evaluated(sent).sort()).toEqual(["a", "b"]);
    stop();
  });

  test("follows the panel that a tab group shows", async () => {
    const { core, sent, settled, stop } = await startDock(document.createElement("div"), {}, withC);
    await settled();

    await core.commands.run("blockr.dock-activate_panel", { view: "main", panel: { block: "c" } });
    await settled();

    expect(evaluated(sent).sort()).toEqual(["a", "b", "c"]);
    stop();
  });
});

// Core's Edit board plugin, which the app of the fixture hello runs, and the
// notes plugin, of a class that a test gives core.
const EDIT = "blockr.core-edit_board_plugin";
const NOTES = "blockr.test-notes_plugin";

// The class of the notes plugin, whose `ui` writes "Notes" in its element and
// keeps what each render() gets and how many of its cleanups ran.
function notesClass() {
  const renders: { readonly el: HTMLElement; readonly signal: AbortSignal }[] = [];
  let cleanups = 0;
  const ui: PluginModule = {
    render({ el, signal }) {
      renders.push({ el, signal });
      el.textContent = "Notes";
      return () => {
        cleanups += 1;
      };
    },
  };
  return { renders, cleanups: () => cleanups, module: { api: 1, default: { ui } } };
}

// Starts core with the dock's modules, where the app runs the notes plugin
// after core's plugins, on the board given, under the board lock where
// `locked` says so.
// R serves the module of the notes plugin's class, which the test gives by the
// class's id, and core registers the class as it opens the board.
async function startWithNotes(el: HTMLElement, board: JsonBoard, locked = false) {
  const notes = notesClass();
  const s = await stubCore({
    hello: {
      ...hello,
      board,
      plugins: { ...hello.plugins, [NOTES]: { name: "Notes", version: 1, module: "blockr.test/js/notes.js" } },
      ...(locked ? { lock: { board: true, load: false } } : {}),
    },
    modules: { ...modules, [NOTES]: notes.module },
    el,
  });
  await s.settled();
  return { ...s, notes };
}

// The dock's state with the view main alone, which is active, of the grid
// given, beside the view side where `side` gives its grid.
function dockStateWith(grid: Grid, side?: Grid): DockState {
  return {
    views: {
      main: { name: "Main", folder: null, index: "a0", grid },
      ...(side === undefined ? {} : { side: { name: "Side", folder: null, index: "a1", grid: side } }),
    },
    folders: {},
    active: "main",
    visible: {},
  };
}

// A grid of the tab groups given, side by side.
function gridWith(...groups: (readonly Panel[])[]): Grid {
  return {
    orientation: "horizontal",
    children: groups.map((panels) => ({ panels, active: panels[panels.length - 1] ?? { block: "a" } })),
    sizes: groups.map(() => 1 / groups.length),
  };
}

const EMPTY_GRID: Grid = { orientation: "horizontal", children: [], sizes: [] };

// A grid with a's panel in one tab group and the notes plugin's on the left
// rail, collapsed or open.
function notesOnRail(collapsed: boolean): Grid {
  return { ...gridWith([{ block: "a" }]), rails: { left: { panels: [{ plugin: NOTES }], active: { plugin: NOTES }, collapsed, size: 260 } } };
}

// An element in the document, which the focus can reach, until the test ends.
function attached(): HTMLElement {
  const el = document.body.appendChild(document.createElement("div"));
  onTestFinished(() => {
    el.remove();
  });
  return el;
}

// The frame of a plugin's panel, which the dock draws in the element.
function panelFrame(el: HTMLElement, plugin: string): HTMLElement | null {
  return el.querySelector<HTMLElement>(`.blockr-dock-panel[data-plugin="${plugin}"]`);
}

describe("a plugin's panel", () => {
  test("holds the plugin's UI, which the dock mounts in the panel's frame with mountPlugin(), and its tab shows the plugin's name", async () => {
    const el = document.createElement("div");
    const s = await startWithNotes(el, boardWith(dockStateWith(gridWith([{ plugin: EDIT }, { plugin: NOTES }], [{ block: "a" }]))));

    expect(tabs(el)).toEqual(["Edit board", "Notes", "Dataset"]);
    expect(panelFrame(el, EDIT)?.querySelector('section[aria-label="Links"]')).not.toBeNull();
    expect(s.notes.renders).toHaveLength(1);
    expect(s.notes.renders[0]?.el).toBe(panelFrame(el, NOTES));
    expect(panelFrame(el, NOTES)?.textContent).toBe("Notes");
    s.stop();
  });

  test("stays mounted behind another tab, which dockview hides", async () => {
    const el = document.createElement("div");
    const s = await startWithNotes(el, boardWith(dockStateWith(gridWith([{ plugin: EDIT }, { plugin: NOTES }]))));

    await s.core.commands.run("blockr.dock-activate_panel", { view: "main", panel: { plugin: EDIT } });
    await s.settled();

    const frame = panelFrame(el, NOTES);
    expect(s.notes.renders).toHaveLength(1);
    expect(s.notes.cleanups()).toBe(0);
    expect(frame?.textContent).toBe("Notes");
    expect(frame?.closest<HTMLElement>(".dv-render-overlay")?.style.visibility).toBe("hidden");
    s.stop();
  });

  test("stays mounted on a collapsed rail, which dockview hides rather than squeezes", async () => {
    const el = document.createElement("div");
    const s = await startWithNotes(el, boardWith(dockStateWith(notesOnRail(true))));

    const frame = panelFrame(el, NOTES);
    expect(s.notes.renders).toHaveLength(1);
    expect(frame?.textContent).toBe("Notes");
    expect(frame?.closest<HTMLElement>(".dv-render-overlay")?.style.visibility).toBe("hidden");

    await s.core.commands.run("blockr.dock-collapse_rail", { view: "main", edge: "left", collapsed: false });
    await s.settled();
    expect(s.notes.renders).toHaveLength(1);
    expect(s.notes.cleanups()).toBe(0);
    s.stop();
  });

  test("runs the cleanup of the plugin's UI once the panel goes, as its tab closes, as the view changes and as another board opens", async () => {
    const el = document.createElement("div");
    const s = await startWithNotes(el, boardWith(dockStateWith(gridWith([{ plugin: EDIT }, { plugin: NOTES }]), gridWith([{ block: "b" }]))));

    await s.core.commands.run("blockr.dock-remove_panel", { view: "main", panel: { plugin: NOTES } });
    await s.settled();
    expect(s.notes.cleanups()).toBe(1);
    expect(s.notes.renders[0]?.signal.aborted).toBe(true);
    expect(panelFrame(el, NOTES)).toBeNull();

    await s.core.commands.run("blockr.dock-add_panel", { view: "main", panel: { plugin: NOTES } });
    await s.settled();
    expect(s.notes.renders).toHaveLength(2);

    await s.core.commands.run("blockr.dock-activate_view", { view: "side" });
    await s.settled();
    expect(s.notes.cleanups()).toBe(2);
    expect(panelFrame(el, EDIT)).toBeNull();

    await s.core.commands.run("blockr.dock-activate_view", { view: "main" });
    await s.settled();
    expect(s.notes.renders).toHaveLength(3);

    await s.core.store.open(JSON.stringify(HELLO.board));
    await s.settled();
    expect(s.notes.cleanups()).toBe(3);
    s.stop();
  });

  test("ends a UI that draws in a React root of its own, as the Edit board plugin's does, without an error, as its tab closes", async () => {
    const el = document.createElement("div");
    const s = await startWithNotes(el, boardWith(dockStateWith(gridWith([{ block: "a" }, { plugin: EDIT }]))));
    const frame = panelFrame(el, EDIT);
    expect(frame?.querySelector('section[aria-label="Links"]')).not.toBeNull();

    // React reports an error of its commit as an error event of the window,
    // and logs what went wrong as an error.
    const reported: unknown[] = [];
    const report = (event: ErrorEvent) => {
      reported.push(event.error);
      event.preventDefault();
    };
    window.addEventListener("error", report);
    const logged = vi.spyOn(console, "error").mockImplementation(() => undefined);
    onTestFinished(() => {
      window.removeEventListener("error", report);
      logged.mockRestore();
    });
    await s.core.commands.run("blockr.dock-remove_panel", { view: "main", panel: { plugin: EDIT } });
    await s.settled();

    expect(panelFrame(el, EDIT)).toBeNull();
    expect(frame?.childElementCount).toBe(0);
    expect(reported).toEqual([]);
    expect(logged).not.toHaveBeenCalled();
    s.stop();
  });
});

// The tab that dockview draws for the panel of the name given.
function tabNamed(el: HTMLElement, name: string): HTMLElement {
  const tab = [...el.querySelectorAll<HTMLElement>(".dv-tab")].find(
    (each) => each.querySelector(".dv-default-tab-content")?.textContent === name,
  );
  if (tab === undefined) throw new Error(`No tab shows ${name}.`);
  return tab;
}

// The severity of the dot on the tab of the name given, or null for none.
function dotOn(el: HTMLElement, name: string): string | null {
  return tabNamed(el, name).querySelector(".blockr-dock-tab-frame")?.getAttribute("data-notified") ?? null;
}

// The lines of the tooltip that shows, or null where none shows.
function tooltipLines(el: HTMLElement): string[] | null {
  const tooltip = el.querySelector('[role="tooltip"]');
  return tooltip === null ? null : [...tooltip.querySelectorAll(".blockr-card-tooltip-line")].map((line) => line.textContent);
}

// What the notes plugin raises about itself.
const lost = { severity: "error", code: "lost", message: "Lost the draft." } as const;
const slow = { severity: "warning", code: "slow", message: "Saving is slow." } as const;
const saved = { severity: "message", code: "saved", message: "Saved." } as const;

describe("the tab of a plugin's panel", () => {
  test("shows a dot while a warning or an error about the plugin holds, the error's while both hold, and none for a message", async () => {
    const el = document.createElement("div");
    // The tab group shows a's panel, with the notes plugin's behind it.
    const s = await startWithNotes(el, boardWith(dockStateWith(gridWith([{ plugin: NOTES }, { block: "a" }]))));
    expect(dotOn(el, "Notes")).toBeNull();

    let withdrawLost: () => void = () => undefined;
    let withdrawSlow: () => void = () => undefined;
    flushSync(() => {
      withdrawSlow = s.core.notifications.notify(NOTES, slow);
    });
    expect(dotOn(el, "Notes")).toBe("warning");
    flushSync(() => {
      withdrawLost = s.core.notifications.notify(NOTES, lost);
    });
    expect(dotOn(el, "Notes")).toBe("error");
    flushSync(() => {
      withdrawLost();
    });
    expect(dotOn(el, "Notes")).toBe("warning");
    flushSync(() => {
      withdrawSlow();
      s.core.notifications.notify(NOTES, saved);
    });
    expect(dotOn(el, "Notes")).toBeNull();
    // A block's tab shows none, nor does another plugin's.
    flushSync(() => {
      s.core.notifications.notify(EDIT, { ...lost, about: { block: "a" } });
    });
    expect(el.querySelectorAll(".blockr-dock-tab-frame[data-notified]")).toHaveLength(0);
    s.stop();
  });

  test("shows the dot on the tab of a collapsed rail, which shows its tabs alone", async () => {
    const el = document.createElement("div");
    const s = await startWithNotes(el, boardWith(dockStateWith(notesOnRail(true))));

    flushSync(() => {
      s.core.notifications.notify(NOTES, lost);
    });
    expect(dotOn(el, "Notes")).toBe("error");
    expect(toasts(el)).toEqual([]);
    s.stop();
  });

  test("tells of the severities that the board option show_conditions names", async () => {
    const el = document.createElement("div");
    const s = await startWithNotes(el, boardWith(dockStateWith(gridWith([{ block: "a" }, { plugin: NOTES }]))));
    await s.core.commands.run("blockr.core-set_option", { id: "show_conditions", value: ["error"] });
    await s.settled();

    flushSync(() => {
      s.core.notifications.notify(NOTES, slow);
    });
    expect(dotOn(el, "Notes")).toBeNull();
    flushSync(() => {
      s.core.notifications.notify(NOTES, lost);
    });
    expect(dotOn(el, "Notes")).toBe("error");
    s.stop();
  });

  test("holds their messages, the errors first, in its tooltip, as the keyboard's focus moves onto it or the pointer rests on it, and as its description", async () => {
    const el = attached();
    const s = await startWithNotes(el, boardWith(dockStateWith(gridWith([{ block: "a" }, { plugin: NOTES }]))));
    const tab = tabNamed(el, "Notes");
    expect(tab.hasAttribute("aria-description")).toBe(false);

    let withdraw: () => void = () => undefined;
    flushSync(() => {
      s.core.notifications.notify(NOTES, slow);
      withdraw = s.core.notifications.notify(NOTES, { ...lost, message: "Lost the draft:\n- \u001b[1mintro\u001b[22m" });
    });
    expect(tab.getAttribute("aria-description")).toBe("Lost the draft:\n- intro\nSaving is slow.");

    tab.focus();
    await drawn();
    expect(tooltipLines(el)).toEqual(["Lost the draft:", "- intro", "Saving is slow."]);
    tab.blur();
    await drawn();
    expect(tooltipLines(el)).toBeNull();

    tab.dispatchEvent(new PointerEvent("pointerenter", { pointerType: "mouse" }));
    await new Promise((resolve) => setTimeout(resolve, 350));
    expect(tooltipLines(el)).toEqual(["Lost the draft:", "- intro", "Saving is slow."]);
    tab.dispatchEvent(new PointerEvent("pointerleave", { pointerType: "mouse" }));
    await drawn();
    expect(tooltipLines(el)).toBeNull();

    flushSync(() => {
      withdraw();
    });
    expect(tab.getAttribute("aria-description")).toBe("Saving is slow.");
    s.stop();
  });

  test("has no tooltip while no warning or error about the plugin holds", async () => {
    const el = attached();
    const s = await startWithNotes(el, boardWith(dockStateWith(gridWith([{ block: "a" }, { plugin: NOTES }]))));
    flushSync(() => {
      s.core.notifications.notify(NOTES, saved);
    });

    tabNamed(el, "Notes").focus();
    await drawn();
    expect(tooltipLines(el)).toBeNull();
    expect(tabNamed(el, "Notes").hasAttribute("aria-description")).toBe(false);
    s.stop();
  });
});

// Clicks the button as a pointer does, which goes down on it first.
function click(button: Element | null | undefined): void {
  if (button === null || button === undefined) throw new Error("No button to click.");
  button.dispatchEvent(new PointerEvent("pointerdown", { bubbles: true }));
  button.dispatchEvent(new MouseEvent("click", { bubbles: true, detail: 1 }));
}

// Presses a key in the element that has the focus.
function press(key: string): void {
  document.activeElement?.dispatchEvent(new KeyboardEvent("keydown", { key, bubbles: true }));
}

// Waits for React to draw what an event changed.
async function drawn(): Promise<void> {
  await new Promise((resolve) => setTimeout(resolve, 0));
}

// The entries that the picker lists, each by its name, with the name of the
// block's class where it has one.
function entries(el: HTMLElement): string[][] {
  return [...el.querySelectorAll(".blockr-dock-menu [role='option']")].map((item) =>
    [...item.querySelectorAll(".blockr-dock-menu-label, .blockr-dock-menu-detail")].map((part) => part.textContent),
  );
}

// The entry of the picker of the name given.
function entry(el: HTMLElement, name: string): Element | undefined {
  return [...el.querySelectorAll(".blockr-dock-menu [role='option']")].find(
    (item) => item.querySelector(".blockr-dock-menu-label")?.textContent === name,
  );
}

// The view main's grid in the dock's state.
function mainGrid(core: CoreApi): Grid | undefined {
  return (core.store.board().plugins[DOCK] as DockState).views.main?.grid;
}

describe("the panel picker", () => {
  test('opens from a "+" in the header of each tab group, and from none on a rail', async () => {
    const el = document.createElement("div");
    const s = await startWithNotes(el, boardWith(dockStateWith({ ...notesOnRail(false), ...gridWith([{ block: "a" }], [{ block: "b" }]) })));

    const buttons = [...el.querySelectorAll(".blockr-dock-add")];
    expect(buttons).toHaveLength(2);
    expect(buttons.map((button) => button.getAttribute("aria-label"))).toEqual(["Add panel", "Add panel"]);
    expect(el.querySelector(".dv-groupview-edge .blockr-dock-add")).toBeNull();
    expect(el.querySelector(".blockr-dock-menu [role='listbox']")).toBeNull();

    click(buttons[0]);
    await drawn();
    expect(buttons[0]?.getAttribute("aria-expanded")).toBe("true");
    expect(el.querySelector(".blockr-dock-menu-caption")?.textContent).toBe("Show in this view");
    s.stop();
  });

  test("lists the blocks that the active view does not show, by name with their class, then under Plugins each plugin with a ui that it does not show", async () => {
    const el = document.createElement("div");
    const s = await startWithNotes(el, boardWith(dockStateWith(gridWith([{ block: "b" }]))));

    click(el.querySelector(".blockr-dock-add"));
    await drawn();

    // Core's file storage plugin, whose class has no ui, has no panel.
    expect(entries(el)).toEqual([["Dataset", "dataset block"], ["Edit board"], ["Notes"]]);
    const group = el.querySelector(".blockr-dock-menu [role='group']");
    const heading = el.querySelector(`[id="${group?.getAttribute("aria-labelledby") ?? ""}"]`);
    expect(heading?.textContent).toBe("Plugins");
    expect([...(group?.querySelectorAll(".blockr-dock-menu-label") ?? [])].map((item) => item.textContent)).toEqual(["Edit board", "Notes"]);
    expect(el.querySelector(".blockr-dock-menu-search input")).not.toBeNull();
    s.stop();
  });

  test('adds a pick from the "+" of a tab group as a new tab of that group, which the group shows', async () => {
    const el = document.createElement("div");
    const s = await startWithNotes(el, boardWith(dockStateWith(gridWith([{ block: "b" }], [{ plugin: NOTES }]))));

    click(el.querySelectorAll(".blockr-dock-add")[0]);
    await drawn();
    click(entry(el, "Edit board"));
    await s.settled();

    expect(mainGrid(s.core)?.children).toEqual([
      { panels: [{ block: "b" }, { plugin: EDIT }], active: { plugin: EDIT } },
      { panels: [{ plugin: NOTES }], active: { plugin: NOTES } },
    ]);
    expect(panelFrame(el, EDIT)?.querySelector('section[aria-label="Links"]')).not.toBeNull();
    expect(el.querySelector(".blockr-dock-menu [role='listbox']")).toBeNull();
    s.stop();
  });

  test("leaves it to core to say why the board refuses a pick", async () => {
    const el = document.createElement("div");
    const s = await startWithNotes(el, boardWith(dockStateWith(gridWith([{ block: "a" }], [{ block: "b" }]))));

    // The picker opens from the "+" of a's tab group.
    // A command then takes a's panel out of the view, and the picker stays
    // open.
    // So the pick goes near a panel that the view no longer shows, and the
    // board refuses it.
    click(el.querySelectorAll(".blockr-dock-add")[0]);
    await drawn();
    await s.core.commands.run("blockr.dock-remove_panel", { view: "main", panel: { block: "a" } });
    await s.settled();
    await drawn();
    click(entry(el, "Notes"));
    await s.settled();

    expect(mainGrid(s.core)?.children).toEqual([{ panels: [{ block: "b" }], active: { block: "b" } }]);
    expect(s.core.notifications.get()).toMatchObject([{ severity: "error", source: { kind: "core", code: "board_edit_failed" } }]);
    expect(toasts(el)).toEqual([["error", "Board", expect.stringContaining("which near names")]]);
    s.stop();
  });

  test("says why where the active view shows every block and plugin already", async () => {
    const el = document.createElement("div");
    const s = await startWithNotes(el, boardWith(dockStateWith(gridWith([{ block: "a" }, { block: "b" }, { plugin: EDIT }, { plugin: NOTES }]))));

    click(el.querySelector(".blockr-dock-add"));
    await drawn();

    expect(el.querySelector(".blockr-dock-menu [role='listbox']")).toBeNull();
    expect(el.querySelector(".blockr-dock-menu-note")?.textContent).toBe(
      "All blocks and plugins are already in this view. Add a new block to the board first.",
    );
    s.stop();
  });

  test("filters its entries by the search field that has the focus as it opens, where Enter picks the first", async () => {
    const el = attached();
    const s = await startWithNotes(el, boardWith(dockStateWith(EMPTY_GRID)));

    click(el.querySelector(".blockr-dock-empty-add"));
    await drawn();
    const search = el.querySelector<HTMLInputElement>(".blockr-dock-menu-search input");
    expect(document.activeElement).toBe(search);
    expect(entries(el)).toHaveLength(4);

    // The search finds an entry by its name, its block class or its id.
    type(search, "set");
    await drawn();
    expect(entries(el)).toEqual([["Dataset", "dataset block"]]);
    type(search, "tail");
    await drawn();
    expect(entries(el)).toEqual([["Head", "head/tail block"]]);
    type(search, "test-notes");
    await drawn();
    expect(entries(el)).toEqual([["Notes"]]);

    type(search, "zzz");
    await drawn();
    expect(entries(el)).toEqual([]);
    expect(el.querySelector(".blockr-dock-menu-note")?.textContent).toBe("No panel matches.");

    type(search, "hea");
    await drawn();
    press("Enter");
    await s.settled();
    expect(mainGrid(s.core)?.children).toEqual([{ panels: [{ block: "b" }], active: { block: "b" } }]);
    s.stop();
  });

  test("keeps the focus in its search, where the arrows move the active entry round the list and Enter picks it", async () => {
    const el = attached();
    const s = await startWithNotes(el, boardWith(dockStateWith(gridWith([{ block: "b" }]))));

    const button = el.querySelector<HTMLElement>(".blockr-dock-add");
    button?.focus();
    press("ArrowDown");
    await drawn();
    const search = el.querySelector<HTMLInputElement>(".blockr-dock-menu-search input");
    expect(document.activeElement).toBe(search);

    // The entry that the search names as its active descendant.
    const active = () =>
      document.getElementById(search?.getAttribute("aria-activedescendant") ?? "")?.querySelector(".blockr-dock-menu-label")
        ?.textContent;
    expect(active()).toBe("Dataset");
    for (const [key, name] of [
      ["ArrowDown", "Edit board"],
      ["ArrowDown", "Notes"],
      ["ArrowDown", "Dataset"],
      ["ArrowUp", "Notes"],
    ] as const) {
      press(key);
      await drawn();
      expect(active()).toBe(name);
    }
    expect(document.activeElement).toBe(search);

    press("Enter");
    await s.settled();
    expect(mainGrid(s.core)?.children).toEqual([{ panels: [{ block: "b" }, { plugin: NOTES }], active: { plugin: NOTES } }]);
    s.stop();
  });
});

// Types the text into the search field, as the user does, in place of what it
// held.
function type(input: HTMLInputElement | null, text: string): void {
  if (input === null) throw new Error("No field to type in.");
  Object.getOwnPropertyDescriptor(HTMLInputElement.prototype, "value")?.set?.call(input, text);
  input.dispatchEvent(new Event("input", { bubbles: true }));
}

describe("an empty view", () => {
  test("says to start by adding a panel, with a button that opens the panel picker, whose pick goes to the default place", async () => {
    const el = document.createElement("div");
    const s = await startWithNotes(el, boardWith(dockStateWith(gridWith([{ plugin: NOTES }]))));
    expect(el.querySelector(".blockr-dock-empty")).toBeNull();

    await s.core.commands.run("blockr.dock-remove_panel", { view: "main", panel: { plugin: NOTES } });
    await s.settled();
    const prompt = el.querySelector(".blockr-dock-empty");
    expect(prompt?.querySelector("p")?.textContent).toBe("Start by adding a panel");

    click(prompt?.querySelector("button"));
    await drawn();
    expect(entries(el)).toEqual([["Dataset", "dataset block"], ["Head", "head/tail block"], ["Edit board"], ["Notes"]]);

    click(entry(el, "Head"));
    await s.settled();
    expect(mainGrid(s.core)).toEqual({ orientation: "horizontal", children: [{ panels: [{ block: "b" }], active: { block: "b" } }], sizes: [1] });
    expect(el.querySelector(".blockr-dock-empty")).toBeNull();
    s.stop();
  });

  test('says it is empty under the board lock, where the dock draws no "+" and no picker', async () => {
    const el = document.createElement("div");
    const s = await startWithNotes(el, boardWith(dockStateWith(EMPTY_GRID)), true);

    expect(el.querySelector(".blockr-dock-empty p")?.textContent).toBe("This view is empty");
    expect(el.querySelector(".blockr-dock-empty button")).toBeNull();
    expect(el.querySelector(".blockr-dock-menu")).toBeNull();
    s.stop();

    const shown = document.createElement("div");
    const t = await startWithNotes(shown, boardWith(dockStateWith(gridWith([{ block: "a" }]))), true);
    expect(tabs(shown)).toEqual(["Dataset"]);
    expect(shown.querySelector(".blockr-dock-add")).toBeNull();
    expect(shown.querySelector(".blockr-dock-empty")).toBeNull();
    t.stop();
  });
});

// Whether dockview shows the frame of the block's panel or the plugin's,
// which it hides behind another tab and on a collapsed rail.
function showsFrame(el: HTMLElement, panel: Panel): boolean {
  const frame = el.querySelector<HTMLElement>(
    "block" in panel ? `.blockr-dock-panel[data-block="${panel.block}"]` : `.blockr-dock-panel[data-plugin="${panel.plugin}"]`,
  );
  return frame !== null && frame.closest<HTMLElement>(".dv-render-overlay")?.style.visibility !== "hidden";
}

describe("showing a block or a plugin", () => {
  test("brings forward the panel of a block that the active view holds, which takes focus, without an undo step", async () => {
    const el = document.createElement("div");
    const s = await startWithNotes(el, boardWith(dockStateWith(gridWith([{ block: "a" }, { block: "b" }]))));
    expect(showsFrame(el, { block: "a" })).toBe(false);

    await s.core.show({ block: "a" });
    expect(mainGrid(s.core)?.children).toEqual([{ panels: [{ block: "a" }, { block: "b" }], active: { block: "a" } }]);
    await s.settled();
    expect(showsFrame(el, { block: "a" })).toBe(true);
    expect(s.core.focus.get()).toBe("a");
    expect(s.core.store.canUndo()).toBe(false);
    s.stop();
  });

  test("adds the panel of a plugin that the active view lacks at the default place, and switches no view", async () => {
    const el = document.createElement("div");
    const s = await startWithNotes(el, boardWith(dockStateWith(gridWith([{ block: "a" }]), gridWith([{ plugin: NOTES }]))));

    await s.core.show({ plugin: NOTES });
    expect(mainGrid(s.core)?.children).toEqual([{ panels: [{ block: "a" }, { plugin: NOTES }], active: { plugin: NOTES } }]);
    expect((s.core.store.board().plugins[DOCK] as DockState).active).toBe("main");
    await s.settled();
    expect(showsFrame(el, { plugin: NOTES })).toBe(true);
    expect(s.notes.renders).toHaveLength(1);
    expect(s.core.store.canUndo()).toBe(false);
    s.stop();
  });

  test("opens a collapsed rail that holds the panel", async () => {
    const el = document.createElement("div");
    const s = await startWithNotes(el, boardWith(dockStateWith(notesOnRail(true))));
    expect(showsFrame(el, { plugin: NOTES })).toBe(false);

    await s.core.show({ plugin: NOTES });
    expect(mainGrid(s.core)?.rails?.left?.collapsed).toBe(false);
    await s.settled();
    expect(showsFrame(el, { plugin: NOTES })).toBe(true);
    s.stop();
  });

  test("on a locked board, adds no panel, while the block still takes focus", async () => {
    const el = document.createElement("div");
    const s = await startWithNotes(el, boardWith(dockStateWith(gridWith([{ block: "a" }]))), true);

    await s.core.show({ block: "b" });
    await s.core.show({ plugin: NOTES });
    expect(mainGrid(s.core)).toEqual(gridWith([{ block: "a" }]));
    expect(s.core.focus.get()).toBe("b");
    expect(s.core.notifications.get()).toEqual([]);
    s.stop();
  });

  test("leaves the keyboard's focus where it is", async () => {
    const el = attached();
    const s = await startWithNotes(el, boardWith(dockStateWith({ ...notesOnRail(true), ...gridWith([{ block: "a" }, { block: "b" }]) })));
    const input = attached().appendChild(document.createElement("input"));
    input.focus();

    await s.core.show({ block: "a" });
    await s.core.show({ plugin: NOTES });
    await s.core.show({ plugin: EDIT });
    expect(mainGrid(s.core)).toMatchObject({ children: [{ active: { plugin: EDIT } }], rails: { left: { collapsed: false } } });
    expect(document.activeElement).toBe(input);
    s.stop();
  });
});

// Resizes happy-dom's window, which tells its listeners as a browser does.
function setWindowWidth(width: number): void {
  (window as unknown as { readonly happyDOM: { setInnerWidth(width: number): void } }).happyDOM.setInnerWidth(width);
}

describe("what is on screen", () => {
  test("is the blocks and the plugins whose panels the active view shows, as the dock draws and as the view or its grid changes", async () => {
    const el = document.createElement("div");
    const main = { ...notesOnRail(false), ...gridWith([{ block: "a" }, { block: "b" }]) };
    const s = await startWithNotes(el, boardWith(dockStateWith(main, gridWith([{ block: "a" }, { plugin: EDIT }]))));
    expect(s.core.onScreen.get()).toEqual({ blocks: ["b"], plugins: [NOTES] });

    await s.core.commands.run("blockr.dock-activate_panel", { view: "main", panel: { block: "a" } });
    expect(s.core.onScreen.get()).toEqual({ blocks: ["a"], plugins: [NOTES] });

    await s.core.commands.run("blockr.dock-collapse_rail", { view: "main", edge: "left", collapsed: true });
    expect(s.core.onScreen.get()).toEqual({ blocks: ["a"], plugins: [] });

    await s.core.commands.run("blockr.dock-activate_view", { view: "side" });
    expect(s.core.onScreen.get()).toEqual({ blocks: [], plugins: [EDIT] });
    s.stop();
  });

  test("holds the panel of a collapsed rail too while the window is narrower than narrow_breakpoint", async () => {
    const width = window.innerWidth;
    onTestFinished(() => {
      setWindowWidth(width);
    });
    const grid: Grid = { ...gridWith([{ block: "a" }]), rails: { left: { panels: [{ plugin: EDIT }], active: { plugin: EDIT }, collapsed: true, size: 260 } } };
    const s = await startDock(document.createElement("div"), { narrow_breakpoint: width - 100 }, boardWith(dockStateWith(grid)));
    await s.settled();
    expect(s.core.onScreen.get()).toEqual({ blocks: ["a"], plugins: [] });

    setWindowWidth(width - 200);
    expect(s.core.onScreen.get()).toEqual({ blocks: ["a"], plugins: [EDIT] });

    setWindowWidth(width);
    expect(s.core.onScreen.get()).toEqual({ blocks: ["a"], plugins: [] });
    s.stop();
  });
});

// A stylesheet without its comments, by its path from this file.
// Vite rewrites `new URL()` of a string literal into the URL that its server
// serves the file at, so the path comes in as an argument.
const read = (path: string) => readFileSync(fileURLToPath(new URL(path, import.meta.url)), "utf8").replace(/\/\*[\s\S]*?\*\//g, "");

// The colour variables that dockview's light-spaced theme sets otherwise than
// its dark abyss-spaced theme, which would show a light value under the dark
// theme unless the bridge sets them, as blockr.dock#502 asks a test to catch.
describe("the bridge from blockr.ui's tokens to dockview's theme", () => {
  const dockview = read("../../../node_modules/dockview-react/dist/styles/dockview.css");
  // The dock's stylesheet with the card's, which it imports.
  const own = read("./index.css") + read("./card/card.css");

  // The custom properties that the rules of a selector set, each with its
  // last value, as the cascade gives it for rules of equal specificity.
  function properties(css: string, selector: string): Map<string, string> {
    const found = new Map<string, string>();
    for (const [, selectors = "", body = ""] of css.matchAll(/([^{}]+)\{([^{}]*)\}/g)) {
      const names = selectors.split(",").map((name) => name.trim());
      if (!names.includes(selector)) continue;
      for (const [, name = "", value = ""] of body.matchAll(/(--[\w-]+)\s*:\s*([^;]+);/g)) found.set(name, value.replace(/\s+/g, " ").trim());
    }
    return found;
  }

  const light = properties(dockview, ".dockview-theme-light-spaced");
  const dark = properties(dockview, ".dockview-theme-abyss-spaced");
  const bridged = properties(own, ".blockr-dock .dv-shell");
  const darkBridged = properties(own, '[data-bc-theme="dark"] .blockr-dock .dv-shell');

  // What the dock does not use: floating groups, which it does not offer,
  // and the palette of the abyss themes, which no variable of the
  // light-spaced theme reads.
  const unused = (name: string) => name.startsWith("--dv-floating-") || name.startsWith("--dv-color-abyss");

  test("sets under the dark theme each colour of the light-spaced theme that the dark theme sets otherwise", () => {
    const differing = [...new Set([...light.keys(), ...dark.keys()])].filter((name) => light.get(name) !== dark.get(name) && !unused(name));
    expect(differing.length).toBeGreaterThan(0);
    expect(differing.filter((name) => !darkBridged.has(name))).toEqual([]);
  });

  test("reads each token with a fallback, its light value under the light theme and its dark value under the dark theme", () => {
    for (const [name, value] of [...bridged, ...darkBridged]) {
      for (const [, token = "", fallback] of value.matchAll(/var\(\s*(--blockr-[\w-]+)\s*(?:,\s*([^)]+))?/g)) {
        expect(fallback, `${name} reads ${token}`).toBeDefined();
      }
    }
    for (const name of bridged.keys()) expect(darkBridged.has(name), name).toBe(true);
    expect(bridged.get("--dv-group-view-background-color")).toBe("var(--blockr-color-bg-page, #f9fafb)");
    expect(darkBridged.get("--dv-group-view-background-color")).toBe("var(--blockr-color-bg-page, #0e1219)");
  });

  // The variables that the dock's own pieces read, the "+" of a tab group, the
  // prompt of an empty view, the panel picker, the bar with its menus and the
  // options sidebar, under each theme.
  const pieces = properties(own, ".blockr-dock");
  const darkPieces = properties(own, '[data-bc-theme="dark"] .blockr-dock');

  test("gives the dock's own pieces each token with a fallback, its light value under the light theme and its dark value under the dark theme", () => {
    expect(pieces.size).toBeGreaterThan(0);
    expect([...darkPieces.keys()]).toEqual([...pieces.keys()]);
    for (const [name, value] of [...pieces, ...darkPieces]) expect(value, name).toMatch(/^var\(\s*--blockr-[\w-]+\s*,/);
    expect(pieces.get("--blockr-dock-bg-raised")).toBe("var(--blockr-color-bg-raised, #ffffff)");
    expect(darkPieces.get("--blockr-dock-bg-raised")).toBe("var(--blockr-color-bg-raised, #1e242e)");
    // The bar and the options sidebar sit beside the dock's root, and take the
    // same variables.
    expect(properties(own, ".blockr-dock-bar")).toEqual(pieces);
    expect(properties(own, '[data-bc-theme="dark"] .blockr-dock-bar')).toEqual(darkPieces);
    expect(properties(own, ".blockr-dock-sidebar")).toEqual(pieces);
    expect(properties(own, '[data-bc-theme="dark"] .blockr-dock-sidebar')).toEqual(darkPieces);

    // A rule reads no variable of the dock's that neither theme sets, and every
    // token that it reads itself, as a radius, with a fallback.
    const used = [...own.matchAll(/var\((--blockr-dock-[\w-]+)/g)].map(([, name = ""]) => name);
    expect(used.filter((name) => !pieces.has(name))).toEqual([]);
    for (const [, token = "", fallback] of own.matchAll(/var\(\s*(--(?:blockr|bc)-(?!dock-)[\w-]+)\s*(?:,\s*([^)]+))?/g)) {
      expect(fallback, token).toBeDefined();
    }
  });
});

// The toasts that the page shows, in order.
const toastsIn = (el: ParentNode): HTMLElement[] => [
  ...el.querySelectorAll<HTMLElement>('section[aria-label="Notifications"] > [data-severity]'),
];

// The severity, the first line and the message of each toast that the page
// shows, in order.
function toasts(el: ParentNode): [string | null, string, string][] {
  return toastsIn(el).map((toast) => {
    const [title, message] = [...toast.querySelectorAll("p")].map((p) => p.textContent);
    return [toast.getAttribute("data-severity"), title ?? "", message ?? ""];
  });
}

// The messages of the toasts that the page shows, in order.
const messages = (el: ParentNode): string[] => toasts(el).map(([, , message]) => message);

// The toast whose message is the one given.
function toastWithMessage(el: ParentNode, message: string): HTMLElement {
  const found = toastsIn(el).find((toast) => toast.querySelectorAll("p")[1]?.textContent === message);
  if (found === undefined) throw new Error(`Found no toast "${message}".`);
  return found;
}

// The button that closes the toast.
function closeButton(toast: HTMLElement): HTMLButtonElement {
  const button = toast.querySelector<HTMLButtonElement>('button[aria-label="Close"]');
  if (button === null) throw new Error("The toast has no button that closes it.");
  return button;
}

describe("the toasts", () => {
  test("show each notification that no card shows, whoever raised it, until core withdraws it or the user closes it", async () => {
    const el = document.createElement("div");
    // R raises a warning in each block's evaluation, which the block's card
    // shows.
    const warned = () => ({
      conditions: [{ severity: "warning" as const, class: ["warning", "condition"], phase: "block_eval" as const, message: "Few rows." }],
    });
    const { core, settled, stop } = await stubCore({ hello, modules, el, evaluate: warned });
    await settled();
    expect(core.notifications.get().some((n) => n.source.kind === "r" && n.message === "Few rows.")).toBe(true);

    let crowded: () => void = () => undefined;
    flushSync(() => {
      core.notifications.notify(DOCK, { severity: "message", code: "saved", message: "Saved." });
      // What a plugin raises about a block shows on the block's card.
      core.notifications.notify(DOCK, { severity: "error", code: "not_placed", message: "Not placed.", about: { block: "b" } });
      crowded = core.notifications.notify(DOCK, { severity: "warning", code: "crowded", message: "The view is crowded.", about: null });
      core.notifications.notify(DOCK, { severity: "error", code: "not_shared", message: "Not shared.", about: null });
    });
    expect(toasts(el)).toEqual([
      ["message", 'Plugin "Dock" · blockr.dock', "Saved."],
      ["warning", "Board · blockr.dock", "The view is crowded."],
      ["error", "Board · blockr.dock", "Not shared."],
    ]);
    expect(toastsIn(el).map((toast) => toast.getAttribute("role"))).toEqual(["status", "alert", "alert"]);
    // The toasts sit beside the dock's root, outside it.
    expect(el.querySelector(".blockr-dock .blockr-dock-toasts")).toBeNull();

    flushSync(() => {
      closeButton(toastWithMessage(el, "Not shared.")).click();
    });
    expect(messages(el)).toEqual(["Saved.", "The view is crowded."]);
    expect(core.notifications.get().some((n) => n.message === "Not shared.")).toBe(true);
    flushSync(() => {
      crowded();
    });
    expect(messages(el)).toEqual(["Saved."]);
    stop();
  });

  test("show a notification about a plugin whose panel the active view does not hold, whose link makes the first view that holds the panel the active one", async () => {
    const el = document.createElement("div");
    // The view side holds the notes plugin's panel, behind b's, and main,
    // which is active, holds none.
    const s = await startWithNotes(el, boardWith(dockStateWith(gridWith([{ block: "a" }]), gridWith([{ plugin: NOTES }, { block: "b" }]))));

    flushSync(() => {
      s.core.notifications.notify(NOTES, lost);
      s.core.notifications.notify(NOTES, saved);
    });
    expect(toasts(el)).toEqual([
      ["error", 'Plugin "Notes" · blockr.test', "Lost the draft."],
      ["message", 'Plugin "Notes" · blockr.test', "Saved."],
    ]);
    const link = toastWithMessage(el, "Lost the draft.").querySelector("button.blockr-dock-toast-link");
    expect(link?.textContent).toBe("Show panel");
    expect(s.core.store.canUndo()).toBe(false);

    click(link);
    await s.settled();
    const state = s.core.store.board().plugins[DOCK] as DockState;
    expect(state.active).toBe("side");
    expect(state.views.side?.grid.children[0]).toMatchObject({ active: { plugin: NOTES } });
    expect(s.core.store.canUndo()).toBe(false);
    // The toasts go, and the dot on the panel's tab tells of the error.
    expect(toasts(el)).toEqual([]);
    expect(dotOn(el, "Notes")).toBe("error");

    // Back on main, the toasts come back.
    await s.core.commands.run("blockr.dock-activate_view", { view: "main" });
    await s.settled();
    expect(messages(el)).toEqual(["Lost the draft.", "Saved."]);
    s.stop();
  });

  test("show the panel in the active view where no view holds it, at the default place, through the link", async () => {
    const el = document.createElement("div");
    const s = await startWithNotes(el, boardWith(dockStateWith(gridWith([{ block: "a" }]), gridWith([{ block: "b" }]))));

    flushSync(() => {
      s.core.notifications.notify(NOTES, lost);
    });
    click(toastWithMessage(el, "Lost the draft.").querySelector("button.blockr-dock-toast-link"));
    await s.settled();

    expect(mainGrid(s.core)).toMatchObject({ children: [{ panels: [{ block: "a" }, { plugin: NOTES }], active: { plugin: NOTES } }] });
    expect(toasts(el)).toEqual([]);
    expect(dotOn(el, "Notes")).toBe("error");
    s.stop();
  });

  test("give no link on a locked board where no view holds the panel, since a locked dock adds none, and one where a view holds it", async () => {
    const el = document.createElement("div");
    const s = await startWithNotes(el, boardWith(dockStateWith(gridWith([{ block: "a" }]))), true);
    flushSync(() => {
      s.core.notifications.notify(NOTES, lost);
    });
    expect(messages(el)).toEqual(["Lost the draft."]);
    expect(toastWithMessage(el, "Lost the draft.").querySelector("button.blockr-dock-toast-link")).toBeNull();
    s.stop();

    const other = document.createElement("div");
    const t = await startWithNotes(other, boardWith(dockStateWith(gridWith([{ block: "a" }]), gridWith([{ plugin: NOTES }]))), true);
    flushSync(() => {
      t.core.notifications.notify(NOTES, lost);
    });
    click(toastWithMessage(other, "Lost the draft.").querySelector("button.blockr-dock-toast-link"));
    await t.settled();
    expect((t.core.store.board().plugins[DOCK] as DockState).active).toBe("side");
    expect(toasts(other)).toEqual([]);
    t.stop();
  });

  test("show what the store dropped as the board opened, which core raised before the dock drew", async () => {
    const board = rBoard();
    const gone = { registry_id: "blockr.gone-gone_block", version: 1, state: {}, name: "Gone", locked: null };
    const el = document.createElement("div");
    const { settled, stop } = await startDock(el, {}, { ...board, board: { ...board.board, blocks: { ...board.board.blocks, x: gone } } });
    await settled();

    expect(toasts(el)).toEqual([["warning", "Board", expect.stringMatching(/opened without[\s\S]*blockr\.gone-gone_block/)]]);
    stop();
  });

  test("show a message as text, never as HTML, and without the escapes of cli's styles", async () => {
    const el = document.createElement("div");
    const { core, settled, stop } = await startDock(el);
    await settled();

    flushSync(() => {
      core.notifications.notify(DOCK, { severity: "error", code: "failed", message: "<b>Not</b> \u001b[1msaved\u001b[22m." });
    });
    expect(messages(el)).toEqual(["<b>Not</b> saved."]);
    expect(toastWithMessage(el, "<b>Not</b> saved.").querySelector("b")).toBeNull();
    stop();
  });

  test("fade from a message's toast after five seconds, but not while the pointer rests on it or the keyboard's focus is in it", async () => {
    const el = document.createElement("div");
    // The button of a toast takes the focus only in the document.
    document.body.append(el);
    const { core, settled, stop } = await startDock(el);
    await settled();
    vi.useFakeTimers({ toFake: ["setTimeout", "clearTimeout"] });
    try {
      flushSync(() => {
        core.notifications.notify(DOCK, { severity: "message", code: "saved", message: "Saved." });
        core.notifications.notify(DOCK, { severity: "error", code: "share_failed", message: "Not shared." });
      });
      const saved = toastWithMessage(el, "Saved.");
      flushSync(() => {
        saved.dispatchEvent(new PointerEvent("pointerover", { bubbles: true }));
        vi.advanceTimersByTime(60_000);
      });
      flushSync(() => {
        closeButton(saved).focus();
        saved.dispatchEvent(new PointerEvent("pointerout", { bubbles: true }));
        vi.advanceTimersByTime(60_000);
      });
      expect(messages(el)).toEqual(["Saved.", "Not shared."]);

      flushSync(() => {
        closeButton(saved).blur();
        vi.advanceTimersByTime(SHOWN_FOR);
      });
      expect(toastWithMessage(el, "Saved.").hasAttribute("data-fading")).toBe(true);
      flushSync(() => {
        vi.advanceTimersByTime(FADES_FOR);
      });
      expect(messages(el)).toEqual(["Not shared."]);
    } finally {
      vi.useRealTimers();
      stop();
      el.remove();
    }
  });

  test("go with what the frontend drew in its cleanup, which leaves the notifications unfollowed", async () => {
    const s = await startDock();
    const el = document.createElement("div");
    const cleanup = await dock.default.frontend.render({
      el,
      core: s.core,
      plugin: DOCK,
      signal: new AbortController().signal,
      args: {},
    });
    flushSync(() => {
      s.core.notifications.notify(DOCK, { severity: "error", code: "failed", message: "Failed." });
    });
    expect(messages(el)).toEqual(["Failed."]);

    cleanup?.();
    expect(el.childElementCount).toBe(0);
    flushSync(() => {
      s.core.notifications.notify(DOCK, { severity: "error", code: "failed", message: "Failed again." });
    });
    expect(el.childElementCount).toBe(0);
    s.stop();
  });
});

describe("the stylesheet of the toasts", () => {
  // The declarations of each rule of the toasts, by the rule's selector.
  const rules = new Map(
    [...read("./index.css").matchAll(/([^{}]+)\{([^{}]*)\}/g)]
      .map(([, selector = "", body = ""]) => [selector.trim(), declarations(body)] as const)
      .filter(([selector]) => selector.includes(".blockr-dock-toast")),
  );
  const dark = '[data-bc-theme="dark"] ';

  // Whether a value reads one of blockr.ui's tokens whose value differs
  // between the light scheme and the dark scheme: a colour, a shadow or the
  // focus outline.
  const themed = (value: string) => /var\(\s*--blockr-(?:color|shadow|focus-outline)/.test(value);

  test("reads each token with a fallback", () => {
    expect(rules.size).toBeGreaterThan(0);
    for (const [selector, declared] of rules) {
      for (const [name, value] of declared) {
        for (const [, token = "", fallback] of value.matchAll(/var\(\s*(--blockr-[\w-]+)\s*(?:,\s*([^)]+))?/g)) {
          expect(fallback, `${selector} { ${name} } reads ${token}`).toBeDefined();
        }
      }
    }
  });

  test("restates under the dark theme each value that reads a token of the light scheme, with the token's dark value as the fallback", () => {
    const light = [...rules].filter(([selector]) => !selector.startsWith(dark));
    expect(light.length).toBeGreaterThan(0);
    for (const [selector, declared] of light) {
      for (const [name, value] of declared) {
        if (!themed(value)) continue;
        const restated = rules.get(`${dark}${selector}`)?.get(name);
        expect(restated, `${selector} { ${name} }`).toBeDefined();
        expect(restated, `${selector} { ${name} }`).not.toBe(value);
      }
    }
    expect(rules.get('.blockr-dock-toast[data-severity="error"]')?.get("color")).toBe("var(--blockr-color-text-danger, #b91c1c)");
    expect(rules.get(`${dark}.blockr-dock-toast[data-severity="error"]`)?.get("color")).toBe("var(--blockr-color-text-danger, #f87171)");
  });
});

describe("the stylesheet of the dot on a plugin's tab", () => {
  // The declarations of each rule, by the rule's selector, on one line.
  const rules = new Map(
    [...read("./index.css").matchAll(/([^{}]+)\{([^{}]*)\}/g)].map(
      ([, selector = "", body = ""]) => [selector.trim().replace(/\s+/g, " "), declarations(body)] as const,
    ),
  );
  const dot = ".dv-default-tab-content::after";

  test("draws the dot in the dock's danger colour for an error and in its amber for a warning", () => {
    expect(rules.get(`.blockr-dock-tab-frame[data-notified] ${dot}`)?.get("background-color")).toBe("var(--blockr-dock-border-danger)");
    expect(rules.get(`.blockr-dock-tab-frame[data-notified="warning"] ${dot}`)?.get("background-color")).toBe(
      "var(--blockr-dock-border-warning)",
    );
  });

  test("takes the amber from blockr.ui's token, with its value in each scheme as the fallback", () => {
    const light = rules.get(".blockr-dock, .blockr-dock-bar, .blockr-dock-sidebar");
    const dark = rules.get('[data-bc-theme="dark"] .blockr-dock, [data-bc-theme="dark"] .blockr-dock-bar, [data-bc-theme="dark"] .blockr-dock-sidebar');
    expect(light?.get("--blockr-dock-border-warning")).toBe("var(--blockr-color-border-warning, #d97706)");
    expect(dark?.get("--blockr-dock-border-warning")).toBe("var(--blockr-color-border-warning, #fbbf24)");
    expect(light?.get("--blockr-dock-border-danger")).toBe("var(--blockr-color-border-danger, #dc2626)");
    expect(dark?.get("--blockr-dock-border-danger")).toBe("var(--blockr-color-border-danger, #f87171)");
  });
});

// The declarations of a rule's body, each by its property, with its value.
function declarations(body: string): Map<string, string> {
  return new Map(
    [...body.matchAll(/([\w-]+)\s*:\s*([^;]+);/g)].map(([, name = "", value = ""]) => [name, value.replace(/\s+/g, " ").trim()]),
  );
}
