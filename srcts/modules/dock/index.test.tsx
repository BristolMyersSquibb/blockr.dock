// The dock's frontend on blockr.core's CoreApi for tests, which starts core
// as the page of an app that runs the dock would, with R played from the
// fixtures of core's wire protocol, so that the tests need neither R nor a
// browser (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#testing).

import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import type { Json, JsonBoard } from "@blockr/core";
import { HELLO, stubCore } from "@blockr/core/stubs";
import { describe, expect, onTestFinished, test } from "vitest";
import { defaultState } from "../../state/index.ts";
import type { DockState } from "../../state/index.ts";
import * as compactOption from "../compact-option/index.ts";
import * as syncRailsOption from "../sync-rails-option/index.ts";
import * as viewTabsOption from "../view-tabs-option/index.ts";
import * as dock from "./index.tsx";
// The fixture of a board that R writes for a dock app script is a file of the
// R package, which R's suite checks too.
// eslint-disable-next-line no-restricted-imports
import dockFixture from "../../../inst/board/fixtures/dock.json" with { type: "json" };

const DOCK = "blockr.dock-dock_plugin";

// R's reply to hello in an app that runs the dock in place of core's basic
// frontend, beside core's other plugins, with where R serves the module of
// the dock plugin's class, at version 1, and those of the dock's board
// options, beside core's.
const hello = {
  ...HELLO,
  packages: { ...HELLO.packages, "blockr.dock": "0.2.1" },
  plugins: {
    ...Object.fromEntries(
      Object.entries(HELLO.plugins).filter(([id]) => id !== "blockr.core-basic_frontend_plugin"),
    ),
    [DOCK]: { name: "Dock", version: 1, module: "blockr.dock/js/dock.js" },
  },
  options: {
    ...HELLO.options,
    compact: { package: "blockr.dock", module: "blockr.dock/compact-option.js", version: 1 },
    view_tabs: { package: "blockr.dock", module: "blockr.dock/view-tabs-option.js", version: 1 },
    sync_rails: { package: "blockr.dock", module: "blockr.dock/sync-rails-option.js", version: 1 },
  },
};

// What each module of the dock exports, by the id that R's reply names it
// under.
const modules = {
  [DOCK]: dock,
  compact: compactOption,
  view_tabs: viewTabsOption,
  sync_rails: syncRailsOption,
};

// Starts core with the dock's modules, with the values of blockr options that
// R holds, none unless given, where the frontend of the dock plugin's class
// draws the page in `el` where a test gives one, on the board that R's reply
// to hello gives, the fixture's unless one is given.
function startDock(el?: HTMLElement, blockrOptions: Readonly<Record<string, Json>> = {}, board?: JsonBoard) {
  return stubCore({ hello: board === undefined ? hello : { ...hello, board }, modules, blockrOptions, el });
}

describe("the dock's frontend", () => {
  test("is the frontend of the dock plugin's class, which requires the dock's board options and two of core's", async () => {
    const { core, stop } = await startDock();

    const cls = core.plugins.pluginClass(DOCK);
    expect(cls.frontend).toBe(dock.default.frontend);
    expect(cls.options).toEqual(["compact", "view_tabs", "sync_rails", "show_conditions", "dark_mode"]);
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
      versions: { [DOCK]: 1 },
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
      plugins: { [DOCK]: dockState },
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
            collapsed_size: 35,
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

    expect(core.store.board().plugins[DOCK]).toEqual(board.board.plugins[DOCK]);
    expect(core.store.jsonBoard().board.versions).toEqual({ [DOCK]: 1 });
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
    expect(core.store.jsonBoard().board.versions).toEqual({ [DOCK]: 1 });
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
                size: 260,
                collapsed_size: 35,
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

// The colour variables that dockview's light-spaced theme sets otherwise than
// its dark abyss-spaced theme, which would show a light value under the dark
// theme unless the bridge sets them, as blockr.dock#502 asks a test to catch.
describe("the bridge from blockr.ui's tokens to dockview's theme", () => {
  // A stylesheet without its comments.
  const read = (path: string) => readFileSync(fileURLToPath(new URL(path, import.meta.url)), "utf8").replace(/\/\*[\s\S]*?\*\//g, "");
  const dockview = read("../../../node_modules/dockview-react/dist/styles/dockview.css");
  const own = read("./index.css");

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
});
