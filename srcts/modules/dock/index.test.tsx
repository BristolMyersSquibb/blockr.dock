// The dock's frontend on blockr.core's CoreApi for tests, which starts core
// as the page of an app that runs the dock would, with R played from the
// fixtures of core's wire protocol, so that the tests need neither R nor a
// browser (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#testing).

import type { Json, JsonBoard } from "@blockr/core";
import { HELLO, stubCore } from "@blockr/core/stubs";
import { describe, expect, test } from "vitest";
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

  test("draws the page as an empty dockview, in the page's element", async () => {
    const el = document.createElement("div");
    const { settled, stop } = await startDock(el);
    await settled();

    const shell = el.querySelector(".blockr-dock .dv-shell");
    expect(shell?.classList).toContain("dockview-theme-light-spaced");
    expect(el.querySelectorAll(".dv-groupview")).toHaveLength(0);
    stop();
  });

  test("takes dockview's dark theme while dark_mode is dark", async () => {
    const el = document.createElement("div");
    const { core, settled, stop } = await startDock(el);
    await settled();
    const shell = () => el.querySelector(".blockr-dock .dv-shell");

    await core.commands.run("blockr.core-set_option", { id: "dark_mode", value: "dark" });
    await settled();
    expect(shell()?.classList).toContain("dockview-theme-abyss-spaced");
    expect(shell()?.classList).not.toContain("dockview-theme-light-spaced");

    await core.commands.run("blockr.core-set_option", { id: "dark_mode", value: "light" });
    await settled();
    expect(shell()?.classList).toContain("dockview-theme-light-spaced");
    stop();
  });

  test("removes what it drew in its cleanup", async () => {
    const s = await startDock();
    const el = document.createElement("div");
    const cleanup = dock.default.frontend.render({
      el,
      core: s.core,
      plugin: DOCK,
      signal: new AbortController().signal,
      args: {},
    });
    expect(el.querySelector(".blockr-dock")).not.toBeNull();

    cleanup();
    expect(el.childElementCount).toBe(0);
    s.stop();
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
