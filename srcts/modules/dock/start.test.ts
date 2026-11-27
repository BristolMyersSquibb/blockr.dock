// The step that the dock takes as it starts on a board, what it demands and
// what it has on screen.

import type { Block, Board, Patch } from "@blockr/core";
import { id } from "@blockr/core/stubs";
import { describe, expect, test } from "vitest";
import { patched, RAIL_SIZE } from "../../state/index.ts";
import type { DockState, Grid, Panel, TabGroup, View } from "../../state/index.ts";
import { onScreen } from "./demand.ts";
import { PAGE_ID, startingPatches, urlView } from "./start.ts";

const DOCK = id("blockr.dock", "dock_plugin");

// The class of core's Edit board plugin, by the Id that core makes of it, and
// as the dock's state holds it, a plain record, which is no object of core's.
const EDIT = id("blockr.core", "edit_board_plugin");
const EDIT_PANEL: Panel = { plugin: { package: "blockr.core", id: "edit_board_plugin" } };

const head: Block = { registry_id: id("blockr.core", "head_block"), state: {}, name: "Head", locked: null };

function group(...panels: Panel[]): TabGroup {
  const [first] = panels;
  if (first === undefined) throw new Error("A tab group holds a panel.");
  return { panels, active: first };
}

function view(grid: Grid, index = "a0"): View {
  return { name: "View", folder: null, index, grid };
}

function boardWith(state: DockState, blocks: readonly string[] = ["a", "b"]): Board {
  return {
    id: "board",
    name: "Board",
    blocks: Object.fromEntries(blocks.map((id) => [id, { ...head, name: id.toUpperCase() }])),
    links: {},
    stacks: {},
    options: {},
    plugins: { [DOCK.package]: { [DOCK.id]: state } },
  };
}

// The dock's state after the patches.
function after(board: Board, patches: readonly Patch[]): unknown {
  return patched(board.plugins[DOCK.package]?.[DOCK.id] ?? null, patches, DOCK);
}

const empty: DockState = { views: {}, folders: {}, active: null, visible: {} };

const twoViews: DockState = {
  views: {
    main: view({ orientation: "horizontal", children: [group({ block: "a" }, EDIT_PANEL)], sizes: [1] }, "a0"),
    side: view({ orientation: "horizontal", children: [group({ block: "b" })], sizes: [1] }, "a1"),
  },
  folders: {},
  active: "main",
  visible: {},
};

describe("startingPatches()", () => {
  test("adds the view Page where the state holds none, with the plugins on its left rail and a tab group of the blocks", () => {
    const board = boardWith(empty);
    const patches = startingPatches(board, [EDIT], undefined);
    expect(after(board, patches)).toEqual({
      ...empty,
      views: {
        [PAGE_ID]: {
          name: "Page",
          folder: null,
          index: "a0",
          grid: {
            orientation: "horizontal",
            children: [group({ block: "a" }, { block: "b" })],
            sizes: [1],
            rails: { left: { ...group(EDIT_PANEL), collapsed: false, size: RAIL_SIZE } },
          },
        },
      },
      active: PAGE_ID,
    });
  });

  test("gives the view Page no rail where no plugin has a UI, and no tab group where the board holds no block", () => {
    const board = boardWith(empty, []);
    expect(after(board, startingPatches(board, [], undefined))).toMatchObject({
      views: { [PAGE_ID]: { grid: { orientation: "horizontal", children: [], sizes: [] } } },
      active: PAGE_ID,
    });
  });

  test("places the view Page after the folders that a state without views holds", () => {
    const board = boardWith({ ...empty, folders: { f: { name: "F", folder: null, index: "a0" } } });
    expect(after(board, startingPatches(board, [], undefined))).toMatchObject({ views: { [PAGE_ID]: { index: "a1" } } });
  });

  test("removes from every view the panels of plugins that the dock cannot draw, by the package and the id of each one's class", () => {
    const board = boardWith(twoViews);
    expect(after(board, startingPatches(board, [], undefined))).toEqual({
      ...twoViews,
      views: { ...twoViews.views, main: view({ orientation: "horizontal", children: [group({ block: "a" })], sizes: [1] }, "a0") },
    });
    expect(startingPatches(board, [EDIT], undefined)).toEqual([]);
  });

  test("makes active the view that the URL names", () => {
    const board = boardWith(twoViews);
    expect(after(board, startingPatches(board, [EDIT], "side"))).toMatchObject({ active: "side" });
    expect(startingPatches(board, [EDIT], "main")).toEqual([]);
  });

  test("changes nothing for a URL that names no view", () => {
    expect(startingPatches(boardWith(twoViews), [EDIT], "gone")).toEqual([]);
  });

  test("makes the view Page active for a URL that names it", () => {
    const board = boardWith(empty);
    expect(after(board, startingPatches(board, [], PAGE_ID))).toMatchObject({ active: PAGE_ID });
  });

  test("gives no patches on a board that holds no dock state", () => {
    const board = { ...boardWith(empty), plugins: {} };
    expect(startingPatches(board, [], undefined)).toEqual([]);
  });
});

describe("urlView()", () => {
  test("reads the query parameter view", () => {
    expect(urlView({ search: "?view=side&x=1" })).toBe("side");
    expect(urlView({ search: "?x=1" })).toBeUndefined();
    expect(urlView({ search: "?view=" })).toBeUndefined();
    expect(urlView(undefined)).toBeUndefined();
  });
});

describe("onScreen()", () => {
  const core = { id };
  const drawn = new Map([[EDIT, "Edit board"]]);
  const rail = { ...group({ block: "c" }, { block: "d" }), collapsed: false, size: 260 };
  const main: Grid = {
    orientation: "horizontal",
    children: [{ panels: [{ block: "a" }, { block: "b" }], active: { block: "b" } }, group(EDIT_PANEL)],
    sizes: [0.5, 0.5],
    rails: { left: rail },
  };
  const state: DockState = { ...twoViews, views: { ...twoViews.views, main: view(main) } };
  const blocks = ["a", "b", "c", "d"];

  test("gives the blocks and the plugins whose panels the tab groups and the open rails of the active view show", () => {
    const shown = onScreen(core, boardWith(state, blocks), drawn, false);
    expect(shown).toEqual({ blocks: ["b", "c"], plugins: [EDIT] });
    // It gives each plugin by core's Id, which core finds the plugin by.
    expect(shown.plugins[0]).toBe(EDIT);
  });

  test("leaves out the panel that a collapsed rail shows, but in the narrow column", () => {
    const collapsed = { ...state, views: { ...state.views, main: view({ ...main, rails: { left: { ...rail, collapsed: true } } }) } };
    expect(onScreen(core, boardWith(collapsed, blocks), drawn, false)).toEqual({ blocks: ["b"], plugins: [EDIT] });
    expect(onScreen(core, boardWith(collapsed, blocks), drawn, true)).toEqual({ blocks: ["b", "c"], plugins: [EDIT] });
  });

  test("gives the blocks and the plugins of the active view alone", () => {
    expect(onScreen(core, boardWith({ ...state, active: "side" }, blocks), drawn, false)).toEqual({ blocks: ["b"], plugins: [] });
  });

  test("gives the first panel of a tab group whose shown panel the dock cannot draw", () => {
    const gone: Panel = { plugin: { package: "blockr.gone", id: "gone_plugin" } };
    const grid: Grid = { orientation: "horizontal", children: [{ panels: [gone, { block: "a" }], active: gone }], sizes: [1] };
    expect(onScreen(core, boardWith({ ...state, views: { main: view(grid) } }, blocks), drawn, false)).toEqual({ blocks: ["a"], plugins: [] });
  });

  test("gives no block and no plugin where no view is active", () => {
    expect(onScreen(core, boardWith(empty), drawn, false)).toEqual({ blocks: [], plugins: [] });
  });
});
