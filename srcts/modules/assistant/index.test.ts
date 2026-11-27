// The module `assistant`, whose describe() tells blockr.assistant's model
// about the dock's views and the active view.

import { id } from "@blockr/core/stubs";
import { describe, expect, test } from "vitest";
import { DOCK } from "../../commands/index.ts";
import { defaultState, RAIL_SIZE } from "../../state/index.ts";
import type { DockState, FolderId, Grid, Panel, Rail, TabGroup, View } from "../../state/index.ts";
import assistant from "./index.ts";

// The Id of the dock plugin's class, made of the commands' DOCK, as the
// assistant passes it to describe().
const dock = id(DOCK.package, DOCK.id);

const ASSISTANT = { plugin: { package: "blockr.assistant", id: "assistant_plugin" } };
const EDIT = { plugin: { package: "blockr.core", id: "edit_board_plugin" } };

// The lines of the views of BOARD, each plugin's panel by the JSON of its
// class.
const ANALYSIS =
  '- Analysis (id: v1): iris, head, plot, {"package":"blockr.assistant","id":"assistant_plugin"}, {"package":"blockr.core","id":"edit_board_plugin"}';
const OVERVIEW = "- Overview (id: v2): empty";

function group(...panels: Panel[]): TabGroup {
  const [first] = panels;
  if (first === undefined) throw new Error("A tab group holds a panel.");
  return { panels, active: first };
}

function rail(...panels: Panel[]): Rail {
  return { ...group(...panels), collapsed: false, size: RAIL_SIZE };
}

const EMPTY: Grid = { orientation: "horizontal", children: [], sizes: [] };

function view(name: string, index: string, grid: Grid = EMPTY, folder: FolderId | null = null): View {
  return { name, folder, index, grid };
}

// The board of the issue: Analysis, the active view, shows iris and head in
// one tab group in its left half and plot in its right half.
// The assistant sits on a rail at its left edge, and core's Edit board plugin
// on a rail at its right edge.
// Overview, after it in the navigation, shows nothing.
const BOARD: DockState = {
  ...defaultState,
  views: {
    v2: view("Overview", "a1"),
    v1: view("Analysis", "a0", {
      orientation: "horizontal",
      children: [group({ block: "iris" }, { block: "head" }), group({ block: "plot" })],
      sizes: [0.5, 0.5],
      rails: { right: rail(EDIT), left: rail(ASSISTANT) },
    }),
  },
  active: "v1",
};

describe("describe()", () => {
  test("names the active view, then each view with the panels that its grid places", () => {
    expect(assistant.describe(dock, BOARD)).toEqual(["The active view is Analysis (id: v1).", ANALYSIS, OVERVIEW]);
  });

  test("names a plugin's panel by the same JSON whether the state holds the record or the Id of its class", () => {
    const rails = { right: rail({ plugin: id("blockr.core", "edit_board_plugin") }), left: rail(ASSISTANT) };
    const v1 = BOARD.views.v1;
    if (v1 === undefined) throw new Error("BOARD holds the view v1.");
    const state: DockState = { ...BOARD, views: { ...BOARD.views, v1: { ...v1, grid: { ...v1.grid, rails } } } };
    expect(assistant.describe(dock, state)).toEqual(["The active view is Analysis (id: v1).", ANALYSIS, OVERVIEW]);
  });

  test("gives the views in the order of the navigation, with the views of a folder in its place", () => {
    const state: DockState = {
      ...defaultState,
      views: {
        listing: view("Listing", "a2"),
        ae: view("Adverse events", "a0", { orientation: "horizontal", children: [group({ block: "ae" })], sizes: [1] }, "safety"),
        overview: view("Overview", "a0"),
      },
      folders: { safety: { name: "Safety", folder: null, index: "a1" } },
      active: "listing",
    };
    expect(assistant.describe(dock, state)).toEqual([
      "The active view is Listing (id: listing).",
      "- Overview (id: overview): empty",
      "- Adverse events (id: ae): ae",
      "- Listing (id: listing): empty",
    ]);
  });

  test("names no active view where `active` names none of the views", () => {
    const lines = [ANALYSIS, OVERVIEW];
    expect(assistant.describe(dock, { ...BOARD, active: null })).toEqual(lines);
    expect(assistant.describe(dock, { ...BOARD, active: "gone" })).toEqual(lines);
    expect(assistant.describe(dock, { ...BOARD, active: "constructor" })).toEqual(lines);
  });

  test("gives no lines for a state without views, or where the board holds no state for the dock", () => {
    expect(assistant.describe(dock, defaultState)).toEqual([]);
    expect(assistant.describe(dock, null)).toEqual([]);
  });

  test("gives no lines for another plugin class, of the dock's package or of the dock class's id in another package", () => {
    expect(assistant.describe(id("blockr.core", "edit_board_plugin"), BOARD)).toEqual([]);
    expect(assistant.describe(id("blockr.dock", "other_plugin"), BOARD)).toEqual([]);
    expect(assistant.describe(id("blockr.test", "dock_plugin"), BOARD)).toEqual([]);
  });

  test("throws where the state does not have the shape of the dock's state", () => {
    expect(() => assistant.describe(dock, { ...BOARD, views: [] })).toThrow("The dock's state does not have the shape that its schema gives.");
    expect(() => assistant.describe(dock, "views")).toThrow("The dock's state does not have the shape that its schema gives.");
  });
});
