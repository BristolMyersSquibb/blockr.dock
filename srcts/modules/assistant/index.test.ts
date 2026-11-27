// The module `assistant`, whose describe() tells blockr.assistant's model
// about the dock's views and the active view.

import { describe, expect, test } from "vitest";
import { DOCK } from "../../commands/index.ts";
import { defaultState, RAIL_SIZE } from "../../state/index.ts";
import type { DockState, FolderId, Grid, Panel, Rail, TabGroup, View } from "../../state/index.ts";
import assistant from "./index.ts";

const ASSISTANT = { plugin: "blockr.assistant-assistant_plugin" };
const DAG = { plugin: "blockr.dag-dag_plugin" };

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
// The assistant sits on a rail at its left edge, and the DAG on a rail at its
// right edge.
// Overview, after it in the navigation, shows nothing.
const BOARD: DockState = {
  ...defaultState,
  views: {
    v2: view("Overview", "a1"),
    v1: view("Analysis", "a0", {
      orientation: "horizontal",
      children: [group({ block: "iris" }, { block: "head" }), group({ block: "plot" })],
      sizes: [0.5, 0.5],
      rails: { right: rail(DAG), left: rail(ASSISTANT) },
    }),
  },
  active: "v1",
};

describe("describe()", () => {
  test("names the active view, then each view with the panels that its grid places", () => {
    expect(assistant.describe(DOCK, BOARD)).toEqual([
      "The active view is Analysis (id: v1).",
      "- Analysis (id: v1): iris, head, plot, blockr.assistant-assistant_plugin, blockr.dag-dag_plugin",
      "- Overview (id: v2): empty",
    ]);
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
    expect(assistant.describe(DOCK, state)).toEqual([
      "The active view is Listing (id: listing).",
      "- Overview (id: overview): empty",
      "- Adverse events (id: ae): ae",
      "- Listing (id: listing): empty",
    ]);
  });

  test("names no active view where `active` names none of the views", () => {
    const lines = ["- Analysis (id: v1): iris, head, plot, blockr.assistant-assistant_plugin, blockr.dag-dag_plugin", "- Overview (id: v2): empty"];
    expect(assistant.describe(DOCK, { ...BOARD, active: null })).toEqual(lines);
    expect(assistant.describe(DOCK, { ...BOARD, active: "gone" })).toEqual(lines);
    expect(assistant.describe(DOCK, { ...BOARD, active: "constructor" })).toEqual(lines);
  });

  test("gives no lines for a state without views, or where the board holds no state for the dock", () => {
    expect(assistant.describe(DOCK, defaultState)).toEqual([]);
    expect(assistant.describe(DOCK, null)).toEqual([]);
  });

  test("gives no lines for another plugin class", () => {
    expect(assistant.describe(DAG.plugin, BOARD)).toEqual([]);
  });

  test("throws where the state does not have the shape of the dock's state", () => {
    expect(() => assistant.describe(DOCK, { ...BOARD, views: [] })).toThrow("The dock's state does not have the shape that its schema gives.");
    expect(() => assistant.describe(DOCK, "views")).toThrow("The dock's state does not have the shape that its schema gives.");
  });
});
