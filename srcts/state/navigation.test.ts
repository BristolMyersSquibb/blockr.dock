// The order in which the navigation lists the views and folders.

import { describe, expect, test } from "vitest";
import { firstView, inFolder, viewOrder } from "./navigation.ts";
import { defaultState } from "./state.ts";
import type { DockState, Folder, FolderId, View } from "./types.ts";

function view(folder: FolderId | null, index: string): View {
  return { name: "View", folder, index, grid: { orientation: "horizontal", children: [], sizes: [] } };
}

function folder(parent: FolderId | null, index: string): Folder {
  return { name: "Folder", folder: parent, index };
}

// The page's board: Overview at the top, then the folder Safety, which holds
// Adverse events, with a second view at the top after both, and a folder
// inside Safety, after Adverse events, which holds Labs.
const state: DockState = {
  ...defaultState,
  views: {
    overview: view(null, "a0"),
    ae: view("safety", "a0"),
    labs: view("nested", "a0"),
    listing: view(null, "a2"),
  },
  folders: { safety: folder(null, "a1"), nested: folder("safety", "a1") },
  active: "overview",
};

describe("inFolder()", () => {
  test("lists the views and folders of a folder, or of the top, by their index", () => {
    expect(inFolder(state, null)).toEqual([{ view: "overview" }, { folder: "safety" }, { view: "listing" }]);
    expect(inFolder(state, "safety")).toEqual([{ view: "ae" }, { folder: "nested" }]);
  });

  test("compares the keys by their code units, and the ids where two keys are equal", () => {
    const keys: DockState = {
      ...defaultState,
      views: { b: view(null, "a0"), a: view(null, "a0"), lower: view(null, "a0V"), upper: view(null, "aZ") },
    };
    expect(inFolder(keys, null)).toEqual([{ view: "a" }, { view: "b" }, { view: "lower" }, { view: "upper" }]);
  });
});

describe("viewOrder()", () => {
  test("lists every view, with the views of each folder in the folder's place", () => {
    expect(viewOrder(state)).toEqual(["overview", "ae", "labs", "listing"]);
  });

  test("lists a view or folder in a folder that the state lacks at the top", () => {
    const stray: DockState = { ...state, views: { ...state.views, lost: view("gone", "a1V") } };
    expect(viewOrder(stray)).toEqual(["overview", "ae", "labs", "lost", "listing"]);
  });

  test("lists a folder that sits in itself at the top, with what it holds", () => {
    const loop: DockState = {
      ...defaultState,
      views: { first: view(null, "a1"), inside: view("x", "a0") },
      folders: { x: folder("y", "a0"), y: folder("x", "a2") },
    };
    expect(inFolder(loop, null)).toEqual([{ folder: "x" }, { view: "first" }, { folder: "y" }]);
    expect(viewOrder(loop)).toEqual(["inside", "first"]);
  });
});

describe("firstView()", () => {
  test("gives the first view of the navigation, which can sit in a folder", () => {
    const folded: DockState = {
      ...defaultState,
      views: { late: view(null, "a1"), early: view("f", "a0") },
      folders: { f: folder(null, "a0") },
    };
    expect(firstView(folded)).toBe("early");
  });

  test("gives null where the state holds no view", () => {
    expect(firstView(defaultState)).toBeNull();
  });
});
