// The order in which the navigation lists the views and folders.

import { describe, expect, test } from "vitest";
import { compare, firstView, inFolder, keyIn, keysBetween, sameEntry, sitsAt, viewOrder, within } from "./navigation.ts";
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

describe("sameEntry()", () => {
  test("tells a view from a folder of the same id", () => {
    expect(sameEntry({ view: "x" }, { view: "x" })).toBe(true);
    expect(sameEntry({ folder: "x" }, { folder: "x" })).toBe(true);
    expect(sameEntry({ view: "x" }, { folder: "x" })).toBe(false);
    expect(sameEntry({ view: "x" }, { view: "y" })).toBe(false);
  });
});

describe("keyIn()", () => {
  test("gives a key after the last view or folder of a folder, or of the top", () => {
    expect(keyIn(state, null, undefined)).toBe("a3");
    expect(keyIn(state, "safety", undefined)).toBe("a2");
  });

  test("gives a key between the neighbours of the view or folder that it goes before", () => {
    const key = keyIn(state, null, { folder: "safety" });
    expect(compare("a0", key)).toBe(-1);
    expect(compare(key, "a1")).toBe(-1);
    expect(keyIn(state, null, { view: "overview" })).toBe("Zz");
  });

  test("gives a key in an empty folder, or one that the state lacks", () => {
    expect(keyIn(state, "nested", { view: "labs" })).toBe("Zz");
    expect(keyIn({ ...state, views: {} }, "safety", undefined)).toBe("a2");
  });

  test("leaves out the view or folder that moves", () => {
    // Moving Overview to the end of the top puts it after Listing, which the
    // key of Listing, a2, is the last before.
    expect(keyIn(state, null, undefined, { view: "overview" })).toBe("a3");
    // Moving Listing before Safety puts it between Overview and Safety.
    const key = keyIn(state, null, { folder: "safety" }, { view: "listing" });
    expect(compare("a0", key)).toBe(-1);
    expect(compare(key, "a1")).toBe(-1);
  });

  test("gives a key after the one before where two neighbours have the same key", () => {
    const tied: DockState = { ...defaultState, views: { a: view(null, "a0"), b: view(null, "a0") } };
    expect(keyIn(tied, null, { view: "b" })).toBe("a1");
  });
});

describe("sitsAt()", () => {
  test("says whether a view or folder sits right before another, or last", () => {
    expect(sitsAt(state, { view: "overview" }, null, { folder: "safety" })).toBe(true);
    expect(sitsAt(state, { view: "overview" }, null, { view: "listing" })).toBe(false);
    expect(sitsAt(state, { view: "listing" }, null, undefined)).toBe(true);
    expect(sitsAt(state, { view: "overview" }, null, undefined)).toBe(false);
  });

  test("says false for a view or folder that sits in another folder", () => {
    expect(sitsAt(state, { view: "ae" }, null, undefined)).toBe(false);
    expect(sitsAt(state, { folder: "nested" }, "safety", undefined)).toBe(true);
  });
});

describe("within()", () => {
  test("says whether a folder is another or sits in it, through the folders that hold it", () => {
    expect(within(state, "nested", "safety")).toBe(true);
    expect(within(state, "safety", "safety")).toBe(true);
    expect(within(state, "safety", "nested")).toBe(false);
  });

  test("ends on a folder that sits in itself", () => {
    const loop: DockState = { ...defaultState, folders: { x: folder("y", "a0"), y: folder("x", "a0"), z: folder(null, "a1") } };
    expect(within(loop, "x", "z")).toBe(false);
    expect(within(loop, "x", "y")).toBe(true);
  });
});

describe("keysBetween()", () => {
  test("gives keys in order between two keys", () => {
    const keys = keysBetween("a0", "a1", 2);
    expect(keys).toHaveLength(2);
    expect([...keys].sort(compare)).toEqual(keys);
    expect(keys.every((key) => compare("a0", key) < 0 && compare(key, "a1") < 0)).toBe(true);
  });

  test("leaves out a bound that is no key, or the upper of two equal bounds", () => {
    expect(keysBetween("a0", "not a key", 1)).toEqual(["a1"]);
    expect(keysBetween("not a key", "a1", 1)).toEqual(["a0"]);
    expect(keysBetween("a0", "a0", 1)).toEqual(["a1"]);
    expect(keysBetween("?", "?", 1)).toEqual(["a0"]);
  });
});
