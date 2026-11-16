// The rows of a menu of views and folders, and where a drag among them lands.

import { describe, expect, test } from "vitest";
import { defaultState } from "../../state/index.ts";
import type { DockState, Entry, Folder, FolderId, View } from "../../state/index.ts";
import { dropAt, rowsOf } from "./views-list.ts";
import type { Row } from "./views-list.ts";

function view(folder: FolderId | null, index: string): View {
  return { name: "View", folder, index, grid: { orientation: "horizontal", children: [], sizes: [] } };
}

function folder(parent: FolderId | null, index: string): Folder {
  return { name: "Folder", folder: parent, index };
}

// The page's board: Overview at the top, then the folder Safety, which holds
// Adverse events and the folder Nested, which holds Labs, and then Listing.
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

// Each row as its entry and its depth.
function listed(rows: readonly Row[]): [Entry, number][] {
  return rows.map((row) => [row.entry, row.depth]);
}

describe("rowsOf()", () => {
  test("lists the views and folders of the top, with what an open folder holds below its row", () => {
    expect(listed(rowsOf(state, null, new Set()))).toEqual([
      [{ view: "overview" }, 0],
      [{ folder: "safety" }, 0],
      [{ view: "listing" }, 0],
    ]);
    expect(listed(rowsOf(state, null, new Set(["safety", "nested"])))).toEqual([
      [{ view: "overview" }, 0],
      [{ folder: "safety" }, 0],
      [{ view: "ae" }, 1],
      [{ folder: "nested" }, 1],
      [{ view: "labs" }, 2],
      [{ view: "listing" }, 0],
    ]);
    // A closed folder hides what it holds, the folders open in it included.
    expect(listed(rowsOf(state, null, new Set(["nested"])))).toHaveLength(3);
  });

  test("gives each row the folder that lists it, and whether a folder shows open", () => {
    const rows = rowsOf(state, null, new Set(["safety"]));
    expect(rows.map((row) => row.folder)).toEqual([null, null, "safety", "safety", null]);
    expect(rows.map((row) => row.open)).toEqual([false, true, false, false, false]);
  });

  test("lists what a folder holds, for the menu of its tab", () => {
    expect(listed(rowsOf(state, "safety", new Set(["nested"])))).toEqual([
      [{ view: "ae" }, 0],
      [{ folder: "nested" }, 0],
      [{ view: "labs" }, 1],
    ]);
  });

  test("shows a folder closed where the menu lists it again inside itself", () => {
    const loop: DockState = {
      ...defaultState,
      views: { inside: view("x", "a0") },
      folders: { x: folder("y", "a0"), y: folder("x", "a1") },
    };
    // Both folders sit in themselves, so the navigation lists both at the top.
    expect(listed(rowsOf(loop, null, new Set(["x", "y"])))).toEqual([
      [{ folder: "x" }, 0],
      [{ view: "inside" }, 1],
      [{ folder: "y" }, 0],
    ]);
    expect(listed(rowsOf(loop, "x", new Set(["x"])))).toEqual([[{ view: "inside" }, 0]]);
  });
});

describe("dropAt()", () => {
  // The rows with Safety open, each 34 pixels high from the top of the
  // window: Overview, Safety, Adverse events, Nested and Listing.
  const rows = rowsOf(state, null, new Set(["safety"]));
  const spans = rows.map((_, i) => ({ top: 34 * i, bottom: 34 * (i + 1) }));
  const at = (y: number, moving: Entry) => dropAt(state, rows, spans, y, moving);

  test("puts a view before or after the view under the pointer, in that view's folder", () => {
    expect(at(10, { view: "listing" })).toEqual({
      folder: null,
      before: { view: "overview" },
      mark: { row: 0, at: "above", depth: 0 },
    });
    expect(at(95, { view: "listing" })).toEqual({
      folder: "safety",
      before: { folder: "nested" },
      mark: { row: 2, at: "below", depth: 1 },
    });
    expect(at(-5, { view: "listing" })?.before).toEqual({ view: "overview" });
  });

  test("puts it before or after a folder over the edges of the folder's row, and first in an open one", () => {
    expect(at(40, { view: "listing" })).toEqual({
      folder: null,
      before: { folder: "safety" },
      mark: { row: 1, at: "above", depth: 0 },
    });
    expect(at(66, { view: "listing" })).toEqual({
      folder: "safety",
      before: { view: "ae" },
      mark: { row: 1, at: "below", depth: 1 },
    });
    // Nested is closed, so a drop below its row goes after it, last in Safety.
    expect(at(132, { view: "listing" })).toEqual({
      folder: "safety",
      before: undefined,
      mark: { row: 3, at: "below", depth: 1 },
    });
  });

  test("puts it last in a folder over the middle of the folder's row", () => {
    expect(at(51, { view: "listing" })).toEqual({
      folder: "safety",
      before: undefined,
      mark: { row: 1, at: "into", depth: 1 },
    });
    expect(at(119, { view: "overview" })).toEqual({
      folder: "nested",
      before: undefined,
      mark: { row: 3, at: "into", depth: 2 },
    });
  });

  test("puts it last at the top below the last row", () => {
    expect(at(200, { view: "overview" })).toEqual({
      folder: null,
      before: undefined,
      mark: { row: 4, at: "below", depth: 0 },
    });
  });

  test("lands nowhere where the view or folder would stay where it is", () => {
    expect(at(140, { view: "listing" })).toBeUndefined();
    expect(at(160, { view: "listing" })).toBeUndefined();
    expect(at(200, { view: "listing" })).toBeUndefined();
    // Overview sits right before Safety already.
    expect(at(25, { view: "overview" })).toBeUndefined();
    expect(at(40, { view: "overview" })).toBeUndefined();
  });

  test("lands nowhere where a folder would go into itself or into a folder that it holds", () => {
    expect(at(51, { folder: "safety" })).toBeUndefined();
    expect(at(80, { folder: "safety" })).toBeUndefined();
    expect(at(119, { folder: "safety" })).toBeUndefined();
    expect(at(10, { folder: "safety" })).toEqual({
      folder: null,
      before: { view: "overview" },
      mark: { row: 0, at: "above", depth: 0 },
    });
    expect(at(140, { folder: "nested" })?.folder).toBeNull();
  });

  test("lands nowhere in a menu without rows", () => {
    expect(dropAt(defaultState, [], [], 10, { view: "overview" })).toBeUndefined();
  });
});
