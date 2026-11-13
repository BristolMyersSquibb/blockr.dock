// The `migrate` of the dock plugin's class from version 1, today's shape, as
// the conversion in blockr.convert writes it, to version 2.
// The tests of the dock's module open the converted board of the page
// "Saved boards and app scripts across the switch" through the store, in
// both of its forms.

import type { Json } from "@blockr/core";
import { describe, expect, test } from "vitest";
import { migrate } from "./migrate.ts";
import { defaultState } from "./state.ts";
import type { DockState, Grid, Panel, TabGroup } from "./types.ts";

const a = { block: "a" };
const b = { block: "b" };
const c = { block: "c" };
const dag = { plugin: "blockr.dag-dag_plugin" };

function group(...panels: Panel[]): TabGroup {
  const [first] = panels;
  if (first === undefined) throw new Error("A tab group holds a panel.");
  return { panels, active: first };
}

// The state at version 2 that a state at version 1 migrates to.
function migrated(state: Json): DockState {
  return migrate(state, 1) as DockState;
}

// The grid at version 2 of the one view of a state at version 1 with the
// members and the grid given.
function gridOf(members: Json, grid?: Json): Grid | undefined {
  const views = migrated({ views: { v: { name: "V", members } }, active: "v", ...(grid === undefined ? {} : { grids: { v: grid } }) }).views;
  return views.v?.grid;
}

describe("the migrate from version 1", () => {
  test("reads a value of length one as an array, and `rails: []` as a grid without rails", () => {
    const grid = { orientation: "horizontal", children: [{ panels: "block_panel-a", active: "block_panel-a" }], sizes: 1, rails: [] };
    expect(gridOf("block_panel-a", grid)).toEqual({ orientation: "horizontal", children: [group(a)], sizes: [1] });
  });

  test("restricts a grid to the view's members, and gives each member that the grid lacks a tab group at the end of the top split", () => {
    const grid = {
      orientation: "vertical",
      children: [
        { panels: ["block_panel-a", "block_panel-gone"], active: "block_panel-gone" },
        { panels: "block_panel-x", active: "block_panel-x" },
      ],
      sizes: [0.6, 0.4],
    };
    expect(gridOf(["block_panel-a", "block_panel-b", "ext_panel-blockr.dag-dag_plugin"], grid)).toEqual({
      orientation: "vertical",
      children: [group(a), group(b), group(dag)],
      sizes: [1 / 3, 1 / 3, 1 / 3],
    });
  });

  test("gives a view without a grid the default grid over its members, and a view without members an empty grid", () => {
    expect(gridOf(["block_panel-a", "ext_panel-blockr.dag-dag_plugin", "block_panel-b"])).toEqual({
      orientation: "horizontal",
      children: [group(dag), group(a, b)],
      sizes: [0.5, 0.5],
    });
    const grid = { orientation: "horizontal", children: [{ panels: "block_panel-a", active: "block_panel-a" }], sizes: 1 };
    expect(gridOf([], grid)).toEqual({ orientation: "horizontal", children: [], sizes: [] });
  });

  test("names each panel by an object, by the prefix of its id, and drops a panel of another prefix", () => {
    expect(gridOf(["block_panel-a", "ext_panel-blockr.dag-dag_plugin", "other-c", "block_panel-"])).toEqual({
      orientation: "horizontal",
      children: [group(dag), group(a)],
      sizes: [0.5, 0.5],
    });
  });

  test("drops a rail's position and has the rail claim its panels from the tree", () => {
    const grid = {
      orientation: "horizontal",
      children: [{ panels: ["block_panel-a", "ext_panel-blockr.dag-dag_plugin"], active: "ext_panel-blockr.dag-dag_plugin" }],
      sizes: 1,
      rails: { left: { position: "left", panels: "ext_panel-blockr.dag-dag_plugin", active: "ext_panel-blockr.dag-dag_plugin", collapsed: false, size: 300, collapsed_size: 40 } },
    };
    expect(gridOf(["block_panel-a", "ext_panel-blockr.dag-dag_plugin"], grid)).toEqual({
      orientation: "horizontal",
      children: [group(a)],
      sizes: [1],
      rails: { left: { ...group(dag), collapsed: false, size: 300, collapsed_size: 40 } },
    });
  });

  test("reads rails given as a list by their position, with the default widths where they give none, and drops a rail without panels", () => {
    const grid: Json = {
      orientation: "horizontal",
      children: [{ panels: "block_panel-a", active: "block_panel-a" }],
      sizes: 1,
      rails: [
        { position: "right", panels: "block_panel-b", active: "block_panel-b", collapsed: true },
        { position: "left", panels: [], collapsed: false, size: 400, collapsed_size: 35 },
      ],
    };
    expect(gridOf(["block_panel-a", "block_panel-b"], grid)).toEqual({
      orientation: "horizontal",
      children: [group(a)],
      sizes: [1],
      rails: { right: { ...group(b), collapsed: true, size: 260, collapsed_size: 35 } },
    });
  });

  test("keeps a panel that a grid places twice where it comes first", () => {
    const grid = {
      orientation: "horizontal",
      children: [
        { panels: ["block_panel-a", "block_panel-b"], active: "block_panel-b" },
        { panels: ["block_panel-b", "block_panel-c"], active: "block_panel-b" },
      ],
      sizes: [0.5, 0.5],
    };
    expect(gridOf(["block_panel-a", "block_panel-b", "block_panel-c"], grid)).toEqual({
      orientation: "horizontal",
      children: [{ panels: [a, b], active: b }, group(c)],
      sizes: [0.5, 0.5],
    });
  });

  test("puts every view at the top, with a key in the order of the views, and gives the state no folders", () => {
    const views = Object.fromEntries(Array.from({ length: 63 }, (_, i) => [`view_${String(i)}`, { name: "V", members: [] }]));
    const state = migrated({ views, active: "view_0" });
    const indexes = Object.values(state.views).map((view) => view.index);
    expect(Object.values(state.views).every((view) => view.folder === null)).toBe(true);
    expect([indexes[0], indexes[9], indexes[10], indexes[35], indexes[36], indexes[61], indexes[62]]).toEqual([
      "a0",
      "a9",
      "aA",
      "aZ",
      "aa",
      "az",
      "b00",
    ]);
    expect(state.folders).toEqual({});
  });

  test("names a view without a name after its id, as today's views menu labels one", () => {
    const state = migrated({ views: { "my_view.2-b": { members: [] }, blank: { name: "  ", members: [] } } });
    expect(state.views["my_view.2-b"]?.name).toBe("My view 2 b");
    expect(state.views.blank?.name).toBe("Blank");
  });

  test("keeps the active view, or makes the first view active where `active` names none", () => {
    const views = { one: { name: "One", members: [] }, two: { name: "Two", members: [] } };
    expect(migrated({ views, active: "two" }).active).toBe("two");
    expect(migrated({ views, active: "gone" }).active).toBe("one");
    expect(migrated({ views }).active).toBe("one");
    expect(migrated({ views: [], active: [] }).active).toBeNull();
  });

  test("keeps `visible` as arrays, without the section ctrl and without an entry that lists both sections", () => {
    const visible = { a: "outputs", b: ["inputs", "outputs"], c: ["inputs", "ctrl"], d: "ctrl", e: ["outputs", "inputs", "ctrl"] };
    expect(migrated({ views: [], visible }).visible).toEqual({ a: ["outputs"], c: ["inputs"], d: [] });
  });

  test("gives the default for a state that is no object, and leaves a state of version 2 as it is", () => {
    expect(migrate("today", 1)).toEqual(defaultState);
    expect(migrate(defaultState, 2)).toBe(defaultState);
  });
});
