// The narrow column of a view, which stacks its tab groups and its rails.

import { describe, expect, test } from "vitest";
import type { Grid, Panel, Rail, TabGroup } from "../state/index.ts";
import { stackGrid } from "./index.ts";

const a = { block: "a" };
const b = { block: "b" };
const c = { block: "c" };
const dag = { plugin: "blockr.dag-dag_plugin" };
const md = { plugin: "blockr.md-md_plugin" };

function group(...panels: Panel[]): TabGroup {
  const [first] = panels;
  if (first === undefined) throw new Error("A tab group holds a panel.");
  return { panels, active: first };
}

function rail(...panels: Panel[]): Rail {
  return { ...group(...panels), collapsed: false, size: 260, collapsed_size: 35 };
}

describe("stackGrid()", () => {
  test("stacks the tab groups in reading order, then the rails, left before right", () => {
    const grid: Grid = {
      orientation: "horizontal",
      children: [{ panels: [a, b], active: b }, { children: [group(c), group(md)], sizes: [0.6, 0.4] }],
      sizes: [0.3, 0.7],
      rails: { right: rail(dag), left: { ...rail(dag), panels: [{ block: "d" }], active: { block: "d" } } },
    };
    const column = stackGrid(grid, 0.8);
    expect(column.grid.orientation).toBe("vertical");
    expect(column.grid.children).toEqual([{ panels: [a, b], active: b }, group(c), group(md), group({ block: "d" }), group(dag)]);
  });

  test("makes each tab group as tall as the wide grid makes it, up to the cap", () => {
    // The top split divides the width, so a and the split beside it are as
    // tall as the window, which the split divides between c and md.
    const grid: Grid = {
      orientation: "horizontal",
      children: [group(a), { children: [group(c), group(md)], sizes: [0.75, 0.25] }],
      sizes: [0.5, 0.5],
    };
    const column = stackGrid(grid, 0.6);
    const heights = column.grid.sizes.map((size) => size * column.height);
    [0.6, 0.6, 0.25].forEach((height, i) => {
      expect(heights[i]).toBeCloseTo(height);
    });
    expect(column.height).toBeCloseTo(1.45);
  });

  test("divides the height where the top split stacks its children", () => {
    const grid: Grid = { orientation: "vertical", children: [group(a), group(b)], sizes: [0.3, 0.7] };
    const column = stackGrid(grid, 0.8);
    expect(column.grid.sizes).toEqual([0.3, 0.7]);
    expect(column.height).toBeCloseTo(1);
  });

  test("gives each rail the cap, collapsed or not", () => {
    const grid: Grid = {
      orientation: "horizontal",
      children: [group(a)],
      sizes: [1],
      rails: { left: { ...rail(dag), collapsed: true } },
    };
    const column = stackGrid(grid, 0.5);
    expect(column.grid.children).toEqual([group(a), group(dag)]);
    expect(column.height).toBeCloseTo(1);
  });

  test("gives a grid that shows nothing an empty column of the cap's height", () => {
    const column = stackGrid({ orientation: "horizontal", children: [], sizes: [] }, 0.8);
    expect(column).toEqual({ grid: { orientation: "vertical", children: [], sizes: [] }, height: 0.8 });
  });
});
