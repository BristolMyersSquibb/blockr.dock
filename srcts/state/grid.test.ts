// The helpers of a view's grid, which the `update`, the `migrate` and the
// dock's commands share.

import { describe, expect, test } from "vitest";
import {
  appendTabGroup,
  canonicalGrid,
  defaultGrid,
  gridFailures,
  gridPanels,
  placePanel,
  prunePanels,
} from "./grid.ts";
import type { Grid, Panel, Rail, TabGroup } from "./types.ts";

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

// The view Overview of the page: a tab group of a and b, which shows b, in
// its left 30%, and in the rest c above blockr.md's document, with the DAG on
// a rail at its left edge.
const overview: Grid = {
  orientation: "horizontal",
  children: [
    { panels: [a, b], active: b },
    { children: [group(c), group(md)], sizes: [0.6, 0.4] },
  ],
  sizes: [0.3, 0.7],
  rails: { left: rail(dag) },
};

describe("gridPanels()", () => {
  test("gives the panels that a grid places in reading order, its tab groups before its rails", () => {
    expect(gridPanels(overview)).toEqual([a, b, c, md, dag]);
  });
});

describe("canonicalGrid()", () => {
  test("leaves a grid in its canonical form as it is", () => {
    expect(canonicalGrid(overview)).toEqual(overview);
  });

  test("scales the shares of each split to sum to 1", () => {
    const grid: Grid = { orientation: "vertical", children: [group(a), group(b)], sizes: [2, 6] };
    expect(canonicalGrid(grid).sizes).toEqual([0.25, 0.75]);
  });

  test("splits evenly where the sizes do not give each child a positive share", () => {
    for (const sizes of [[1], [0.5, 0], [-1, 2], [Number.NaN, 1]]) {
      const grid: Grid = { orientation: "horizontal", children: [group(a), group(b)], sizes };
      expect(canonicalGrid(grid).sizes).toEqual([0.5, 0.5]);
    }
  });

  test("drops the tab groups, splits and rails that hold no panel, and gives their shares to their siblings", () => {
    const grid: Grid = {
      orientation: "horizontal",
      children: [
        { panels: [], active: a },
        { children: [{ children: [], sizes: [] }], sizes: [1] },
        group(b),
      ],
      sizes: [0.2, 0.3, 0.5],
      rails: { right: { ...rail(dag), panels: [] } },
    };
    expect(canonicalGrid(grid)).toEqual({ orientation: "horizontal", children: [group(b)], sizes: [1] });
  });

  test("has a tab group or rail whose `active` it does not hold show its first panel", () => {
    const grid: Grid = {
      orientation: "horizontal",
      children: [{ panels: [a, b], active: c }],
      sizes: [1],
      rails: { left: { ...rail(dag, md), active: c } },
    };
    expect(canonicalGrid(grid)).toEqual({
      orientation: "horizontal",
      children: [{ panels: [a, b], active: a }],
      sizes: [1],
      rails: { left: rail(dag, md) },
    });
  });

  test("keeps an empty top split, the grid of a view that shows nothing", () => {
    const grid: Grid = { orientation: "horizontal", children: [], sizes: [] };
    expect(canonicalGrid(grid)).toEqual(grid);
  });
});

describe("prunePanels()", () => {
  test("takes panels out and gives the shares of what this empties to the siblings, in proportion", () => {
    const pruned = prunePanels(overview, (panel) => !("block" in panel && panel.block === "c"));
    expect(pruned).toEqual({
      orientation: "horizontal",
      children: [{ panels: [a, b], active: b }, { children: [group(md)], sizes: [1] }],
      sizes: [0.3, 0.7],
      rails: { left: rail(dag) },
    });
  });

  test("has a tab group whose shown panel went show its first panel", () => {
    const pruned = prunePanels(overview, (panel) => !("block" in panel && panel.block === "b"));
    expect(pruned.children[0]).toEqual(group(a));
  });

  test("drops a rail that it empties, and a split, with its share", () => {
    const pruned = prunePanels(overview, (panel) => "block" in panel && panel.block !== "c");
    expect(pruned).toEqual({ orientation: "horizontal", children: [{ panels: [a, b], active: b }], sizes: [1] });
  });
});

describe("gridFailures()", () => {
  test("finds nothing wrong with a grid in its canonical form", () => {
    expect(gridFailures(overview)).toEqual([]);
  });

  test("names sizes that do not give each child one share, a share not above 0, and shares that do not sum to 1", () => {
    const grid: Grid = {
      orientation: "horizontal",
      children: [
        { children: [group(a), group(b)], sizes: [1] },
        { children: [group(c), group(md)], sizes: [0, 1] },
        { children: [group(dag)], sizes: [0.5] },
      ],
      sizes: [0.2, 0.2, 0.2],
    };
    expect(gridFailures(grid)).toEqual([
      'The shares of the split at "/" do not sum to 1.',
      'The split at "/children/0" has 2 children and 1 share.',
      'The split at "/children/1" gives a child a share that is not above 0.',
      'The shares of the split at "/children/2" do not sum to 1.',
    ]);
  });

  test("names what holds no panel, an `active` that is none of the panels, and a panel placed twice", () => {
    const grid: Grid = {
      orientation: "horizontal",
      children: [{ panels: [], active: a }, { children: [], sizes: [] }, { panels: [a, b], active: c }],
      sizes: [0.25, 0.25, 0.5],
      rails: { left: rail(a), right: { ...rail(dag), panels: [] } },
    };
    expect(gridFailures(grid)).toEqual([
      'The tab group at "/children/0" holds no panel.',
      'The split at "/children/1" holds nothing.',
      'The tab group at "/children/2" shows a panel that it does not hold.',
      'The rail at "/rails/right" holds no panel.',
      'The grid places the panel of the block "a" more than once.',
    ]);
  });

  test("allows the shares of a split to sum to 1 as floating point does", () => {
    const grid: Grid = { orientation: "horizontal", children: [group(a), group(b), group(c)], sizes: [0.1, 0.2, 0.7] };
    expect(gridFailures(grid)).toEqual([]);
  });
});

describe("placePanel()", () => {
  test("adds a panel as a new tab, which it shows, in the last tab group that holds no plugin's panel", () => {
    const grid: Grid = {
      orientation: "horizontal",
      children: [group(a), { children: [group(b), group(md)], sizes: [0.5, 0.5] }],
      sizes: [0.4, 0.6],
    };
    expect(placePanel(grid, c)).toEqual({
      ...grid,
      children: [group(a), { children: [{ panels: [b, c], active: c }, group(md)], sizes: [0.5, 0.5] }],
    });
  });

  test("adds a tab group at the end of the top split, with the mean of its siblings' shares, where every tab group holds a plugin's panel", () => {
    const grid: Grid = { orientation: "horizontal", children: [group(dag), { panels: [a, md], active: a }], sizes: [0.25, 0.75] };
    expect(placePanel(grid, b)).toEqual({
      orientation: "horizontal",
      children: [group(dag), { panels: [a, md], active: a }, group(b)],
      sizes: [0.25 / 1.5, 0.75 / 1.5, 0.5 / 1.5],
    });
  });

  test("puts the panel in a tab group of its own in an empty grid", () => {
    expect(placePanel(defaultGrid([]), a)).toEqual({ orientation: "horizontal", children: [group(a)], sizes: [1] });
  });

  test("leaves a grid that places the panel already, on a rail too, as it is", () => {
    expect(placePanel(overview, dag)).toBe(overview);
    expect(placePanel(overview, c)).toBe(overview);
  });
});

describe("appendTabGroup()", () => {
  test("adds a tab group of the panel alone at the end of the top split", () => {
    const grid: Grid = { orientation: "vertical", children: [group(a)], sizes: [1] };
    expect(appendTabGroup(grid, b)).toEqual({ orientation: "vertical", children: [group(a), group(b)], sizes: [0.5, 0.5] });
  });
});

describe("defaultGrid()", () => {
  test("puts the plugins' panels in one tab group, beside the blocks' panels in another", () => {
    expect(defaultGrid([a, dag, b, md])).toEqual({
      orientation: "horizontal",
      children: [group(dag, md), group(a, b)],
      sizes: [0.5, 0.5],
    });
  });

  test("gives one tab group where the panels are of one kind, and none where there are none", () => {
    expect(defaultGrid([a, b])).toEqual({ orientation: "horizontal", children: [group(a, b)], sizes: [1] });
    expect(defaultGrid([])).toEqual({ orientation: "horizontal", children: [], sizes: [] });
  });
});
