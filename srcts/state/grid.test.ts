// The helpers of a view's grid, which the `update` and the dock's commands
// share.

import { describe, expect, test } from "vitest";
import {
  canonicalGrid,
  collapsedRail,
  gridChanges,
  gridFailures,
  gridPanels,
  locate,
  placedAt,
  placeFailure,
  placePanel,
  prunePanels,
  resized,
  showing,
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

  test("drops the tab groups, splits and rails that hold no panel, and gives their shares to their siblings in proportion", () => {
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
    const three: Grid = { orientation: "horizontal", children: [{ panels: [], active: a }, group(a), group(b)], sizes: [0.2, 0.3, 0.5] };
    expect(canonicalGrid(three).sizes).toEqual([0.3 / 0.8, 0.5 / 0.8]);
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
  test("takes panels out, and drops the tab groups, splits and rails that this empties", () => {
    const pruned = prunePanels(overview, (panel) => !("block" in panel && panel.block === "c"));
    expect(pruned).toEqual({
      orientation: "horizontal",
      children: [{ panels: [a, b], active: b }, { children: [group(md)], sizes: [1] }],
      sizes: [0.3, 0.7],
      rails: { left: rail(dag) },
    });
  });

  test("evens out a split whose tab group it empties, as dockview does where it removes a group", () => {
    const three: Grid = { orientation: "horizontal", children: [group(a), group(b), group(c)], sizes: [0.2, 0.3, 0.5] };
    expect(prunePanels(three, (panel) => panel !== b).sizes).toEqual([0.5, 0.5]);
    // A tab group that held no panel already does not even it out.
    const empty: Grid = { ...three, children: [group(a), { panels: [], active: a }, group(c)] };
    expect(prunePanels(empty, () => true).sizes).toEqual([0.2 / 0.7, 0.5 / 0.7]);
    const both: Grid = { orientation: "horizontal", children: [group(a), { panels: [], active: a }, group(b), group(c)], sizes: [0.1, 0.2, 0.3, 0.4] };
    expect(prunePanels(both, (panel) => panel !== b).sizes).toEqual([0.5, 0.5]);
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

  test("adds a tab group at the end of the top split, which evens it out, where every tab group holds a plugin's panel", () => {
    const grid: Grid = { orientation: "horizontal", children: [group(dag), { panels: [a, md], active: a }], sizes: [0.25, 0.75] };
    expect(placePanel(grid, b)).toEqual({
      orientation: "horizontal",
      children: [group(dag), { panels: [a, md], active: a }, group(b)],
      sizes: [1 / 3, 1 / 3, 1 / 3],
    });
  });

  test("puts the panel in a tab group of its own in an empty grid", () => {
    expect(placePanel({ orientation: "horizontal", children: [], sizes: [] }, a)).toEqual({
      orientation: "horizontal",
      children: [group(a)],
      sizes: [1],
    });
  });

  test("leaves a grid that places the panel already, on a rail too, as it is", () => {
    expect(placePanel(overview, dag)).toBe(overview);
    expect(placePanel(overview, c)).toBe(overview);
  });
});

describe("locate()", () => {
  test("gives the path of the tab group that holds a panel, or the edge of its rail", () => {
    expect(locate(overview, b)).toEqual({ path: [0] });
    expect(locate(overview, md)).toEqual({ path: [1, 1] });
    expect(locate(overview, dag)).toEqual({ edge: "left" });
    expect(locate(overview, { block: "z" })).toBeUndefined();
  });
});

describe("placedAt()", () => {
  test("adds a panel as a new tab of the tab group of the panel that `near` names, which it shows", () => {
    const placed = placedAt(overview, { block: "d" }, { near: c }, "distribute");
    expect(placed.children[1]).toEqual({
      children: [{ panels: [c, { block: "d" }], active: { block: "d" } }, group(md)],
      sizes: [0.6, 0.4],
    });
  });

  test("adds a tab group beside it, which evens out its split where it divides that way, as dockview's addPanel() does", () => {
    // The split that holds c divides vertically, the other way from the top.
    const placed = placedAt(overview, { block: "d" }, { near: c, side: "below" }, "distribute");
    expect(placed.children[1]).toEqual({ children: [group(c), group({ block: "d" }), group(md)], sizes: [1 / 3, 1 / 3, 1 / 3] });
    const left = placedAt(overview, { block: "d" }, { near: a, side: "left" }, "distribute");
    expect(left.children).toEqual([group({ block: "d" }), overview.children[0], overview.children[1]]);
    expect(left.sizes).toEqual([1 / 3, 1 / 3, 1 / 3]);
  });

  test("halves the tab group's share there with \"split\", as dockview's moveTo() and a drag do", () => {
    const placed = placedAt(overview, { block: "d" }, { near: c, side: "below" }, "split");
    expect(placed.children[1]).toEqual({ children: [group(c), group({ block: "d" }), group(md)], sizes: [0.3, 0.3, 0.4] });
    const left = placedAt(overview, { block: "d" }, { near: a, side: "left" }, "split");
    expect(left.children).toEqual([group({ block: "d" }), overview.children[0], overview.children[1]]);
    expect(left.sizes).toEqual([0.15, 0.15, 0.7]);
  });

  test("splits the tab group's place in two where its split divides the other way, with either sizing", () => {
    for (const sizing of ["split", "distribute"] as const) {
      const placed = placedAt(overview, { block: "d" }, { near: c, side: "right" }, sizing);
      expect(placed.children[1]).toEqual({
        children: [{ children: [group(c), group({ block: "d" })], sizes: [0.5, 0.5] }, group(md)],
        sizes: [0.6, 0.4],
      });
      const above = placedAt(overview, { block: "d" }, { near: b, side: "above" }, sizing);
      expect(above.children[0]).toEqual({ children: [group({ block: "d" }), overview.children[0]], sizes: [0.5, 0.5] });
      expect(above.sizes).toEqual([0.3, 0.7]);
    }
  });

  test("adds a panel to the rail that `rail` names, before `near` and `side`, or opens a rail there", () => {
    expect(placedAt(overview, { block: "d" }, { rail: "left", near: a, side: "right" }, "distribute").rails).toEqual({
      left: { ...rail(dag), panels: [dag, { block: "d" }], active: { block: "d" } },
    });
    expect(placedAt(overview, { block: "d" }, { rail: "right" }, "distribute").rails).toEqual({ left: rail(dag), right: rail({ block: "d" }) });
  });

  test("adds a panel to the rail of the panel that `near` names, within", () => {
    expect(placedAt(overview, { block: "d" }, { near: dag }, "distribute").rails?.left?.panels).toEqual([dag, { block: "d" }]);
  });

  test("places a panel at an edge of the whole grid where `side` comes without `near`, evening out the top split", () => {
    for (const sizing of ["split", "distribute"] as const) {
      const right = placedAt(overview, { block: "d" }, { side: "right" }, sizing);
      expect(right.children).toEqual([...overview.children, group({ block: "d" })]);
      expect(right.sizes).toEqual([1 / 3, 1 / 3, 1 / 3]);
    }

    const above = placedAt(overview, { block: "d" }, { side: "above" }, "distribute");
    expect(above).toEqual({
      orientation: "vertical",
      children: [group({ block: "d" }), { children: overview.children, sizes: overview.sizes }],
      sizes: [0.5, 0.5],
      rails: overview.rails,
    });
  });

  test("places a panel at an edge beside a top split that holds one tab group alone, or none", () => {
    const one: Grid = { orientation: "horizontal", children: [group(a)], sizes: [1] };
    expect(placedAt(one, b, { side: "below" }, "distribute")).toEqual({
      orientation: "vertical",
      children: [group(a), group(b)],
      sizes: [0.5, 0.5],
    });
    const none: Grid = { orientation: "horizontal", children: [], sizes: [] };
    expect(placedAt(none, b, { side: "above" }, "distribute")).toEqual({ orientation: "vertical", children: [group(b)], sizes: [1] });
  });

  test("places a panel at its default place, without `near` or `rail`, or within alone", () => {
    expect(placedAt(overview, { block: "d" }, {}, "distribute")).toEqual(placePanel(overview, { block: "d" }));
    expect(placedAt(overview, { block: "d" }, { side: "within" }, "distribute")).toEqual(placePanel(overview, { block: "d" }));
  });

  test("throws where `near` names a panel that the grid lacks, or one on a rail beside which it places", () => {
    expect(() => placedAt(overview, { block: "d" }, { near: { block: "z" } }, "distribute")).toThrow(
      'The view does not show the panel of the block "z", which near names.',
    );
    expect(() => placedAt(overview, { block: "d" }, { near: dag, side: "below" }, "distribute")).toThrow("on a rail");
    expect(placeFailure(overview, { near: dag, side: "below", rail: "right" })).toBeUndefined();
  });
});

describe("showing()", () => {
  test("has the tab group or rail that holds a panel show it", () => {
    expect(showing(overview, a).children[0]).toEqual(group(a, b));
    const twoOnRail: Grid = { ...overview, rails: { left: rail(dag, { block: "d" }) } };
    expect(showing(twoOnRail, { block: "d" }).rails?.left?.active).toEqual({ block: "d" });
    expect(showing(overview, { block: "z" })).toBe(overview);
  });
});

describe("collapsedRail()", () => {
  test("collapses or opens the rail of an edge, and leaves a grid without one as it is", () => {
    expect(collapsedRail(overview, "left", true).rails?.left).toEqual({ ...rail(dag), collapsed: true });
    expect(collapsedRail(overview, "right", true)).toBe(overview);
  });
});

describe("resized()", () => {
  test("gives the tab group that holds a panel its share of its split, and the others the rest in proportion", () => {
    const grid: Grid = { orientation: "horizontal", children: [group(a), group(b), group(c)], sizes: [0.2, 0.3, 0.5] };
    const sizes = resized(grid, b, 0.5).sizes;
    // The other two had 0.2 and 0.5, so they share the half that is left
    // two to five.
    expect(sizes[1]).toBe(0.5);
    expect(sizes[0]).toBeCloseTo(0.5 * (2 / 7), 12);
    expect(sizes[2]).toBeCloseTo(0.5 * (5 / 7), 12);
    expect(gridFailures(resized(grid, b, 0.5))).toEqual([]);
    expect(resized(overview, md, 0.25).children[1]).toEqual({ children: [group(c), group(md)], sizes: [0.75, 0.25] });
  });

  test("sets the width of the rail that holds a panel", () => {
    expect(resized(overview, dag, 320).rails?.left).toEqual({ ...rail(dag), size: 320 });
  });

  test("throws for a share that is not below 1, and for a tab group that fills its split alone", () => {
    expect(() => resized(overview, a, 1)).toThrow("below 1");
    const one: Grid = { orientation: "horizontal", children: [group(a)], sizes: [1] };
    expect(() => resized(one, a, 0.5)).toThrow('The tab group that holds the panel of the block "a" fills its split alone');
  });
});

describe("gridChanges()", () => {
  const none = { panels: false, arrangement: false, sizes: false };

  test("finds no change where only the panel that a tab group or rail shows, or a rail's collapse, changes", () => {
    const grid = collapsedRail(showing(overview, a), "left", true);
    expect(gridChanges(overview, grid)).toEqual(none);
  });

  test("finds other panels, which change the arrangement too", () => {
    expect(gridChanges(overview, placedAt(overview, { block: "d" }, { near: a }, "distribute"))).toEqual({ ...none, panels: true, arrangement: true });
  });

  test("finds another arrangement of the same panels, with new shares that it does not compare", () => {
    const moved = placedAt(prunePanels(overview, (panel) => panel !== c), c, { near: a, side: "below" }, "split");
    expect(gridChanges(overview, moved)).toEqual({ ...none, arrangement: true });
    const swapped: Grid = { ...overview, children: [{ panels: [b, a], active: b }, overview.children[1] ?? group(c)] };
    expect(gridChanges(overview, swapped)).toEqual({ ...none, arrangement: true });
  });

  test("finds other shares in the same splits, and other widths of a rail that both grids hold", () => {
    expect(gridChanges(overview, { ...overview, sizes: [0.4, 0.6] })).toEqual({ ...none, sizes: true });
    expect(gridChanges(overview, resized(overview, dag, 300))).toEqual({ ...none, sizes: true });
    expect(gridChanges(overview, { ...overview, sizes: [0.3 + 1e-12, 0.7 - 1e-12] })).toEqual(none);
  });
});
