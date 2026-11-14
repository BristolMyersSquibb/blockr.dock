// The conversions between a view's grid and dockview's layout, and the ids by
// which dockview knows panels and rails.
// Some tests draw the layout with dockview itself, on happy-dom, which lays
// it out at the size that the test gives.

import { createDockview, Orientation } from "dockview-react";
import type { DockviewApi, IContentRenderer } from "dockview-react";
import { describe, expect, test } from "vitest";
import type { Grid, Panel, Rail, TabGroup } from "../state/index.ts";
import { gridToLayout, layoutToGrid, PANEL_COMPONENT, panelId, panelOf, railId } from "./index.ts";

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

// The view Overview of The dock's state: a tab group of a and b, which shows
// b, in its left 30%, and in the rest c above blockr.md's document, with the
// DAG on a rail at its left edge.
const overview: Grid = {
  orientation: "horizontal",
  children: [
    { panels: [a, b], active: b },
    { children: [group(c), group(md)], sizes: [0.6, 0.4] },
  ],
  sizes: [0.3, 0.7],
  rails: { left: rail(dag) },
};

// The title of every panel's tab: the block's id or the plugin class's id.
const named = (panel: Panel) => ("block" in panel ? panel.block : panel.plugin);

const SIZE = { width: 1000, height: 800 };

// Dockview in an element of its own, laid out at the size given, with a
// content component that records each panel that it makes and disposes.
function dockview(size = SIZE) {
  const el = document.createElement("div");
  document.body.append(el);
  const made: string[] = [];
  const disposed: string[] = [];
  const api: DockviewApi = createDockview(el, {
    createComponent: (options): IContentRenderer => {
      made.push(options.id);
      return {
        element: document.createElement("div"),
        init: () => undefined,
        dispose: () => {
          disposed.push(options.id);
        },
      };
    },
  });
  api.layout(size.width, size.height, true);
  return { api, made, disposed };
}

describe("panelId()", () => {
  test("names a panel by the JSON of its block or its plugin class", () => {
    expect(panelId(a)).toBe('{"block":"a"}');
    expect(panelId(dag)).toBe('{"plugin":"blockr.dag-dag_plugin"}');
    expect(panelOf(panelId(dag))).toEqual(dag);
  });

  test("names the panel of a block whose id holds any character", () => {
    const odd = { block: 'a "quoted"/~id' };
    expect(panelOf(panelId(odd))).toEqual(odd);
  });

  test("leaves a dockview id that names no panel without one", () => {
    for (const id of ["1", "group-1", "{}", '{"block":1}', '{"block":"a","plugin":"p"}', "[]", "null"]) {
      expect(panelOf(id)).toBeUndefined();
    }
  });

  test("gives each rail's group the id of its edge", () => {
    expect(railId("left")).toBe("rail-left");
    expect(railId("right")).toBe("rail-right");
  });
});

describe("gridToLayout()", () => {
  test("gives a group for each tab group, an edge group for each rail and a panel for each panel", () => {
    const layout = gridToLayout(overview, named, SIZE);

    expect(layout.grid.orientation).toBe(Orientation.HORIZONTAL);
    expect(layout.grid.root).toEqual({
      type: "branch",
      size: 1,
      data: [
        { type: "leaf", size: 0.3, data: { id: "group-1", views: [panelId(a), panelId(b)], activeView: panelId(b) } },
        {
          type: "branch",
          size: 0.7,
          data: [
            { type: "leaf", size: 0.6, data: { id: "group-2", views: [panelId(c)], activeView: panelId(c) } },
            { type: "leaf", size: 0.4, data: { id: "group-3", views: [panelId(md)], activeView: panelId(md) } },
          ],
        },
      ],
    });
    expect(layout.panels[panelId(b)]).toEqual({ id: panelId(b), contentComponent: PANEL_COMPONENT, title: "b", params: { panel: b } });
    expect(Object.keys(layout.panels)).toEqual([a, b, c, md, dag].map(panelId));
  });

  test("always gives both rails an edge group, which shows only while it holds panels", () => {
    expect(gridToLayout(overview, named, SIZE).edgeGroups).toEqual({
      left: {
        size: 260,
        visible: true,
        collapsed: false,
        collapsedSize: 35,
        group: { id: "rail-left", views: [panelId(dag)], activeView: panelId(dag) },
      },
      right: { size: 260, visible: false, collapsed: false, collapsedSize: 35, group: { id: "rail-right", views: [] } },
    });
  });

  test("gives a split that divides the height dockview's vertical orientation", () => {
    const grid: Grid = { orientation: "vertical", children: [group(a), group(b)], sizes: [0.5, 0.5] };
    expect(gridToLayout(grid, named, SIZE).grid.orientation).toBe(Orientation.VERTICAL);
  });

  test("draws a grid that shows nothing as an empty layout", () => {
    const layout = gridToLayout({ orientation: "horizontal", children: [], sizes: [] }, named, SIZE);
    expect(layout.grid.root).toEqual({ type: "branch", size: 1, data: [] });
    expect(layout.panels).toEqual({});
  });

  test("has a tab group show its first panel where its active panel is none of its panels", () => {
    const grid: Grid = { orientation: "horizontal", children: [{ panels: [a, b], active: c }], sizes: [1] };
    expect(gridToLayout(grid, named, SIZE).grid.root.data).toMatchObject([{ data: { activeView: panelId(a) } }]);
  });

  test("divides a split evenly where its sizes do not give each child a positive share", () => {
    for (const sizes of [[0.5], [0, 1], [-1, 2], [Number.NaN, 1]]) {
      const grid: Grid = { orientation: "horizontal", children: [group(a), group(b)], sizes };
      expect(gridToLayout(grid, named, SIZE).grid.root.data).toMatchObject([{ size: 0.5 }, { size: 0.5 }]);
    }
  });

  test("shows a panel that the grid places twice where it comes first, its tab groups before its rails", () => {
    const grid: Grid = {
      orientation: "horizontal",
      children: [group(a, b), { panels: [b, c], active: b }],
      sizes: [0.5, 0.5],
      rails: { left: rail(a, dag) },
    };
    const layout = gridToLayout(grid, named, SIZE);
    expect(layout.grid.root.data).toMatchObject([
      { data: { views: [panelId(a), panelId(b)] } },
      { data: { views: [panelId(c)], activeView: panelId(c) } },
    ]);
    expect(layout.edgeGroups?.left?.group).toEqual({ id: "rail-left", views: [panelId(dag)], activeView: panelId(dag) });
  });

  test("leaves out each panel that it cannot draw, and each tab group, split and rail that this empties", () => {
    const drawn = (panel: Panel) => ("plugin" in panel ? undefined : named(panel));
    const layout = gridToLayout(overview, drawn, SIZE);
    expect(layout.grid.root.data).toEqual([
      { type: "leaf", size: 0.3, data: { id: "group-1", views: [panelId(a), panelId(b)], activeView: panelId(b) } },
      { type: "branch", size: 0.7, data: [{ type: "leaf", size: 1, data: { id: "group-2", views: [panelId(c)], activeView: panelId(c) } }] },
    ]);
    expect(layout.edgeGroups?.left).toMatchObject({ visible: false, group: { views: [] } });
  });

  test("gives the siblings of a split that it leaves out its place in proportion", () => {
    const grid: Grid = { orientation: "horizontal", children: [group(a), group(dag), group(b)], sizes: [0.2, 0.3, 0.5] };
    const layout = gridToLayout(grid, (panel) => ("plugin" in panel ? undefined : named(panel)), SIZE);
    expect(layout.grid.root.data).toMatchObject([{ size: 0.2 / 0.7 }, { size: 0.5 / 0.7 }]);
  });

  test("gives a rail that the grid collapsed its collapse and its widths", () => {
    const grid: Grid = { ...overview, rails: { right: { ...rail(dag), collapsed: true, size: 300, collapsed_size: 40 } } };
    expect(gridToLayout(grid, named, SIZE).edgeGroups?.right).toMatchObject({ size: 300, visible: true, collapsed: true, collapsedSize: 40 });
  });
});

describe("layoutToGrid()", () => {
  test("reads back the grid that gridToLayout() gives", () => {
    expect(layoutToGrid(gridToLayout(overview, named, SIZE))).toEqual(overview);
  });

  test("reads back what dockview draws of a grid, in shares of dockview's pixels", () => {
    const { api } = dockview();
    api.fromJSON(gridToLayout(overview, named, SIZE));
    expect(layoutToGrid(api.toJSON())).toEqual(overview);
  });

  test("gives even shares where dockview has laid nothing out yet", () => {
    const { api } = dockview({ width: 0, height: 0 });
    api.fromJSON(gridToLayout(overview, named, { width: 0, height: 0 }));
    const grid = layoutToGrid(api.toJSON());
    expect(grid.sizes).toEqual([0.5, 0.5]);
  });

  test("reads a root that is a group as the one tab group of the top split", () => {
    const layout = gridToLayout(overview, named, SIZE);
    const leaf = { type: "leaf" as const, size: 800, data: { id: "1", views: [panelId(a)], activeView: panelId(a) } };
    expect(layoutToGrid({ ...layout, grid: { ...layout.grid, root: leaf } })).toMatchObject({ children: [group(a)], sizes: [1] });
  });

  test("leaves out a group that holds no panel that the dock drew, and a split left empty", () => {
    const layout = gridToLayout(overview, named, SIZE);
    const empty = { type: "leaf" as const, size: 100, data: { id: "9", views: ["9"] } };
    const root = { ...layout.grid.root, data: [{ type: "branch" as const, size: 100, data: [empty] }, ...(layout.grid.root.data as object[])] };
    expect(layoutToGrid({ ...layout, grid: { ...layout.grid, root: root as typeof layout.grid.root } })).toMatchObject({
      children: overview.children,
    });
  });

  test("reads a rail's collapse and its width in whole pixels, and no rail on an edge group without panels", () => {
    const layout = gridToLayout(overview, named, SIZE);
    const left = { ...layout.edgeGroups?.left, size: 259.6, collapsed: true, collapsedSize: 35, visible: true, group: layout.edgeGroups?.left?.group };
    expect(layoutToGrid({ ...layout, edgeGroups: { left } }).rails).toEqual({ left: { ...rail(dag), collapsed: true, size: 260 } });
    expect(layoutToGrid({ ...layout, edgeGroups: {} }).rails).toBeUndefined();
  });
});

describe("drawing a layout with dockview", () => {
  test("keeps the panels that stay and disposes those that leave, as fromJSON() reuses panels", () => {
    const { api, made, disposed } = dockview();
    api.fromJSON(gridToLayout(overview, named, SIZE), { reuseExistingPanels: true });
    const before = api.getPanel(panelId(c));

    const moved: Grid = { orientation: "vertical", children: [group(a, b, c), group(md)], sizes: [0.5, 0.5] };
    api.fromJSON(gridToLayout(moved, named, SIZE), { reuseExistingPanels: true });

    expect(api.getPanel(panelId(c))).toBe(before);
    expect(made).toEqual([a, b, c, md, dag].map(panelId));
    expect(disposed).toEqual([panelId(dag)]);
    expect(layoutToGrid(api.toJSON())).toEqual({ ...moved, children: [{ ...group(a, b, c), active: a }, group(md)] });
    expect(api.isEdgeGroupVisible("left")).toBe(false);
    expect(api.isEdgeGroupVisible("right")).toBe(false);
  });
});
