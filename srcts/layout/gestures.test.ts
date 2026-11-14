// What the dock runs after a gesture: the commands that write into the dock's
// state what the gesture changed.

import { describe, expect, test } from "vitest";
import type { Grid, Panel, Rail, TabGroup } from "../state/index.ts";
import { columnRuns, gestureRuns } from "./index.ts";
import type { Reading } from "./index.ts";

const a = { block: "a" };
const b = { block: "b" };
const c = { block: "c" };
const dag = { plugin: "blockr.dag-dag_plugin" };

function group(...panels: Panel[]): TabGroup {
  const [first] = panels;
  if (first === undefined) throw new Error("A tab group holds a panel.");
  return { panels, active: first };
}

function rail(...panels: Panel[]): Rail {
  return { ...group(...panels), collapsed: false, size: 260, collapsed_size: 35 };
}

const stored: Grid = {
  orientation: "horizontal",
  children: [group(a, b), group(c)],
  sizes: [0.3, 0.7],
  rails: { left: rail(dag) },
};

// A gesture that was no drag, as a click on a tab, and a drag of a sash.
const click: Reading = { shares: false, widths: false, tolerance: 0.005 };
const drag: Reading = { shares: true, widths: true, tolerance: 0.005 };

describe("gestureRuns()", () => {
  test("runs nothing where nothing changed", () => {
    expect(gestureRuns("v", stored, stored, stored, click)).toEqual([]);
  });

  test("runs blockr.dock-activate_panel where only the panel that a tab group shows changed", () => {
    const read: Grid = { ...stored, children: [{ ...group(a, b), active: b }, group(c)] };
    expect(gestureRuns("v", stored, read, stored, click)).toEqual([{ id: "blockr.dock-activate_panel", args: { view: "v", panel: b } }]);
  });

  test("runs blockr.dock-collapse_rail where only whether a rail is collapsed changed", () => {
    const read: Grid = { ...stored, rails: { left: { ...rail(dag), collapsed: true } } };
    expect(gestureRuns("v", stored, read, stored, click)).toEqual([
      { id: "blockr.dock-collapse_rail", args: { view: "v", edge: "left", collapsed: true } },
    ]);
  });

  test("runs both where a click on a collapsed rail's other tab shows it and opens the rail", () => {
    const shown: Grid = { ...stored, rails: { left: { ...rail(dag, c), collapsed: true } } };
    const read: Grid = { ...stored, rails: { left: { ...rail(dag, c), active: c } } };
    expect(gestureRuns("v", shown, read, shown, click)).toEqual([
      { id: "blockr.dock-activate_panel", args: { view: "v", panel: c } },
      { id: "blockr.dock-collapse_rail", args: { view: "v", edge: "left", collapsed: false } },
    ]);
  });

  test("runs blockr.dock-set_grid with the new grid where a panel moved", () => {
    const read: Grid = { ...stored, children: [{ ...group(a, b, c), active: c }], sizes: [1] };
    expect(gestureRuns("v", stored, read, stored, click)).toEqual([{ id: "blockr.dock-set_grid", args: { view: "v", grid: read } }]);
  });

  test("runs blockr.dock-set_grid where a panel was closed", () => {
    const read: Grid = { ...stored, children: [group(a), group(c)] };
    expect(gestureRuns("v", stored, read, stored, click)).toEqual([{ id: "blockr.dock-set_grid", args: { view: "v", grid: read } }]);
  });

  test("runs blockr.dock-set_grid where a drag moved a share by more than the tolerance", () => {
    const read: Grid = { ...stored, sizes: [0.4, 0.6] };
    expect(gestureRuns("v", stored, read, stored, drag)).toEqual([{ id: "blockr.dock-set_grid", args: { view: "v", grid: read } }]);
  });

  test("keeps the shares that a drag moved by no more than the tolerance", () => {
    const read: Grid = { ...stored, sizes: [0.302, 0.698] };
    expect(gestureRuns("v", stored, read, stored, drag)).toEqual([]);
  });

  test("keeps the shares where the gesture was no drag, as a window resize moves them", () => {
    const read: Grid = { ...stored, sizes: [0.4, 0.6] };
    expect(gestureRuns("v", stored, read, stored, click)).toEqual([]);
  });

  test("keeps the shares that dockview showed in a grid that a drop changed elsewhere", () => {
    const shown: Grid = { ...stored, sizes: [0.3011, 0.6989] };
    const read: Grid = { ...shown, children: [group(a, b), group(c, { block: "d" })] };
    expect(gestureRuns("v", shown, read, stored, click)).toEqual([
      { id: "blockr.dock-set_grid", args: { view: "v", grid: { ...read, sizes: [0.3011, 0.6989] } } },
    ]);
  });

  test("runs blockr.dock-set_grid where a drag of the rail's sash widened the rail", () => {
    const read: Grid = { ...stored, rails: { left: { ...rail(dag), size: 400 } } };
    expect(gestureRuns("v", stored, read, stored, drag)).toEqual([{ id: "blockr.dock-set_grid", args: { view: "v", grid: read } }]);
  });

  test("keeps a rail's stored widths where the dock's own width changed, as dockview narrows a rail that it cannot hold", () => {
    const read: Grid = { ...stored, rails: { left: { ...rail(dag), size: 120 } } };
    expect(gestureRuns("v", read, read, stored, { ...drag, widths: false })).toEqual([]);
    const moved: Grid = { ...read, children: [group(a), group(b, c)] };
    expect(gestureRuns("v", read, moved, stored, click)).toEqual([
      { id: "blockr.dock-set_grid", args: { view: "v", grid: { ...moved, rails: { left: rail(dag) } } } },
    ]);
  });

  test("keeps the stored width of a rail that dockview narrowed before, through a drag of another sash", () => {
    const shown: Grid = { ...stored, rails: { left: { ...rail(dag), size: 120 } } };
    const read: Grid = { ...shown, sizes: [0.4, 0.6] };
    expect(gestureRuns("v", shown, read, stored, drag)).toEqual([
      { id: "blockr.dock-set_grid", args: { view: "v", grid: { ...read, rails: { left: rail(dag) } } } },
    ]);
  });

  test("keeps a rail's stored collapsed width, which no gesture changes", () => {
    const shown: Grid = { ...stored, rails: { left: { ...rail(dag), collapsed_size: 44 } } };
    const read: Grid = { ...shown, children: [group(a), group(b, c)] };
    expect(gestureRuns("v", shown, read, stored, click)).toEqual([
      { id: "blockr.dock-set_grid", args: { view: "v", grid: { ...read, rails: { left: rail(dag) } } } },
    ]);
  });

  test("takes a rail that a drop opened as dockview gives it", () => {
    const read: Grid = { ...stored, children: [group(a, b)], sizes: [1], rails: { ...stored.rails, right: rail(c) } };
    expect(gestureRuns("v", stored, read, stored, click)).toEqual([{ id: "blockr.dock-set_grid", args: { view: "v", grid: read } }]);
  });
});

describe("columnRuns()", () => {
  const column: Grid = { orientation: "vertical", children: [group(a, b), group(c), group(dag)], sizes: [0.4, 0.4, 0.2] };

  test("runs nothing where nothing changed", () => {
    expect(columnRuns("v", column, column)).toEqual([]);
  });

  test("runs blockr.dock-activate_panel for a panel that a row shows now", () => {
    const read: Grid = { ...column, children: [{ ...group(a, b), active: b }, group(c), group(dag)] };
    expect(columnRuns("v", column, read)).toEqual([{ id: "blockr.dock-activate_panel", args: { view: "v", panel: b } }]);
  });

  test("runs blockr.dock-remove_panel for a panel that a row lost, then shows the one that its row shows now", () => {
    const read: Grid = { ...column, children: [{ ...group(b), active: b }, group(c), group(dag)] };
    expect(columnRuns("v", column, read)).toEqual([
      { id: "blockr.dock-remove_panel", args: { view: "v", panel: a } },
      { id: "blockr.dock-activate_panel", args: { view: "v", panel: b } },
    ]);
  });

  test("reads nothing else back, as the shares of the column's rows", () => {
    expect(columnRuns("v", column, { ...column, sizes: [0.2, 0.2, 0.6] })).toEqual([]);
  });
});
