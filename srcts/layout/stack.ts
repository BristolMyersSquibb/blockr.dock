// The narrow layout of a view, which the dock draws in a window narrower than
// the blockr option narrow_breakpoint
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.1#drawing-a-view).
// It stacks the view's tab groups, in reading order, and then its rails in
// one column, as today's stack_grid() in R/dock-grid.R does, at 655e323.
// Each tab group is as tall as the wide grid makes it, as a share of the
// window's height: the product of the shares of the splits on its path that
// divide the height.
// No row is taller than `cap`, the blockr option narrow_group_fraction.
// A rail takes `cap`, since its width says nothing about a height.
// The column is as tall as its rows together, so that the page scrolls
// through them, rather than the window's height being divided among them.

import { EDGES, fitted, isTabGroup } from "../state/index.ts";
import type { Grid, Node, TabGroup } from "../state/index.ts";

// The column of a view: a grid whose top split holds one tab group for each
// row, one above the other, and its height, as a share of the window's
// height.
export interface Column {
  readonly grid: Grid;
  readonly height: number;
}

export function stackGrid(grid: Grid, cap: number): Column {
  const rows: { readonly group: TabGroup; readonly height: number }[] = [];

  // A split that divides the height gives each child its share of the split's
  // height, and one that divides the width gives each child the whole of it.
  const walk = (children: readonly Node[], sizes: readonly number[], vertical: boolean, height: number) => {
    const shares = fitted(sizes, children.length);
    children.forEach((child, i) => {
      const share = vertical ? height * (shares[i] ?? 0) : height;
      if (isTabGroup(child)) rows.push({ group: child, height: Math.min(share, cap) });
      else walk(child.children, child.sizes, !vertical, share);
    });
  };
  walk(grid.children, grid.sizes, grid.orientation === "vertical", 1);

  for (const edge of EDGES) {
    const rail = grid.rails?.[edge];
    if (rail !== undefined && rail.panels.length > 0) {
      rows.push({ group: { panels: rail.panels, active: rail.active }, height: cap });
    }
  }

  if (rows.length === 0) return { grid: { orientation: "vertical", children: [], sizes: [] }, height: cap };
  const heights = rows.map((row) => row.height);
  return {
    grid: { orientation: "vertical", children: rows.map((row) => row.group), sizes: fitted(heights, heights.length) },
    height: heights.reduce((total, height) => total + height, 0),
  };
}
