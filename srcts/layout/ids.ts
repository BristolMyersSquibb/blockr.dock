// The ids by which dockview knows what the dock draws.
// Dockview needs a string id for each panel.
// A grid names a panel by an object, so the dock derives the id from it with
// JSON.stringify(), as {"block":"a"}
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.1#grids).
// The dockview group of a rail takes the id rail-left or rail-right, as
// today's rail_group_id() in R/dock-layout.R names it, at 655e323, so that a
// position names a rail without the tree.

import type { Edge, Panel } from "../state/index.ts";

// The dockview id of a panel.
// It holds the block's id or the plugin class's id alone, in that one key, so
// that two values that name the same panel give the same id.
export function panelId(panel: Panel): string {
  return "block" in panel ? JSON.stringify({ block: panel.block }) : JSON.stringify({ plugin: panel.plugin });
}

// The panel that a dockview id names, or undefined where the id names none, as
// the id of a panel that the dock did not draw.
export function panelOf(id: string): Panel | undefined {
  let value: unknown;
  try {
    value = JSON.parse(id);
  } catch {
    return undefined;
  }
  if (typeof value !== "object" || value === null || Array.isArray(value)) return undefined;
  const keys = Object.keys(value);
  if (keys.length !== 1) return undefined;
  const { block, plugin } = value as { block?: unknown; plugin?: unknown };
  if (typeof block === "string") return { block };
  if (typeof plugin === "string") return { plugin };
  return undefined;
}

// The dockview id of the group of the rail on an edge.
export function railId(edge: Edge): string {
  return `rail-${edge}`;
}
