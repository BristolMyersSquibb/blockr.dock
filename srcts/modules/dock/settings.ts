// The blockr options that the dock's frontend reads as it starts on a board,
// which R holds for the page.
// Each value that R holds but the option does not take fails the frontend's
// render(), with a message that names the value as R sent it, so that a
// deployment that sets an option learns of its typo, as today's dock fails
// in R.

import type { CoreApi } from "@blockr/core";
import { gridSizeTolerance, layoutLocks } from "../../commands/index.ts";

export interface Settings {
  // How far a share may move before it counts as resized, the blockr option
  // dock_grid_size_tol, 0.005 unless set.
  readonly tolerance: number;
  // Whether the board lock keeps the sizes of the splits and rails, and where
  // each panel sits, as the blockr options lock_resize and lock_rearrange
  // say, each true unless set, and false on a board without the board lock.
  readonly lockResize: boolean;
  readonly lockRearrange: boolean;
  // The width of the window in CSS pixels below which the dock draws the
  // narrow column, the blockr option narrow_breakpoint: Infinity to draw it
  // at every width, or undefined to draw it at none, as where nothing sets
  // the option.
  readonly breakpoint: number | undefined;
  // The most of the window's height that one row of the narrow column takes,
  // the blockr option narrow_group_fraction, 0.8 unless set.
  readonly fraction: number;
}

export async function readSettings(core: Pick<CoreApi, "lock" | "blockrOption">): Promise<Settings> {
  const [tolerance, locks, breakpoint, fraction] = await Promise.all([
    gridSizeTolerance(core),
    layoutLocks(core),
    narrowBreakpoint(core),
    groupFraction(core),
  ]);
  return { tolerance, lockResize: locks.resize, lockRearrange: locks.rearrange, breakpoint, fraction };
}

// A breakpoint below this is a typo rather than a layout, as today's
// narrow_breakpoint_floor in R/utils-dock.R has it, at 655e323.
const BREAKPOINT_FLOOR = 50;

// The blockr option narrow_breakpoint, as Settings has it.
// R writes Inf as null, so null reads as Infinity.
// A string, as an environment variable gives, reads as the number that it
// holds, or as Infinity for "Inf".
async function narrowBreakpoint(core: Pick<CoreApi, "blockrOption">): Promise<number | undefined> {
  const sent = await core.blockrOption("narrow_breakpoint");
  if (sent === undefined) return undefined;
  const value = sent === null ? Infinity : typeof sent === "string" ? numberIn(sent) : sent;
  if (typeof value === "number" && value >= BREAKPOINT_FLOOR) return value;
  throw new TypeError(
    `The blockr option narrow_breakpoint is ${JSON.stringify(sent)}, where it takes a number of at least ${String(BREAKPOINT_FLOOR)} or Inf.`,
  );
}

// The blockr option narrow_group_fraction, as Settings has it.
// A string reads as the number that it holds.
async function groupFraction(core: Pick<CoreApi, "blockrOption">): Promise<number> {
  const sent = await core.blockrOption("narrow_group_fraction");
  if (sent === undefined) return 0.8;
  const value = typeof sent === "string" ? numberIn(sent) : sent;
  if (typeof value === "number" && value > 0 && value <= 1) return value;
  throw new TypeError(`The blockr option narrow_group_fraction is ${JSON.stringify(sent)}, where it takes a number above 0 and at most 1.`);
}

// The number that a string holds, with "Inf" as Infinity, as R's as.numeric()
// reads it, or NaN where it holds none.
function numberIn(text: string): number {
  const trimmed = text.trim();
  if (trimmed === "Inf") return Infinity;
  return trimmed === "" ? Number.NaN : Number(trimmed);
}
