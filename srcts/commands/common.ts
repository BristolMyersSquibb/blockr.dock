// What the dock's commands share: where the board holds the dock's state, how
// a command reads it and writes patches into it, the checks of what they are
// given, how a command reads what a locked board keeps of a view's grid, and
// how far it lets a grid that it is given move the shares of a split.

import type { Board, Command, CommandContext, Delta, Json, JsonSchema } from "@blockr/core";
import { logicalOption, numericOption } from "../options/index.ts";
import { gridFailures, gridPanels, inFolder, isDockState, pointer, sameEntry } from "../state/index.ts";
import type { DockState, Entry, FolderId, Grid, Panel } from "../state/index.ts";

// The id of the dock plugin's class, under which the board holds the dock's
// state, at /plugins/blockr.dock-dock_plugin.
export const DOCK = "blockr.dock-dock_plugin";

// The dock's state on the board, or undefined where the board holds none of
// its shape, as where the app does not run the dock.
export function stateOf(board: Board): DockState | undefined {
  const state = board.plugins[DOCK];
  return isDockState(state) ? state : undefined;
}

// The JSON Pointer from the board's root to the segments given, inside the
// dock's state.
export function at(...segments: string[]): string {
  return pointer(["plugins", DOCK, ...segments]);
}

// A command of the dock's, whose args are an A, as its schema has them.
export interface DockCommand<A, P = A> {
  readonly id: string;
  readonly label: string;
  readonly description: string;
  readonly args: JsonSchema;
  readonly undo?: false;
  enabled?(board: Board, args: A, context: CommandContext): boolean;
  prepare?(board: Board, args: A, context: CommandContext): Promise<P>;
  run(board: Board, args: P, context: CommandContext): Delta | readonly Delta[];
}

// A command of the dock's, as core registers it.
// The command registry hands enabled(), prepare() and run() only args that
// the command's schema takes, so they read them as an A, which TypeScript
// cannot take for a Json where it has optional properties.
export function command<A, P = A>(definition: DockCommand<A, P>): Command<Json, unknown> {
  return definition as unknown as Command<Json, unknown>;
}

// Whether a folder lists the view or folder that `before` names, where a
// view or folder can go before it, or `before` names none.
// It cannot go before itself.
export function fits(state: DockState, folder: FolderId | null, before: Entry | undefined, moving?: Entry): boolean {
  if (before === undefined) return true;
  if (moving !== undefined && sameEntry(before, moving)) return false;
  return inFolder(state, folder).some((entry) => sameEntry(entry, before));
}

// Whether a folder is one that the state holds, or the top, null.
export function folderOrTop(state: DockState, folder: FolderId | null): boolean {
  return folder === null || Object.hasOwn(state.folders, folder);
}

// The failures of a grid that a command is given: those of gridFailures(),
// and the panel of each block that the board lacks, in a sentence each.
export function givenGridFailures(board: Board, grid: Grid): string[] {
  const lacking = gridPanels(grid).flatMap((panel) =>
    "block" in panel && !Object.hasOwn(board.blocks, panel.block) ? [panel.block] : [],
  );
  return [...gridFailures(grid), ...lacking.map((block) => `The grid places the panel of the block "${block}", which the board does not hold.`)];
}

// Throws an Error that says the failures, where there are any.
export function refuse(failures: readonly string[]): void {
  if (failures.length > 0) throw new Error(failures.join(" "));
}

// Whether the board holds the block, where a panel names one.
export function holdsBlock(board: Board, panel: Panel): boolean {
  return !("block" in panel) || Object.hasOwn(board.blocks, panel.block);
}

// What a locked board keeps of a view's grid: its arrangement, while the
// blockr option lock_rearrange is TRUE, and its sizes, while the blockr
// option lock_resize is TRUE, each TRUE unless set, as today's
// is_layout_locked() reads them.
// A board without the board lock keeps neither, and reads no blockr option.
export interface LayoutLocks {
  readonly rearrange: boolean;
  readonly resize: boolean;
}

export async function layoutLocks(context: CommandContext): Promise<LayoutLocks> {
  if (!context.lock.board) return { rearrange: false, resize: false };
  const read = (option: string) => logicalOption(context, option, true, "where it takes TRUE or FALSE");
  const [rearrange, resize] = await Promise.all([read("lock_rearrange"), read("lock_resize")]);
  return { rearrange, resize };
}

// How far a grid that a command is given may move a share of a split, which
// then keeps its stored shares: the blockr option dock_grid_size_tol, or
// 0.005 unless set, as today's grid_size_tol() reads it.
export async function gridSizeTolerance(context: CommandContext): Promise<number> {
  return await numericOption(context, "dock_grid_size_tol", 0.005, "where it takes a number of 0 or more");
}

// "View N" or "Folder N", with N one more than the names given, or the first
// N after it that no name given has, as today's next_view_name() names a new
// view.
export function nextName(word: string, taken: readonly string[]): string {
  let n = taken.length + 1;
  while (taken.includes(`${word} ${String(n)}`)) n += 1;
  return `${word} ${String(n)}`;
}
