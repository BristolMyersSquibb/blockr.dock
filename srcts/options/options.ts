// The dock's board options, each an instance of core's Option without its id,
// which the module of the board option in ../modules exports as its default,
// and which core registers under the id that R's reply to `hello` gives it
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.13#defining-a-board-option):
// compact, which turns each block card's header compact, view_tabs, which
// shows the views as a line of tabs, and sync_rails, which opens and collapses
// a rail on every view at once. Each keeps the label and the category of
// today's R constructor, and takes its schema from ./schemas.ts and its UI
// module from ./ui.tsx.
//
// Each default is a function of CoreApi that reads the blockr option of the
// board option's name, which today's R constructor reads, through
// CoreApi.blockrOption(), so that a deployment still sets the default, and
// takes today's fallback only where nothing sets the blockr option.
// Core calls it only where a board lacks the board option's value, as every
// board does that opens without one, since the dock plugin's class requires
// the three.
// A string, as an environment variable gives, reads as the logical that R's
// as.logical() reads it as, and where R holds any other value, the default
// fails, naming the value as R sent it, as the defaults of core's board
// options do, so that no reader gets the fallback in place of the value that
// was set.

import type { CoreApi, Json, Option, OptionId } from "@blockr/core";
import { optionSchemas } from "./schemas.ts";
import { checkboxUi } from "./ui.tsx";

// What a board option's default reads of CoreApi.
export type BlockrOptions = Pick<CoreApi, "blockrOption">;

// Whether each block card shows a compact header, an eyebrow over the block's
// content.
export const compact: Omit<Option, "id"> = {
  label: "Compact",
  category: "Theme options",
  schema: optionSchemas.compact,
  default: logicalDefault("compact", false),
  ui: checkboxUi("Compact"),
};

// Whether the views show as a line of tabs under the navbar.
export const viewTabs: Omit<Option, "id"> = {
  label: "Views as tabs",
  category: "Theme options",
  schema: optionSchemas.view_tabs,
  default: logicalDefault("view_tabs", false),
  ui: checkboxUi("Views as tabs"),
};

// Whether collapsing or opening a view's rail does so on every view at once.
export const syncRails: Omit<Option, "id"> = {
  label: "Sync rails across views",
  category: "Board options",
  schema: optionSchemas.sync_rails,
  default: logicalDefault("sync_rails", true),
  ui: checkboxUi("Sync rails across views"),
};

// The dock's board options by id, as blockr.dock lists them in R, each with
// its module.
export const dockOptions: Readonly<Record<OptionId, Omit<Option, "id">>> = {
  compact,
  view_tabs: viewTabs,
  sync_rails: syncRails,
};

// The default of a board option whose value is a logical: the blockr option
// of the board option's name, read as a logical, or `fallback` where nothing
// sets it.
function logicalDefault(id: OptionId, fallback: boolean): (core: BlockrOptions) => Promise<Json> {
  return (core) => logicalOption(core, id, fallback, `which the board option ${id} does not take`);
}

// A blockr option read as a logical, or `fallback` where nothing sets it, as
// the dock's commands read the blockr options that loosen the board lock.
// A string reads as the logical that R's as.logical() reads it as.
// Any other value that is no logical fails, with a message that names the
// value as R sent it and ends on `refused`.
export async function logicalOption(core: BlockrOptions, name: string, fallback: boolean, refused: string): Promise<boolean> {
  return await readOption(core, name, fallback, refused, (sent) => {
    const value = typeof sent === "string" ? asLogical(sent) : sent;
    return typeof value === "boolean" ? value : undefined;
  });
}

// A blockr option read as a number of 0 or more, or `fallback` where nothing
// sets it, as blockr.dock-set_grid reads dock_grid_size_tol.
// A string reads as the number that it holds.
// Any other value, and a number below 0, fails as in logicalOption().
export async function numericOption(core: BlockrOptions, name: string, fallback: number, refused: string): Promise<number> {
  return await readOption(core, name, fallback, refused, (sent) => {
    const value = typeof sent === "string" ? asNumber(sent) : sent;
    return typeof value === "number" && Number.isFinite(value) && value >= 0 ? value : undefined;
  });
}

// A blockr option as `read` reads the value that R sent, or `fallback` where
// nothing sets it.
// Where `read` gives undefined, it fails, with a message that names the value
// as R sent it and ends on `refused`.
async function readOption<T>(core: BlockrOptions, name: string, fallback: T, refused: string, read: (sent: Json) => T | undefined): Promise<T> {
  const sent = await core.blockrOption(name);
  if (sent === undefined) return fallback;
  const value = read(sent);
  if (value !== undefined) return value;
  throw new TypeError(`The blockr option ${name} is ${JSON.stringify(sent)}, ${refused}.`);
}

// The finite number that a string holds, or undefined where it holds none, as
// "", "a" and "Inf" do, as core reads the string that an environment variable
// gives, which core keeps to itself.
function asNumber(text: string): number | undefined {
  const number = text.trim() === "" ? Number.NaN : Number(text);
  return Number.isFinite(number) ? number : undefined;
}

// The logical that R's as.logical() reads a string as, or undefined where it
// reads NA, as core reads the string that an environment variable gives,
// which core keeps to itself.
function asLogical(text: string): boolean | undefined {
  const trimmed = text.trim();
  if (["TRUE", "true", "True", "T"].includes(trimmed)) return true;
  if (["FALSE", "false", "False", "F"].includes(trimmed)) return false;
  return undefined;
}
