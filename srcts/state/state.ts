// The default of the dock plugin's state, and a check of a value's shape.
// The `update` checks with it what a delta's patches leave of the state.

import type { Json } from "@blockr/core";
import { isList, isObject } from "./json.ts";
import type { DockState, Folder, Grid, Node, Panel, Rail, Split, TabGroup, View } from "./types.ts";

// The state that the store gives a board that holds none for the dock.
// It holds no view: the default of a plugin class's state is one value for
// every board, so it cannot list a board's blocks
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.1#starting-on-a-board).
export const defaultState: DockState = { views: {}, folders: {}, active: null, visible: {} };

// Whether a value has the shape of the dock's state, as far as code reads it.
// Unlike the schema, it lets a share be 0 or a rail's width be negative, and
// it ignores properties that the types do not declare.
export function isDockState(value: Json | undefined): value is DockState {
  return (
    isObject(value) &&
    recordOf(value.views, isView) &&
    recordOf(value.folders, isFolder) &&
    (value.active === null || typeof value.active === "string") &&
    recordOf(value.visible, (sections) => listOf(sections, (s) => s === "inputs" || s === "outputs"))
  );
}

function isView(value: Json | undefined): value is View {
  return isFolder(value) && isGrid(value.grid);
}

function isFolder(value: Json | undefined): value is Folder & Readonly<Record<string, Json>> {
  return (
    isObject(value) &&
    typeof value.name === "string" &&
    (value.folder === null || typeof value.folder === "string") &&
    typeof value.index === "string"
  );
}

function isGrid(value: Json | undefined): value is Grid {
  if (!isObject(value)) return false;
  const { orientation, rails } = value;
  if (orientation !== "horizontal" && orientation !== "vertical") return false;
  const railsValid =
    rails === undefined ||
    (isObject(rails) && (rails.left === undefined || isRail(rails.left)) && (rails.right === undefined || isRail(rails.right)));
  return railsValid && isSplit(value);
}

function isNode(value: Json | undefined): value is Node {
  return isSplit(value) || isTabGroup(value);
}

function isSplit(value: Json | undefined): value is Split {
  return isObject(value) && listOf(value.children, isNode) && listOf(value.sizes, (size) => typeof size === "number");
}

function isTabGroup(value: Json | undefined): value is TabGroup {
  return isObject(value) && listOf(value.panels, isPanel) && isPanel(value.active);
}

function isRail(value: Json | undefined): value is Rail {
  if (!isObject(value)) return false;
  const { collapsed, size, collapsed_size } = value;
  return (
    typeof collapsed === "boolean" && typeof size === "number" && typeof collapsed_size === "number" && isTabGroup(value)
  );
}

// Whether a value names a panel: a block by its id or a plugin's class by its
// id, never both.
function isPanel(value: Json | undefined): value is Panel {
  if (!isObject(value)) return false;
  return (typeof value.block === "string") !== (typeof value.plugin === "string");
}

function recordOf(value: Json | undefined, item: (value: Json) => boolean): boolean {
  return isObject(value) && Object.values(value).every(item);
}

function listOf(value: Json | undefined, item: (value: Json) => boolean): boolean {
  return isList(value) && value.every(item);
}
