// The dock plugin's state: its types, its default, the `update` and the
// `migrate` of the dock plugin's class, and the helpers that the dock's
// commands share with them, for a view's grid and for the order of the
// navigation (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.1).

export type { DockState, Folder, FolderId, Grid, Node, Panel, Rail, Section, Split, TabGroup, View, ViewId } from "./types.ts";
export { defaultState, isDockState } from "./state.ts";
export { update } from "./update.ts";
export { migrate } from "./migrate.ts";
export {
  appendTabGroup,
  canonicalGrid,
  defaultGrid,
  EDGES,
  gridFailures,
  gridPanels,
  isTabGroup,
  panelName,
  placePanel,
  places,
  prunePanels,
  RAIL_COLLAPSED_SIZE,
  RAIL_SIZE,
  samePanel,
} from "./grid.ts";
export type { Edge } from "./grid.ts";
export { compare, firstView, inFolder, viewOrder } from "./navigation.ts";
export { patched, pointer } from "./patch.ts";
