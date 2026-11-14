// The dock plugin's state: its types, its default, the `update` of the dock
// plugin's class, and the helpers that the dock's commands share with it, for
// a view's grid and for the order of the navigation
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.1).

export type { DockState, Folder, FolderId, Grid, Node, Panel, Rail, Section, Split, TabGroup, View, ViewId } from "./types.ts";
export { defaultState, isDockState } from "./state.ts";
export { rehoused, update } from "./update.ts";
export {
  canonicalGrid,
  collapsedRail,
  EDGES,
  gridChanges,
  gridFailures,
  gridPanels,
  isTabGroup,
  locate,
  panelName,
  placedAt,
  placePanel,
  places,
  prunePanels,
  resized,
  samePanel,
  showing,
  SIDES,
  withStoredShares,
} from "./grid.ts";
export type { Edge, GridChanges, Place } from "./grid.ts";
export { compare, firstView, inFolder, keyIn, sameEntry, sitsAt, viewOrder, within } from "./navigation.ts";
export type { Entry } from "./navigation.ts";
export { patched, pointer } from "./patch.ts";
