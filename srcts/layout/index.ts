// How the dock draws a view's grid with dockview and reads dockview's layout
// back into a grid: the conversions between the two, the ids that dockview
// knows panels by, the narrow column, and what the dock runs after a gesture
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.1#drawing-a-view).

export { columnRuns, gestureRuns } from "./gestures.ts";
export type { Reading, Run } from "./gestures.ts";
export { panelId, panelOf, railId } from "./ids.ts";
export { gridToLayout, layoutToGrid, PANEL_COMPONENT } from "./layout.ts";
export type { PanelParams, Size } from "./layout.ts";
export { stackGrid } from "./stack.ts";
export type { Column } from "./stack.ts";
