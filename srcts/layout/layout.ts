// The conversions between a view's grid and dockview's layout
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.1#drawing-a-view).
// The function gridToLayout() turns a grid into the layout that dockview's
// fromJSON() draws, as today's grid_to_tree() and rails_to_edges() in
// R/dock-layout.R do, at 655e323.
// The function layoutToGrid() reads the layout that dockview's toJSON() gives
// back into a grid, as today's tree_to_grid() and edge_to_rail() do.
//
// A tab group becomes a dockview group, a rail a dockview edge group, and a
// panel a dockview panel, whose tab shows the block's name or the plugin's.
// A node takes its share of its split as its size, and the top split the size
// 1, which dockview lays out in proportion, as today's grid_to_tree() has it.
// The layout always holds the edge groups of both rails, since a view offers
// a rail on each edge.
// Each shows only while it holds panels.
// A rail that the grid lacks opens at the default widths once a panel goes
// onto it.
//
// The dock's commands write grids in their canonical form, which
// gridToLayout() draws as it is.
// A patch set with core's command can leave a grid otherwise, and
// gridToLayout() draws such a grid all the same:
// - A tab group or rail shows its first panel where `active` names none of
//   its panels.
// - A split divides evenly where its sizes do not give each child a positive
//   share.
// - A panel that the grid places twice shows where it comes first, in
//   reading order: the tab groups depth first, then the rails.
// - A tab group, split or rail that holds no panel that the dock draws is
//   left out, and its siblings share its place in proportion.
// It leaves out each panel that the dock cannot draw, as `title` says.

import { Orientation } from "dockview-react";
import type { GroupviewPanelState, SerializedDockview, SerializedEdgeGroups } from "dockview-react";
import { EDGES, fitted, isTabGroup, RAIL_COLLAPSED_SIZE, RAIL_SIZE, samePanel } from "../state/index.ts";
import type { Edge, Grid, Node, Panel, Rail, Split, TabGroup } from "../state/index.ts";
import { panelId, panelOf, railId } from "./ids.ts";

// The name of the component that draws the content of every panel, which the
// dock gives dockview.
export const PANEL_COMPONENT = "panel";

// The size of the dock in pixels, which dockview lays the layout out in.
export interface Size {
  readonly width: number;
  readonly height: number;
}

// What a panel's content component reads of the panel, from its params.
export interface PanelParams {
  readonly panel: Panel;
}

// A node of dockview's layout, the state of a dockview group in it, and the
// state of an edge group, which dockview does not export by name.
type GridObject = SerializedDockview["grid"]["root"];
type GroupPanelViewState = Exclude<GridObject["data"], readonly unknown[]>;
type SerializedEdgeGroup = NonNullable<SerializedEdgeGroups["left"]>;

// The layout that draws a grid, in a dock of the size given.
// The title of a panel's tab is the one that `title` gives, and a panel for
// which it gives undefined is left out.
export function gridToLayout(grid: Grid, title: (panel: Panel) => string | undefined, size: Size): SerializedDockview {
  const panels: Record<string, GroupviewPanelState> = {};
  let groups = 0;

  // The ids of the panels given that the dock draws, but for those that a tab
  // group or rail before took already.
  const take = (list: readonly Panel[]): string[] =>
    list.flatMap((panel) => {
      const id = panelId(panel);
      if (Object.hasOwn(panels, id)) return [];
      const name = title(panel);
      if (name === undefined) return [];
      const params: PanelParams = { panel: panelOf(id) ?? panel };
      panels[id] = { id, contentComponent: PANEL_COMPONENT, title: name, params };
      return [id];
    });

  // The panels that a tab group or rail shows, and the one that it shows.
  const shown = (group: TabGroup): { views: string[]; activeView?: string } => {
    const views = take(group.panels);
    const active = panelId(group.active);
    const activeView = views.includes(active) ? active : views[0];
    return activeView === undefined ? { views } : { views, activeView };
  };

  const node = (child: Node, share: number): GridObject | undefined => {
    if (isTabGroup(child)) {
      const state = shown(child);
      if (state.views.length === 0) return undefined;
      groups += 1;
      return { type: "leaf", size: share, data: { id: `group-${String(groups)}`, ...state } };
    }
    const data = split(child);
    return data.length === 0 ? undefined : { type: "branch", size: share, data };
  };

  // The nodes of the children of a split that hold a panel that the dock
  // draws, with their shares scaled to sum to 1 again.
  const split = (parent: Split): GridObject[] => {
    const shares = fitted(parent.sizes, parent.children.length);
    const kept = parent.children.flatMap((child, i) => node(child, shares[i] ?? 0) ?? []);
    const rescaled = fitted(
      kept.map((object) => object.size ?? 0),
      kept.length,
    );
    return kept.map((object, i) => ({ ...object, size: rescaled[i] ?? 0 }));
  };

  // The tab groups go first, so that a panel that the grid places twice shows
  // in its tab group rather than on a rail.
  const root = split(grid);

  const edgeGroup = (edge: Edge, rail: Rail | undefined): SerializedEdgeGroup => {
    const state = rail === undefined ? { views: [] } : shown(rail);
    const holds = state.views.length > 0;
    return {
      size: rail?.size ?? RAIL_SIZE,
      visible: holds,
      collapsed: holds && rail?.collapsed === true,
      collapsedSize: rail?.collapsed_size ?? RAIL_COLLAPSED_SIZE,
      group: { id: railId(edge), ...state } satisfies GroupPanelViewState,
    };
  };

  const edgeGroups: SerializedEdgeGroups = {};
  for (const edge of EDGES) edgeGroups[edge] = edgeGroup(edge, grid.rails?.[edge]);

  return {
    grid: {
      root: { type: "branch", size: 1, data: root },
      width: size.width,
      height: size.height,
      orientation: grid.orientation === "vertical" ? Orientation.VERTICAL : Orientation.HORIZONTAL,
    },
    panels,
    edgeGroups,
  };
}

// The grid that a layout draws.
// Each split takes the sizes of its children in pixels, scaled to sum to 1,
// or even shares where dockview has laid nothing out yet.
// A dockview group without a panel that the dock drew is left out, as is a
// split left empty.
// A rail is the edge group of an edge that holds a panel that the dock drew,
// with its width in whole pixels.
// A root that is a group, which dockview can give for a layout of one group,
// becomes the one tab group of the top split.
export function layoutToGrid(layout: SerializedDockview): Grid {
  const { root, orientation } = layout.grid;
  const top = Array.isArray(root.data) ? children(root.data) : children([root]);
  const rails: Partial<Record<Edge, Rail>> = {};
  for (const edge of EDGES) {
    const rail = railOf(layout.edgeGroups?.[edge]);
    if (rail !== undefined) rails[edge] = rail;
  }
  return {
    orientation: orientation === Orientation.VERTICAL ? "vertical" : "horizontal",
    ...top,
    ...(Object.keys(rails).length === 0 ? {} : { rails }),
  };
}

// The split that holds the nodes of the objects given, but for those that
// hold no panel that the dock drew.
function children(objects: readonly GridObject[]): Split {
  const kept = objects.flatMap((object) => {
    const node = nodeOf(object);
    return node === undefined ? [] : [{ node, size: object.size ?? 0 }];
  });
  return {
    children: kept.map(({ node }) => node),
    sizes: fitted(
      kept.map(({ size }) => size),
      kept.length,
    ),
  };
}

function nodeOf(object: GridObject): Node | undefined {
  if (!Array.isArray(object.data)) return groupOf(object.data);
  const split = children(object.data);
  return split.children.length === 0 ? undefined : split;
}

// The tab group of a dockview group's state, or undefined where the group
// holds no panel that the dock drew.
function groupOf(state: GroupPanelViewState): TabGroup | undefined {
  const panels = state.views.flatMap((id) => panelOf(id) ?? []);
  const [first] = panels;
  if (first === undefined) return undefined;
  const shown = state.activeView === undefined ? undefined : panelOf(state.activeView);
  const active = shown === undefined ? undefined : panels.find((panel) => samePanel(panel, shown));
  return { panels, active: active ?? first };
}

// The rail of an edge group, or undefined where it holds no panel that the
// dock drew.
function railOf(edgeGroup: SerializedEdgeGroup | undefined): Rail | undefined {
  if (edgeGroup === undefined || !isGroupState(edgeGroup.group)) return undefined;
  const group = groupOf(edgeGroup.group);
  if (group === undefined) return undefined;
  return {
    ...group,
    collapsed: edgeGroup.collapsed === true,
    size: Math.max(0, Math.round(edgeGroup.size)),
    collapsed_size: edgeGroup.collapsedSize ?? RAIL_COLLAPSED_SIZE,
  };
}

// Whether a value is a dockview group's state, as an edge group's toJSON()
// gives it, whose type dockview leaves open.
function isGroupState(value: unknown): value is GroupPanelViewState {
  if (typeof value !== "object" || value === null) return false;
  const { views, activeView } = value as { views?: unknown; activeView?: unknown };
  return (
    Array.isArray(views) &&
    views.every((view) => typeof view === "string") &&
    (activeView === undefined || typeof activeView === "string")
  );
}
