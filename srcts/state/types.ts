// The types of the dock plugin's state, which the board holds at
// /plugins/blockr.dock-dock_plugin
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.1#the-state).
// The build generates the JSON Schema of the state from DockState and writes
// it beside the dock's module, as inst/js/dock.schema.json.
// The store checks the state against that schema after every change, and R
// reads the same file to write a board.
// The comment on each type and property becomes its description in the
// schema, which a model reads too.
// Each is a `type` rather than an `interface`, so that a state is a Json.

/* eslint-disable @typescript-eslint/consistent-type-definitions -- an interface would not be assignable to Json */

import type { BlockId, PluginClassId } from "@blockr/core";

/** The id of a view, which never changes. */
export type ViewId = string;

/** The id of a folder, which never changes. */
export type FolderId = string;

/** The dock's views, the folders that group them, the active view and the sections that each block's card shows. */
export type DockState = {
  /** Every view, by its id. */
  readonly views: Readonly<Record<ViewId, View>>;
  /** Every folder, by its id. */
  readonly folders: Readonly<Record<FolderId, Folder>>;
  /** The view that the board opens on, or null while the state holds no view. */
  readonly active: ViewId | null;
  /** The sections that a block's card shows, by the block's id, for each card that hides a section. A card without an entry shows both. */
  readonly visible: Readonly<Record<BlockId, readonly Section[]>>;
};

/** A view: a grid of panels, with a name, in a place in the navigation. */
export type View = {
  /** The name that the user gives the view. */
  readonly name: string;
  /** The folder that the view sits in, or null at the top. */
  readonly folder: FolderId | null;
  /** The key that orders the view among the views and folders of its folder, compared as a string. */
  readonly index: string;
  /** Which panels the view shows, and where. */
  readonly grid: Grid;
};

/** A folder, which groups views and other folders in the navigation. */
export type Folder = {
  /** The name that the user gives the folder. */
  readonly name: string;
  /** The folder that the folder sits in, or null at the top. */
  readonly folder: FolderId | null;
  /** The key that orders the folder among the views and folders of its folder, compared as a string. */
  readonly index: string;
};

/** A section of a block's card. */
export type Section = "inputs" | "outputs";

/** Where a view places its panels: nested splits of tab groups, and a rail on each side. */
export type Grid = {
  /** How the top split divides. Each split inside divides the other way from the split that holds it. */
  readonly orientation: "horizontal" | "vertical";
  /** What the top split holds, in order. */
  readonly children: readonly Node[];
  /** The share of each child, ratios that sum to 1. */
  readonly sizes: readonly Share[];
  /** The rails of the view, by the edge that each sits on. */
  readonly rails?: {
    readonly left?: Rail;
    readonly right?: Rail;
  };
};

/**
 * The share of a split that one of its children takes, a ratio above 0.
 * @exclusiveMinimum 0
 */
type Share = number;

/** What a split holds: a split or a tab group. */
export type Node = Split | TabGroup;

/** A split, which divides the other way from the split that holds it. */
export type Split = {
  /** What the split holds, in order. */
  readonly children: readonly Node[];
  /** The share of each child, ratios that sum to 1. */
  readonly sizes: readonly Share[];
};

/** A tab group, with a tab for each of its panels. */
export type TabGroup = {
  /** Its panels, in the order of their tabs. */
  readonly panels: readonly Panel[];
  /** The panel that it shows. */
  readonly active: Panel;
};

/** A rail on an edge of the view, with a tab for each of its panels. */
export type Rail = {
  /** Its panels, in the order of their tabs. */
  readonly panels: readonly Panel[];
  /** The panel that it shows. */
  readonly active: Panel;
  /** Whether the rail shows its tabs alone. */
  readonly collapsed: boolean;
  /**
   * Its width in pixels while it is open, 260 by default.
   * @minimum 0
   */
  readonly size: number;
  /**
   * Its width in pixels while it is collapsed, 35 by default.
   * @minimum 0
   */
  readonly collapsed_size: number;
};

/** A panel: the card of one of the board's blocks, by the block's id, or the UI of one of the app's plugins, by the id of the plugin's class. */
export type Panel = { readonly block: BlockId } | { readonly plugin: PluginClassId };
