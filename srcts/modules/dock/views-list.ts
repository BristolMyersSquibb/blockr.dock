// The rows of a menu that lists the views and folders of the navigation, and
// where a view or folder that a drag moves among them lands
// (./views-menu.tsx).
// A menu lists the views and folders of a folder, or of the top, in the order
// of the navigation, with what each open folder holds below the folder's row,
// one level deeper (../../state/navigation.ts).
//
// A drag lands where the pointer lets go, by the row under it:
// - Over the upper half of a view's row, the view or folder goes before the
//   view, in the view's folder, and over the lower half, after it.
// - Over the upper quarter of a folder's row, it goes before the folder, and
//   over the lower quarter, after it, or first in the folder where the folder
//   is open, since its row then sits above what it holds.
// - Over the middle of a folder's row, it goes last in that folder.
// - Above the first row, it goes before the first, and below the last row, it
//   goes last at the top, which a drop after the last row could not reach
//   where that row sits in a folder.
// A drop that leaves the view or folder where it is lands nowhere, and so
// does a drop of a folder into itself or into a folder that it holds.

import { inFolder, sameEntry, sitsAt, within } from "../../state/index.ts";
import type { DockState, Entry, FolderId } from "../../state/index.ts";

export interface Row {
  readonly entry: Entry;
  // How many of the menu's folders hold it.
  readonly depth: number;
  // The folder that the navigation lists it in, or null at the top.
  readonly folder: FolderId | null;
  // Whether it is a folder that the menu shows open, with what it holds below
  // its row.
  readonly open: boolean;
}

// The rows that a menu lists from a folder, or from the top for null, given
// the folders that are open.
// A folder that sits in itself, which only a patch set with core's command
// can leave, shows closed where the menu lists it again inside itself.
export function rowsOf(state: DockState, root: FolderId | null, open: ReadonlySet<FolderId>): Row[] {
  const rows: Row[] = [];
  const walked = new Set<FolderId>(root === null ? [] : [root]);
  const walk = (folder: FolderId | null, depth: number) => {
    for (const entry of inFolder(state, folder)) {
      const opens = "folder" in entry && open.has(entry.folder) && !walked.has(entry.folder);
      rows.push({ entry, depth, folder, open: opens });
      if (opens) {
        walked.add(entry.folder);
        walk(entry.folder, depth + 1);
      }
    }
  };
  walk(root, 0);
  return rows;
}

// Where a row sits on the screen, from its top to its bottom.
export interface Span {
  readonly top: number;
  readonly bottom: number;
}

// What the menu marks where a drag would land: the line above or below a
// row, which starts at the depth of the place, or the row of the folder that
// takes the view or folder.
export interface Mark {
  readonly row: number;
  readonly at: "above" | "below" | "into";
  readonly depth: number;
}

// Where a view or folder that a drag moves lands: in a folder, or at the top
// for null, before the view or folder that `before` names, or last where it
// names none, as the dock's commands blockr.dock-move_view and
// blockr.dock-move_folder take it.
export interface Drop {
  readonly folder: FolderId | null;
  readonly before: Entry | undefined;
  readonly mark: Mark;
}

// Where `moving` lands as the pointer lets go at the height `y`, given the
// rows of the menu and where each sits, or undefined where it lands nowhere.
export function dropAt(state: DockState, rows: readonly Row[], spans: readonly Span[], y: number, moving: Entry): Drop | undefined {
  const drop = placeAt(state, rows, spans, y);
  if (drop === undefined) return undefined;
  if ("folder" in moving && drop.folder !== null && within(state, drop.folder, moving.folder)) return undefined;
  if (drop.before !== undefined && sameEntry(drop.before, moving)) return undefined;
  const folder = "view" in moving ? state.views[moving.view]?.folder : state.folders[moving.folder]?.folder;
  if (folder === drop.folder && sitsAt(state, moving, drop.folder, drop.before)) return undefined;
  return drop;
}

// The place under the height `y`, whatever moves there.
function placeAt(state: DockState, rows: readonly Row[], spans: readonly Span[], y: number): Drop | undefined {
  const last = rows.length - 1;
  const lastSpan = spans[last];
  if (last < 0 || lastSpan === undefined) return undefined;
  if (y >= lastSpan.bottom) return { folder: null, before: undefined, mark: { row: last, at: "below", depth: 0 } };
  const at = spans.findIndex((span) => y < span.bottom);
  const row = rows[at];
  const span = spans[at];
  if (row === undefined || span === undefined) return undefined;
  const share = (y - span.top) / Math.max(1, span.bottom - span.top);
  const { entry } = row;
  const isFolder = "folder" in entry;
  if (share < (isFolder ? 0.25 : 0.5)) return { folder: row.folder, before: entry, mark: { row: at, at: "above", depth: row.depth } };
  if (!isFolder) return { folder: row.folder, before: after(state, row), mark: { row: at, at: "below", depth: row.depth } };
  if (share <= 0.75) return { folder: entry.folder, before: undefined, mark: { row: at, at: "into", depth: row.depth + 1 } };
  if (row.open) return { folder: entry.folder, before: inFolder(state, entry.folder)[0], mark: { row: at, at: "below", depth: row.depth + 1 } };
  return { folder: row.folder, before: after(state, row), mark: { row: at, at: "below", depth: row.depth } };
}

// The view or folder that follows a row's in its folder, or undefined where
// the row's is the last.
function after(state: DockState, row: Row): Entry | undefined {
  const listed = inFolder(state, row.folder);
  return listed[listed.findIndex((entry) => sameEntry(entry, row.entry)) + 1];
}
