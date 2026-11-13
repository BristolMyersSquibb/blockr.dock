// The order in which the navigation lists the views and folders
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.1#views-and-folders).
// A folder lists the views and folders that sit in it by their `index`,
// compared as strings, and by their id where two have the same `index`.
// The views of a folder come in the folder's place, so the first view of the
// navigation can sit in a folder.
//
// A view or folder can sit in a folder that the state lacks, and a folder can
// sit in itself, through the folders that hold it.
// Only a patch set with core's command can leave either.
// The navigation lists each such view or folder at the top, so that every
// view and folder has a place in it.

import type { DockState, FolderId, ViewId } from "./types.ts";

// The views and folders that sit in a folder, or at the top for null, in the
// order of the navigation.
export function inFolder(
  state: DockState,
  folder: FolderId | null,
): ({ readonly view: ViewId } | { readonly folder: FolderId })[] {
  const views = Object.entries(state.views)
    .filter(([, view]) => holder(state, view.folder) === folder)
    .map(([id, view]) => ({ entry: { view: id }, id, index: view.index }));
  const folders = Object.entries(state.folders)
    .filter(([id, inner]) => holder(state, inner.folder, id) === folder)
    .map(([id, inner]) => ({ entry: { folder: id }, id, index: inner.index }));
  return [...views, ...folders].sort((a, b) => compare(a.index, b.index) || compare(a.id, b.id)).map(({ entry }) => entry);
}

// Every view, in the order of the navigation, with the views of each folder
// in the folder's place.
export function viewOrder(state: DockState): ViewId[] {
  const order: ViewId[] = [];
  const walked = new Set<FolderId>();
  const walk = (folder: FolderId | null) => {
    for (const entry of inFolder(state, folder)) {
      if ("view" in entry) order.push(entry.view);
      else if (!walked.has(entry.folder)) {
        walked.add(entry.folder);
        walk(entry.folder);
      }
    }
  };
  walk(null);
  return order;
}

// The first view of the navigation, or null where the state holds no view.
export function firstView(state: DockState): ViewId | null {
  return viewOrder(state)[0] ?? null;
}

// Two strings in the order of their code units, as fractional-indexing
// compares its keys.
export function compare(a: string, b: string): number {
  return a < b ? -1 : a > b ? 1 : 0;
}

// The folder that the navigation lists a view or folder in, given the folder
// that it names: that folder, or the top where the state lacks it or where
// `self`, the folder that names it, sits in itself.
function holder(state: DockState, folder: FolderId | null, self?: FolderId): FolderId | null {
  if (folder === null || !Object.hasOwn(state.folders, folder)) return null;
  if (self !== undefined && insideItself(state, self)) return null;
  return folder;
}

// Whether a folder sits in itself, through the folders that hold it.
function insideItself(state: DockState, id: FolderId): boolean {
  const passed = new Set<FolderId>();
  let at = state.folders[id]?.folder ?? null;
  while (at !== null && !passed.has(at)) {
    if (at === id) return true;
    passed.add(at);
    at = state.folders[at]?.folder ?? null;
  }
  return false;
}
