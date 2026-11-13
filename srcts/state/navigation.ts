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
//
// A view or folder that the dock adds or moves goes between two neighbours in
// its folder, with a key between theirs, from fractional-indexing's
// generateKeyBetween().
// So it changes only its own `folder` and `index`.

import { generateKeyBetween, generateNKeysBetween } from "fractional-indexing";
import type { DockState, FolderId, ViewId } from "./types.ts";

// A view or a folder, as the navigation lists it.
export type Entry = { readonly view: ViewId } | { readonly folder: FolderId };

// The views and folders that sit in a folder, or at the top for null, in the
// order of the navigation.
export function inFolder(state: DockState, folder: FolderId | null): Entry[] {
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

// Whether two entries name the same view or the same folder.
export function sameEntry(a: Entry, b: Entry): boolean {
  return "view" in a ? "view" in b && a.view === b.view : "folder" in b && a.folder === b.folder;
}

// The key that places a view or folder in a folder, or at the top for null,
// before the view or folder that `before` names, or at the end where it names
// none.
// It goes among the views and folders that the folder lists, but for
// `moving`, the view or folder that the key is for.
// The caller checks that the folder lists `before`.
export function keyIn(state: DockState, folder: FolderId | null, before: Entry | undefined, moving?: Entry): string {
  const others = inFolder(state, folder).filter((entry) => moving === undefined || !sameEntry(entry, moving));
  const at = before === undefined ? others.length : others.findIndex((entry) => sameEntry(entry, before));
  const index = (entry: Entry | undefined) => (entry === undefined ? null : indexOf(state, entry));
  const [key] = keysBetween(index(others[at - 1]), index(others[at]), 1);
  return key ?? generateKeyBetween(null, null);
}

// Whether the folder lists a view or folder right before the view or folder
// that `before` names, or last where it names none, where moving it there
// changes nothing.
export function sitsAt(state: DockState, entry: Entry, folder: FolderId | null, before: Entry | undefined): boolean {
  const listed = inFolder(state, folder);
  const at = listed.findIndex((other) => sameEntry(other, entry));
  if (at === -1) return false;
  const next = listed[at + 1];
  return before === undefined ? next === undefined : next !== undefined && sameEntry(next, before);
}

// Whether a folder is `outer` or sits in it, through the folders that hold
// it.
export function within(state: DockState, folder: FolderId, outer: FolderId): boolean {
  const passed = new Set<FolderId>();
  let at: FolderId | null = folder;
  while (at !== null && !passed.has(at)) {
    if (at === outer) return true;
    passed.add(at);
    at = state.folders[at]?.folder ?? null;
  }
  return false;
}

// Keys in order between two keys, `n` of them, or with fewer bounds where a
// bound is no key of fractional-indexing's, as a patch can leave one, or
// where the bounds leave no key between them, as two equal keys do.
export function keysBetween(a: string | null, b: string | null, n: number): string[] {
  const bounds: [string | null, string | null][] = [
    [a, b],
    [a, null],
    [null, b],
  ];
  for (const [low, high] of bounds) {
    try {
      return generateNKeysBetween(low, high, n);
    } catch {
      // A bound that is no key, or two equal bounds, which the next pair
      // leaves out.
    }
  }
  return generateNKeysBetween(null, null, n);
}

// The `index` of a view or folder, or null where the state lacks it.
function indexOf(state: DockState, entry: Entry): string | null {
  return ("view" in entry ? state.views[entry.view]?.index : state.folders[entry.folder]?.index) ?? null;
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
  const parent = state.folders[id]?.folder ?? null;
  return parent !== null && within(state, parent, id);
}
