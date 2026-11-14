// The `update` of the dock plugin's class
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.1#the-update).
// The store calls it with every change that it applies, undos included, and
// adds the patches that it returns to the same step.
// It reads only the delta and the board, never the screen, so every peer that
// applies a change computes the same patches.
//
// It starts from the dock's state in the board before the delta, and applies
// the delta's patches to it.
// Then it takes these steps:
// 1. It takes each removed block's panel out of every grid, as prunePanels()
//    does, and drops the block's entry in `visible`.
// 2. It moves each view and folder that sits in a folder the state lacks up,
//    into the folder that held the removed one in the board before the delta.
//    They go where the removed folder stood, with new keys in `index`.
// 3. Where `active` names no view, it makes the first view of the navigation
//    active.
// 4. It places each added block's panel in the active view, at the default
//    place, unless a view places it already.
//
// It returns a patch for each value that these steps change:
// - The grid of a view, replaced whole, as the dock's commands replace one.
// - A block's entry in `visible`.
// - The `folder` and `index` of a view or folder.
// - The active view, in `active`.
// The store then calls the `update` again, on the delta with these patches,
// and it finds nothing more to change.
// Where the delta's patches leave a state of another shape, it returns no
// patch.
// The store's check of the state against the schema then says what is wrong.

import type { Board, Delta, Patch, PluginClassId } from "@blockr/core";
import { gridPanels, placePanel, places, prunePanels } from "./grid.ts";
import { compare, firstView, keysBetween } from "./navigation.ts";
import { patched, pointer } from "./patch.ts";
import { isDockState } from "./state.ts";
import type { DockState, Folder, FolderId, Panel, View } from "./types.ts";

export function update(delta: Delta, board: Board, plugin: PluginClassId): Patch[] {
  const stored = board.plugins[plugin];
  const given = stored === undefined ? undefined : patched(stored, delta.patches ?? [], plugin);
  const removed = new Set(delta.blocks?.rm ?? []);
  const added = Object.keys(delta.blocks?.add ?? {});
  // A delta that neither adds nor removes a block nor patches the dock's
  // state leaves the state as it was, with nothing to repair.
  if (removed.size === 0 && added.length === 0 && given === stored) return [];
  if (!isDockState(given)) return [];
  const before = isDockState(stored) ? stored : given;

  const cleared = removeBlocks(given, removed);
  const { state: moved, moves } = rehoused(cleared, before);
  const active =
    moved.active !== null && Object.hasOwn(moved.views, moved.active) ? moved.active : firstView(moved);
  const state = placeBlocks({ ...moved, active }, added);

  const at = (...segments: string[]) => pointer(["plugins", plugin, ...segments]);
  const patches: Patch[] = [];
  for (const [id, view] of Object.entries(state.views)) {
    if (view.grid !== given.views[id]?.grid) {
      patches.push({ op: "replace", path: at("views", id, "grid"), value: view.grid });
    }
  }
  for (const block of removed) {
    if (Object.hasOwn(given.visible, block)) patches.push({ op: "remove", path: at("visible", block) });
  }
  for (const move of moves) {
    patches.push({ op: "replace", path: at(move.kind, move.id, "folder"), value: move.folder });
    patches.push({ op: "replace", path: at(move.kind, move.id, "index"), value: move.index });
  }
  if (state.active !== given.active) patches.push({ op: "replace", path: at("active"), value: state.active });
  return patches;
}

// The state without the panels of the blocks given, in any grid, and without
// their entries in `visible`.
// A view whose grid places none of them keeps its grid as it is.
function removeBlocks(state: DockState, removed: ReadonlySet<string>): DockState {
  if (removed.size === 0) return state;
  const kept = (panel: Panel) => !("block" in panel && removed.has(panel.block));
  const views = Object.fromEntries(
    Object.entries(state.views).map(([id, view]) => [
      id,
      gridPanels(view.grid).every(kept) ? view : { ...view, grid: prunePanels(view.grid, kept) },
    ]),
  );
  const visible = Object.fromEntries(Object.entries(state.visible).filter(([block]) => !removed.has(block)));
  return { ...state, views, visible };
}

interface Move {
  readonly kind: "views" | "folders";
  readonly id: string;
  readonly folder: FolderId | null;
  readonly index: string;
}

// Step 2 above: moves each view and folder that sits in a folder the state
// lacks, and gives the state after the moves, with the moves.
// The views and folders that a removed folder held keep their order.
// Where the board before the delta lacks the folder too, they go to the end
// of the top.
// The command blockr.dock-remove_folder moves what the folder held in the
// same way.
export function rehoused(state: DockState, before: DockState): { state: DockState; moves: Move[] } {
  const lacks = (folder: FolderId | null): folder is FolderId => folder !== null && !Object.hasOwn(state.folders, folder);
  const entries = (kind: Move["kind"], record: Readonly<Record<string, View | Folder>>) =>
    Object.entries(record).map(([id, entry]) => ({ kind, id, folder: entry.folder, index: entry.index }));
  const all = [...entries("views", state.views), ...entries("folders", state.folders)];
  const stray = all.flatMap((entry) => (lacks(entry.folder) ? [{ ...entry, folder: entry.folder }] : []));
  if (stray.length === 0) return { state, moves: [] };

  // Where each view or folder goes.
  // It walks up the folders of the board before the delta, from the removed
  // folder that holds it, until it reaches a folder that the state holds, or
  // the top.
  // It goes there, in the place of the last removed folder on the way.
  // Its path, the keys from that removed folder down to it, orders the views
  // and folders that go to one place.
  const placed = stray.map((entry) => {
    const path = [entry.index];
    let outer = entry.folder;
    const passed = new Set([outer]);
    let parent = before.folders[outer]?.folder ?? null;
    while (lacks(parent) && Object.hasOwn(before.folders, parent) && !passed.has(parent)) {
      path.unshift(before.folders[outer]?.index ?? "");
      passed.add(parent);
      outer = parent;
      parent = before.folders[outer]?.folder ?? null;
    }
    const place = before.folders[outer]?.index ?? null;
    return { ...entry, target: lacks(parent) ? null : parent, place, path };
  });

  // The keys of what each receiving folder holds, in order.
  // Each place adds the keys that it gives out to its folder's list.
  const lists = new Map<FolderId | null, { index: string; added: boolean }[]>();
  const listOf = (target: FolderId | null) => {
    let list = lists.get(target);
    if (list === undefined) {
      list = all
        .filter((entry) => entry.folder === target)
        .sort((a, b) => compare(a.index, b.index) || compare(a.id, b.id))
        .map((entry) => ({ index: entry.index, added: false }));
      lists.set(target, list);
    }
    return list;
  };

  const groups = new Map<string, typeof placed>();
  for (const entry of placed) {
    const key = JSON.stringify([entry.target, entry.place]);
    groups.set(key, [...(groups.get(key) ?? []), entry]);
  }

  // The places in the order of their keys, so that a place goes after what
  // the places before it added.
  // The place of a folder that the board before lacks comes last: the end of
  // the top.
  const order = [...groups.values()].sort((a, b) => placeOrder(a[0]?.place ?? null, b[0]?.place ?? null));

  const moves: Move[] = [];
  for (const group of order) {
    const [first] = group;
    if (first === undefined) continue;
    group.sort((a, b) => pathOrder(a.path, b.path) || compare(a.kind, b.kind) || compare(a.id, b.id));
    const list = listOf(first.target);
    const place = first.place;
    const after = place === null ? list.length : list.filter((held) => held.added || compare(held.index, place) <= 0).length;
    const keys = keysBetween(list[after - 1]?.index ?? null, list[after]?.index ?? null, group.length);
    group.forEach((entry, i) => {
      const index = keys[i] ?? entry.index;
      moves.push({ kind: entry.kind, id: entry.id, folder: first.target, index });
      list.splice(after + i, 0, { index, added: true });
    });
  }

  let next = state;
  for (const { kind, id, folder, index } of moves) {
    if (kind === "views") {
      const view = next.views[id];
      if (view !== undefined) next = { ...next, views: { ...next.views, [id]: { ...view, folder, index } } };
    } else {
      const inner = next.folders[id];
      if (inner !== undefined) next = { ...next, folders: { ...next.folders, [id]: { ...inner, folder, index } } };
    }
  }
  return { state: next, moves };
}

// Places in a folder by their keys, with the end, null, last.
function placeOrder(a: string | null, b: string | null): number {
  if (a === b) return 0;
  if (a === null) return 1;
  if (b === null) return -1;
  return compare(a, b);
}

// Paths of keys in the order of the navigation: by their first key, then by
// the next, with a folder before what it holds.
function pathOrder(a: readonly string[], b: readonly string[]): number {
  for (let i = 0; i < Math.min(a.length, b.length); i++) {
    const order = compare(a[i] ?? "", b[i] ?? "");
    if (order !== 0) return order;
  }
  return a.length - b.length;
}

// The state with the panel of each block given placed in the active view, at
// the default place, unless a view places it already.
function placeBlocks(state: DockState, added: readonly string[]): DockState {
  const active = state.active;
  if (active === null) return state;
  let next = state;
  for (const block of added) {
    const panel = { block };
    const view = next.views[active];
    if (view === undefined) return next;
    if (Object.values(next.views).some((other) => places(other.grid, panel))) continue;
    next = { ...next, views: { ...next.views, [active]: { ...view, grid: placePanel(view.grid, panel) } } };
  }
  return next;
}
