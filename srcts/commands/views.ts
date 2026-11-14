// The dock's commands for its views and the folders that group them in the
// navigation (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.1#views-and-folders).
// A view or folder that one adds or moves goes into a folder, or to the top,
// before the view or folder that `before` names, or at the end, with a key in
// `index` between those of its new neighbours.
// So moving one changes only its own `folder` and `index`.
// Removing a folder moves what it holds up into its place, as the `update`
// does where core's command removes one, and removing the last view is
// refused.
// The id of a view or folder that one adds is new among both, minted in the
// shape that the blockr option rand_id names.
// On a locked board, a reader still switches views, while no command adds,
// renames, moves or removes a view or a folder.

import type { Patch } from "@blockr/core";
import { firstView, keyIn, rehoused, sitsAt, within } from "../state/index.ts";
import type { DockState, Entry, FolderId, Grid, ViewId } from "../state/index.ts";
import { at, command, fits, folderOrTop, givenGridFailures, nextName, refuse, stateOf } from "./common.ts";
import * as schemas from "./schemas.ts";

// The grid of a view that shows nothing.
const EMPTY: Grid = { orientation: "horizontal", children: [], sizes: [] };

interface AddView {
  readonly name?: string;
  readonly folder?: FolderId | null;
  readonly before?: Entry;
  readonly grid?: Grid;
}

interface AddFolder {
  readonly name?: string;
  readonly folder?: FolderId | null;
  readonly before?: Entry;
}

interface MoveView {
  readonly view: ViewId;
  readonly folder: FolderId | null;
  readonly before?: Entry;
}

interface MoveFolder {
  readonly folder: FolderId;
  readonly into: FolderId | null;
  readonly before?: Entry;
}

// Whether the board lets the user add, rename, move or remove a view or a
// folder: the board holds the dock's state and is not locked.
function editable(state: DockState | undefined, locked: boolean): state is DockState {
  return state !== undefined && !locked;
}

// Whether a view or a folder has an id, so that a view or folder that a
// command adds gets an id that neither has.
function taken(state: DockState): (id: string) => boolean {
  return (id) => Object.hasOwn(state.views, id) || Object.hasOwn(state.folders, id);
}

export const viewCommands = [
  command<AddView>({
    id: "blockr.dock-add_view",
    label: "Add view",
    description:
      "Adds a view, under a new id, and makes it the active view. It goes into a folder, or to the top, before the view or folder that `before` names, or at the end. Where no name is given, it is named \"View N\", with N one more than the views there are, or the first N after it that no view's name has. Where no grid is given, it shows nothing.",
    args: schemas.object({
      name: schemas.name("view", "The name of the new view."),
      folder: schemas.into("view", true),
      before: schemas.before,
      grid: schemas.ref("Grid", "The grid of the new view, which says which panels it shows and where."),
    }),
    enabled(board, args, { lock }) {
      const state = stateOf(board);
      const folder = args.folder ?? null;
      return editable(state, lock.board) && folderOrTop(state, folder) && fits(state, folder, args.before);
    },
    run(board, args, { mintId }) {
      const state = stateOf(board);
      if (state === undefined) return [];
      const grid = args.grid ?? EMPTY;
      refuse(givenGridFailures(board, grid));
      const folder = args.folder ?? null;
      const id = mintId(taken(state));
      const view = {
        name: args.name ?? nextName("View", Object.values(state.views).map((other) => other.name)),
        folder,
        index: keyIn(state, folder, args.before),
        grid,
      };
      return {
        patches: [
          { op: "add", path: at("views", id), value: view },
          { op: "replace", path: at("active"), value: id },
        ],
      };
    },
  }),

  command<{ readonly view: ViewId }>({
    id: "blockr.dock-remove_view",
    label: "Remove view",
    description:
      "Removes a view, and makes the first view of the navigation active where the view it removes was. It removes no block, and refuses to remove the last view.",
    args: schemas.object({ view: schemas.view }, ["view"]),
    enabled(board, { view }, { lock }) {
      const state = stateOf(board);
      return editable(state, lock.board) && Object.hasOwn(state.views, view) && Object.keys(state.views).length > 1;
    },
    run(board, { view }) {
      const state = stateOf(board);
      if (state === undefined) return [];
      const views = Object.fromEntries(Object.entries(state.views).filter(([id]) => id !== view));
      const patches: Patch[] = [{ op: "remove", path: at("views", view) }];
      if (state.active === view) patches.push({ op: "replace", path: at("active"), value: firstView({ ...state, views }) });
      return { patches };
    },
  }),

  command<{ readonly view: ViewId; readonly name: string }>({
    id: "blockr.dock-rename_view",
    label: "Rename view",
    description: "Gives a view a new name.",
    args: schemas.object({ view: schemas.view, name: schemas.name("view", "The view's new name.") }, ["view", "name"]),
    enabled(board, { view }, { lock }) {
      const state = stateOf(board);
      return editable(state, lock.board) && Object.hasOwn(state.views, view);
    },
    run: (_board, { view, name }) => ({ patches: [{ op: "replace", path: at("views", view, "name"), value: name }] }),
  }),

  command<MoveView>({
    id: "blockr.dock-move_view",
    label: "Move view",
    description:
      "Moves a view into a folder, or to the top, before the view or folder that `before` names, or at the end of that folder.",
    args: schemas.object({ view: schemas.view, folder: schemas.into("view"), before: schemas.before }, ["view", "folder"]),
    enabled(board, { view, folder, before }, { lock }) {
      const state = stateOf(board);
      return (
        editable(state, lock.board) &&
        Object.hasOwn(state.views, view) &&
        folderOrTop(state, folder) &&
        fits(state, folder, before, { view })
      );
    },
    run(board, { view, folder, before }) {
      const state = stateOf(board);
      const moving = state?.views[view];
      if (state === undefined || moving === undefined) return [];
      if (moving.folder === folder && sitsAt(state, { view }, folder, before)) return [];
      return {
        patches: [
          { op: "replace", path: at("views", view, "folder"), value: folder },
          { op: "replace", path: at("views", view, "index"), value: keyIn(state, folder, before, { view }) },
        ],
      };
    },
  }),

  command<{ readonly view: ViewId }>({
    id: "blockr.dock-activate_view",
    label: "Activate view",
    description:
      "Makes a view the active one, which the dock shows. It takes no undo step, since it changes only what the user looks at.",
    args: schemas.object({ view: schemas.view }, ["view"]),
    undo: false,
    enabled(board, { view }) {
      const state = stateOf(board);
      return state !== undefined && Object.hasOwn(state.views, view);
    },
    run: (_board, { view }) => ({ patches: [{ op: "replace", path: at("active"), value: view }] }),
  }),

  command<AddFolder>({
    id: "blockr.dock-add_folder",
    label: "Add folder",
    description:
      "Adds a folder, under a new id, which groups views and other folders in the navigation. It goes into a folder, or to the top, before the view or folder that `before` names, or at the end. Where no name is given, it is named \"Folder N\", with N one more than the folders there are, or the first N after it that no folder's name has.",
    args: schemas.object({
      name: schemas.name("folder", "The name of the new folder."),
      folder: schemas.into("folder", true),
      before: schemas.before,
    }),
    enabled(board, args, { lock }) {
      const state = stateOf(board);
      const folder = args.folder ?? null;
      return editable(state, lock.board) && folderOrTop(state, folder) && fits(state, folder, args.before);
    },
    run(board, args, { mintId }) {
      const state = stateOf(board);
      if (state === undefined) return [];
      const folder = args.folder ?? null;
      const value = {
        name: args.name ?? nextName("Folder", Object.values(state.folders).map((other) => other.name)),
        folder,
        index: keyIn(state, folder, args.before),
      };
      return { patches: [{ op: "add", path: at("folders", mintId(taken(state))), value }] };
    },
  }),

  command<{ readonly folder: FolderId }>({
    id: "blockr.dock-remove_folder",
    label: "Remove folder",
    description:
      "Removes a folder, and moves the views and folders that it holds up into the folder that held it, in its place and in their order. It removes no view.",
    args: schemas.object({ folder: schemas.folder }, ["folder"]),
    enabled(board, { folder }, { lock }) {
      const state = stateOf(board);
      return editable(state, lock.board) && Object.hasOwn(state.folders, folder);
    },
    run(board, { folder }) {
      const state = stateOf(board);
      if (state === undefined) return [];
      const folders = Object.fromEntries(Object.entries(state.folders).filter(([id]) => id !== folder));
      const { moves } = rehoused({ ...state, folders }, state);
      return {
        patches: [
          { op: "remove", path: at("folders", folder) },
          ...moves.flatMap((move): Patch[] => [
            { op: "replace", path: at(move.kind, move.id, "folder"), value: move.folder },
            { op: "replace", path: at(move.kind, move.id, "index"), value: move.index },
          ]),
        ],
      };
    },
  }),

  command<{ readonly folder: FolderId; readonly name: string }>({
    id: "blockr.dock-rename_folder",
    label: "Rename folder",
    description: "Gives a folder a new name.",
    args: schemas.object({ folder: schemas.folder, name: schemas.name("folder", "The folder's new name.") }, ["folder", "name"]),
    enabled(board, { folder }, { lock }) {
      const state = stateOf(board);
      return editable(state, lock.board) && Object.hasOwn(state.folders, folder);
    },
    run: (_board, { folder, name }) => ({
      patches: [{ op: "replace", path: at("folders", folder, "name"), value: name }],
    }),
  }),

  command<MoveFolder>({
    id: "blockr.dock-move_folder",
    label: "Move folder",
    description:
      "Moves a folder, with what it holds, into another folder, or to the top, before the view or folder that `before` names, or at the end of that folder. It refuses to move a folder into itself or into a folder inside it.",
    args: schemas.object({ folder: schemas.folder, into: schemas.into("folder"), before: schemas.before }, ["folder", "into"]),
    enabled(board, { folder, into, before }, { lock }) {
      const state = stateOf(board);
      return (
        editable(state, lock.board) &&
        Object.hasOwn(state.folders, folder) &&
        folderOrTop(state, into) &&
        (into === null || !within(state, into, folder)) &&
        fits(state, into, before, { folder })
      );
    },
    run(board, { folder, into, before }) {
      const state = stateOf(board);
      const moving = state?.folders[folder];
      if (state === undefined || moving === undefined) return [];
      if (moving.folder === into && sitsAt(state, { folder }, into, before)) return [];
      return {
        patches: [
          { op: "replace", path: at("folders", folder, "folder"), value: into },
          { op: "replace", path: at("folders", folder, "index"), value: keyIn(state, into, before, { folder }) },
        ],
      };
    },
  }),
];
