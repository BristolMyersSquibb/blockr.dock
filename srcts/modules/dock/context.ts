// What the dock's components share through React's context: CoreApi, the id
// of the dock plugin's class, and the app's plugins whose panels the dock
// draws, each fixed for one opening of a board, and the tooltips of the cards
// (./card).
// Dockview draws the content of each panel and the header of each tab group
// in a portal of the dock's React tree, so they read the context of the dock's
// root as well.
// A component that draws from the board follows it with useBoard(), and one
// that draws a block's status or the notifications about it follows them with
// useStatus() and useNotified(), as core's own cards do.
// The module also holds how the dock runs one of its commands as an edit, and
// how it gives a block focus.

import type { BlockId, BlockStatus, Board, CoreApi, Json, Notification, PluginClassId } from "@blockr/core";
import { createContext, useCallback, useContext, useMemo, useSyncExternalStore } from "react";

export interface DockContextValue {
  readonly core: CoreApi;
  // The id of the dock plugin's class, which raises the error of a failed copy
  // of a block's id (./card/header.tsx).
  readonly plugin: PluginClassId;
  // The app's plugins whose class has a `ui`, each with its name, in the order
  // of the app's plugins.
  readonly drawn: ReadonlyMap<PluginClassId, string>;
  readonly tooltips: Tooltips;
}

// The tooltips of the dock, of which one shows at a time, each by the
// function that hides it (./card/tooltip.tsx).
export interface Tooltips {
  // A tooltip shows, which hides the one on screen.
  showing(hide: () => void): void;
  // A tooltip left the screen.
  left(): void;
  // Whether a tooltip left the screen less than 400ms ago, so that the next
  // shows at once.
  warm(): boolean;
}

export const DockContext = createContext<DockContextValue | null>(null);

export function useDock(): DockContextValue {
  const value = useContext(DockContext);
  if (value === null) throw new Error("A component of the dock is drawn outside the dock.");
  return value;
}

// The board, which the component draws again after each change.
export function useBoard(core: CoreApi): Board {
  const subscribe = useCallback(
    (changed: () => void) =>
      core.store.subscribe(() => {
        changed();
      }),
    [core],
  );
  const board = useCallback(() => core.store.board(), [core]);
  return useSyncExternalStore(subscribe, board);
}

// The block's status, which the component draws again as the scheduler
// announces that it changed, since the scheduler keeps the same object until
// then.
export function useStatus(core: CoreApi, block: BlockId): BlockStatus | undefined {
  const subscribe = useCallback(
    (changed: () => void) =>
      core.scheduler.subscribe((blocks) => {
        if (blocks.includes(block)) changed();
      }),
    [core, block],
  );
  const status = useCallback(() => core.scheduler.status(block), [core, block]);
  return useSyncExternalStore(subscribe, status);
}

// The notifications about the block, in the order that they came, which the
// component draws again as one comes or goes, since CoreApi keeps the same
// array of them until then.
export function useNotified(core: CoreApi, block: BlockId): readonly Notification[] {
  const subscribe = useCallback(
    (changed: () => void) =>
      core.notifications.subscribe(() => {
        changed();
      }),
    [core],
  );
  const notifications = useCallback(() => core.notifications.get(), [core]);
  const all = useSyncExternalStore(subscribe, notifications);
  return useMemo(
    () => all.filter(({ about }) => about !== undefined && "block" in about && about.block === block),
    [all, block],
  );
}

// Runs one of the board's commands as an edit of the dock.
// Where the board refuses it, core tells the user why, as an error of the code
// board_edit_failed.
export function runEdit(core: CoreApi, id: string, args: Json): void {
  core.commands.run(id, args).catch(() => {
    // Core tells the user why the board refused the command.
  });
}

// Gives a block focus in CoreApi.focus, unless it has it.
export function giveFocus(core: CoreApi, block: BlockId): void {
  if (core.focus.get() !== block) core.focus.set(block);
}
