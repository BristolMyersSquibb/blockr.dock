// What the dock's components share through React's context: CoreApi, the id
// of the dock plugin's class, and the app's plugins whose panels the dock
// draws, each fixed for one opening of a board, and the layer and the
// tooltips of the cards (./card).
// Dockview draws the content of each panel and the header of each tab group
// in a portal of the dock's React tree, so they read the context of the dock's
// root as well.
// A component that draws from the board follows it with useBoard().
// The module also holds how the dock runs one of its commands as an edit,
// raises that the board refused one, and gives a block focus.

import type { BlockId, Board, CoreApi, Json, PluginClassId } from "@blockr/core";
import { createContext, useCallback, useContext, useSyncExternalStore } from "react";

export interface DockContextValue {
  readonly core: CoreApi;
  // The id of the dock plugin's class, which raises the dock's notifications.
  readonly plugin: PluginClassId;
  // The app's plugins whose class has a `ui`, each with its name, in the order
  // of the app's plugins.
  readonly drawn: ReadonlyMap<PluginClassId, string>;
  // The element at the end of the dock's root in which the cards' menus and
  // tooltips float, outside dockview's panels, which clip what overflows them.
  // It is null until React has drawn it.
  readonly layer: HTMLElement | null;
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

// Raises that the board refused a command that the dock ran, as a
// notification of the code edit_refused, which concerns the dock plugin, as
// core's plugins raise one for a refused edit.
export function notifyRefused(core: CoreApi, plugin: PluginClassId, error: unknown): void {
  core.notifications.notify(plugin, {
    severity: "error",
    code: "edit_refused",
    message: error instanceof Error ? error.message : String(error),
  });
}

// Runs one of the board's commands as an edit of the dock, and raises that the
// board refused it, where it does.
export function runEdit(core: CoreApi, plugin: PluginClassId, id: string, args: Json): void {
  core.commands.run(id, args).catch((error: unknown) => {
    notifyRefused(core, plugin, error);
  });
}

// Gives a block focus in CoreApi.focus, unless it has it.
export function giveFocus(core: CoreApi, block: BlockId): void {
  if (core.focus.get() !== block) core.focus.set(block);
}
