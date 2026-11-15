// What the dock's components share through React's context: CoreApi and the
// app's plugins whose panels the dock draws, each fixed for one opening of a
// board.
// Dockview draws the content of each panel and the header of each tab group
// in a portal of the dock's React tree, so they read the context of the dock's
// root as well.
// A component that draws from the board follows it with useBoard().

import type { Board, CoreApi, PluginClassId } from "@blockr/core";
import { createContext, useCallback, useContext, useSyncExternalStore } from "react";

export interface DockContextValue {
  readonly core: CoreApi;
  // The app's plugins whose class has a `ui`, each with its name, in the order
  // of the app's plugins.
  readonly drawn: ReadonlyMap<PluginClassId, string>;
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
