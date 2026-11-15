// What every card of a dock shares, which the dock gives its cards and its
// tabs through React's context: core's CoreApi, the dock plugin's class, the
// element in which cards draw their menus and tooltips, and the tooltip on
// screen.
// Dockview draws each panel's content and tab through a React portal inside
// its own component, so the context reaches them.

import type { BlockId, Board, CoreApi, PluginClassId } from "@blockr/core";
import { createContext, useCallback, useContext, useSyncExternalStore } from "react";

// The tooltips of a dock, of which one shows at a time, each by the function
// that hides it (./tooltip.tsx).
export interface Tooltips {
  // A tooltip shows, which hides the one on screen.
  showing(hide: () => void): void;
  // A tooltip left the screen.
  left(): void;
  // Whether a tooltip left the screen less than 400ms ago, so that the next
  // shows at once.
  warm(): boolean;
}

export interface Scope {
  readonly core: CoreApi;
  // The id of the dock plugin's class, which raises the dock's notifications.
  readonly plugin: PluginClassId;
  // The element at the end of the dock's root that holds the menus and the
  // tooltips, outside dockview's panels, which clip what overflows them.
  // It is null until React has drawn it.
  readonly layer: HTMLElement | null;
  readonly tooltips: Tooltips;
}

export const CardScope = createContext<Scope | null>(null);

export function useScope(): Scope {
  const scope = useContext(CardScope);
  if (scope === null) throw new Error("A card is drawn outside the dock.");
  return scope;
}

// What a component reads of the board, which it reads again after every change
// of the board.
// The read gives a part of the board or a value of its own, so that it gives
// the same as long as the board is the same.
export function useBoard<T>(core: CoreApi, read: (board: Board) => T): T {
  const subscribe = useCallback((changed: () => void) => core.store.subscribe(changed), [core]);
  return useSyncExternalStore(subscribe, () => read(core.store.board()));
}

// Gives a block focus, unless it has it.
export function giveFocus(core: CoreApi, block: BlockId): void {
  if (core.focus.get() !== block) core.focus.set(block);
}
