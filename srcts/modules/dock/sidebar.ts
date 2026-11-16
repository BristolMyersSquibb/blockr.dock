// Whether the dock's options sidebar is open, which the options button on the
// bar sets (./navbar.tsx), and from which the sidebar draws
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.5#the-docks-own-parts).
// The dock holds it for the page, outside the board, so that nothing saves or
// undoes it, and the sidebar is closed as the dock starts on a board.

export interface Sidebar {
  // Whether the sidebar is open now.
  readonly isOpen: () => boolean;
  // Opens or closes the sidebar.
  readonly set: (open: boolean) => void;
  // Calls `fn` whenever the sidebar opens or closes, until the returned
  // function runs.
  readonly subscribe: (fn: () => void) => () => void;
}

export function createSidebar(): Sidebar {
  let open = false;
  const listeners = new Set<() => void>();
  return {
    isOpen: () => open,
    set(next) {
      if (next === open) return;
      open = next;
      for (const fn of [...listeners]) fn();
    },
    subscribe(fn) {
      listeners.add(fn);
      return () => {
        listeners.delete(fn);
      };
    },
  };
}
