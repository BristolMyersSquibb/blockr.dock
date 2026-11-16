// What the dock holds about its options sidebar (./options-sidebar.tsx):
// whether it is open, which the options button on the bar sets (./navbar.tsx),
// whether it is pinned, and which category's page it shows
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.4#settings).
// The dock holds it for the page, outside the board, so that nothing saves or
// undoes it, and the sidebar is closed as the dock starts on a board.
// Closing the sidebar unpins it and returns it to the list of categories, as
// today's sidebar does, so that it always opens on the list, unpinned.

export interface SidebarState {
  // Whether the sidebar is open.
  readonly open: boolean;
  // Whether the sidebar is pinned, which it is only while it is open.
  readonly pinned: boolean;
  // The category whose page the sidebar shows, or undefined while it shows
  // the list of categories.
  readonly page: string | undefined;
}

export interface Sidebar {
  // The id of the sidebar's element, which the options button names as the
  // element that it controls.
  readonly id: string;
  // The state now, the same object until it changes.
  readonly state: () => SidebarState;
  // Opens or closes the sidebar.
  readonly set: (open: boolean) => void;
  // Pins or unpins the sidebar while it is open.
  readonly pin: (pinned: boolean) => void;
  // Shows the page of the category, or the list of categories for undefined,
  // while the sidebar is open.
  readonly show: (page: string | undefined) => void;
  // Calls `fn` whenever the state changes, until the returned function runs.
  readonly subscribe: (fn: () => void) => () => void;
}

const CLOSED: SidebarState = { open: false, pinned: false, page: undefined };

// How many sidebars the dock has made, which gives each sidebar an id of its
// own.
let made = 0;

export function createSidebar(): Sidebar {
  made += 1;
  let state = CLOSED;
  const listeners = new Set<() => void>();
  const change = (next: SidebarState): void => {
    if (next.open === state.open && next.pinned === state.pinned && next.page === state.page) return;
    state = next;
    for (const fn of [...listeners]) fn();
  };
  return {
    id: `blockr-dock-sidebar-${String(made)}`,
    state: () => state,
    set(open) {
      change(open ? { ...state, open: true } : CLOSED);
    },
    pin(pinned) {
      if (state.open) change({ ...state, pinned });
    },
    show(page) {
      if (state.open) change({ ...state, page });
    },
    subscribe(fn) {
      listeners.add(fn);
      return () => {
        listeners.delete(fn);
      };
    },
  };
}
