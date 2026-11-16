// What the dock holds about its options sidebar (./options-sidebar.tsx):
// whether it is open, which the options button on the bar sets (./navbar.tsx),
// whether it is pinned, and which categories of board options are expanded
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.4#settings).
// The dock holds it for the page, outside the board, so that nothing saves or
// undoes it.
// As the dock starts on a board, the sidebar is closed and every category is
// collapsed.
// Closing the sidebar unpins it, as today's sidebar does. The categories that
// the user expanded stay expanded, until another board opens or the page
// reloads.

export interface SidebarState {
  // Whether the sidebar is open.
  readonly open: boolean;
  // Whether the sidebar is pinned, which it is only while it is open.
  readonly pinned: boolean;
  // The categories that are expanded, in the order in which they expanded.
  readonly expanded: readonly string[];
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
  // Expands or collapses the category.
  readonly expand: (category: string, expanded: boolean) => void;
  // Calls `fn` whenever the state changes, until the returned function runs.
  readonly subscribe: (fn: () => void) => () => void;
}

const CLOSED: SidebarState = { open: false, pinned: false, expanded: [] };

// How many sidebars the dock has made, which gives each sidebar an id of its
// own.
let made = 0;

export function createSidebar(): Sidebar {
  made += 1;
  let state = CLOSED;
  const listeners = new Set<() => void>();
  const change = (next: SidebarState): void => {
    if (next.open === state.open && next.pinned === state.pinned && next.expanded === state.expanded) return;
    state = next;
    for (const fn of [...listeners]) fn();
  };
  return {
    id: `blockr-dock-sidebar-${String(made)}`,
    state: () => state,
    set(open) {
      change(open ? { ...state, open: true } : { ...state, open: false, pinned: false });
    },
    pin(pinned) {
      if (state.open) change({ ...state, pinned });
    },
    expand(category, expanded) {
      if (state.expanded.includes(category) === expanded) return;
      const next = expanded ? [...state.expanded, category] : state.expanded.filter((c) => c !== category);
      change({ ...state, expanded: next });
    },
    subscribe(fn) {
      listeners.add(fn);
      return () => {
        listeners.delete(fn);
      };
    },
  };
}
