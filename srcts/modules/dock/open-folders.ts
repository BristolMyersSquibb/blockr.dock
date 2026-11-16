// Which folders are open in the views menu and in the menus of the line of
// tabs (./views-menu.tsx), which a click on a folder's row sets
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.1#views-and-folders).
// Only this screen needs them, so the dock holds them for the page, outside
// the board, and nothing saves or undoes them.
// No folder is open as the dock starts on a board.

import type { FolderId } from "../../state/index.ts";

export interface OpenFolders {
  // The folders that are open now, as the same set until one opens or closes.
  readonly get: () => ReadonlySet<FolderId>;
  // Opens or closes a folder.
  readonly set: (folder: FolderId, open: boolean) => void;
  // Opens the folders given, and leaves every other as it is.
  readonly open: (folders: readonly FolderId[]) => void;
  // Calls `fn` whenever a folder opens or closes, until the returned function
  // runs.
  readonly subscribe: (fn: () => void) => () => void;
}

export function createOpenFolders(): OpenFolders {
  let open: ReadonlySet<FolderId> = new Set();
  const listeners = new Set<() => void>();
  const change = (next: ReadonlySet<FolderId>) => {
    open = next;
    for (const fn of [...listeners]) fn();
  };
  return {
    get: () => open,
    set(folder, on) {
      if (open.has(folder) === on) return;
      change(on ? new Set([...open, folder]) : new Set([...open].filter((other) => other !== folder)));
    },
    open(folders) {
      const closed = folders.filter((folder) => !open.has(folder));
      if (closed.length > 0) change(new Set([...open, ...closed]));
    },
    subscribe(fn) {
      listeners.add(fn);
      return () => {
        listeners.delete(fn);
      };
    },
  };
}
