// The toasts of the dock, followed from notifications and a board that the
// test sets, as CoreApi would hold them, with the browser's timers faked.

import type { Board, BoardObject, CoreApi, Notification, NotificationSource } from "@blockr/core";
import { afterEach, beforeEach, describe, expect, test, vi } from "vitest";
import { DOCK } from "../../commands/index.ts";
import type { DockState, Grid, Panel } from "../../state/index.ts";
import { concerned, createToasts, FADES_FOR, panelShowing, SHOWN_FOR, sourcePackage } from "./toasts.tsx";
import type { Toasts } from "./toasts.tsx";

// Notifications that hold what the test sets, the same array until it sets
// another, as CoreApi's are.
function holding(initial: readonly Notification[] = []) {
  let all = initial;
  const listeners = new Set<(all: readonly Notification[]) => void>();
  return {
    notifications: {
      get: () => all,
      subscribe(fn: (all: readonly Notification[]) => void) {
        listeners.add(fn);
        return () => {
          listeners.delete(fn);
        };
      },
    } satisfies Pick<CoreApi["notifications"], "get" | "subscribe">,
    set(next: readonly Notification[]): void {
      all = next;
      for (const fn of [...listeners]) fn(all);
    },
    listening: () => listeners.size,
  };
}

// A store that holds the board that the test sets, as CoreApi's does, and
// announces each board that the test sets.
function storing(initial: Board) {
  let board = initial;
  const listeners = new Set<(change: { opened: Board; dropped: [] }, board: Board) => void>();
  return {
    store: {
      board: () => board,
      subscribe(fn: (change: { opened: Board; dropped: [] }, board: Board) => void) {
        listeners.add(fn);
        return () => {
          listeners.delete(fn);
        };
      },
    } satisfies Pick<CoreApi["store"], "board" | "subscribe">,
    set(next: Board): void {
      board = next;
      for (const fn of [...listeners]) fn({ opened: next, dropped: [] }, next);
    },
    listening: () => listeners.size,
  };
}

// The id of the DAG's plugin class, whose panel the dock draws.
const DAG_PLUGIN = "blockr.dag-dag_plugin";
const drawn = new Map([[DAG_PLUGIN, "DAG"]]);

// The DAG's plugin, as the source of what it raises.
const DAG: NotificationSource = {
  kind: "module",
  module: { object: { plugin: DAG_PLUGIN }, package: "blockr.dag" },
  code: "link_refused",
};

// A grid of one tab group that shows a's panel, with the panels given behind
// it.
function groupGrid(...panels: Panel[]): Grid {
  return { orientation: "horizontal", children: [{ panels: [{ block: "a" }, ...panels], active: { block: "a" } }], sizes: [1] };
}

// A board whose dock state holds the views given, by id, at the top of the
// navigation in the order given, each with its grid, with the view given
// active.
function boardWith(views: Readonly<Record<string, Grid>>, active: string | null): Board {
  const state: DockState = {
    views: Object.fromEntries(Object.entries(views).map(([id, grid], i) => [id, { name: id, folder: null, index: `a${String(i)}`, grid }])),
    folders: {},
    active,
    visible: {},
  };
  return dockBoard(state);
}

// A board that holds the dock state given.
function dockBoard(state: DockState): Board {
  return { id: "board", name: "Board", blocks: {}, links: {}, stacks: {}, options: {}, plugins: { [DOCK]: state } };
}

// A board whose one view, main, shows a's panel alone.
const plainBoard = boardWith({ main: groupGrid() }, "main");

// A notification of the severity given, which the DAG's plugin raised unless
// another source is given, about the board unless a board object is given.
function raised(
  id: string,
  severity: Notification["severity"],
  about?: BoardObject,
  source: NotificationSource = DAG,
): Notification {
  return { id, severity, message: `${severity} ${id}`, source, ...(about === undefined ? {} : { about }) };
}

// The ids of the notifications whose toasts show, each marked where it fades.
const ids = (toasts: Toasts): string[] => toasts.list().map((toast) => `${toast.notification.id}${toast.fading ? " fading" : ""}`);

const made: Toasts[] = [];
function createTestToasts(
  notifications: Pick<CoreApi["notifications"], "get" | "subscribe">,
  store: Pick<CoreApi["store"], "board" | "subscribe"> = storing(plainBoard).store,
): Toasts {
  const toasts = createToasts({ notifications, store }, drawn);
  made.push(toasts);
  return toasts;
}

beforeEach(() => {
  vi.useFakeTimers();
});

afterEach(() => {
  for (const toasts of made.splice(0)) toasts.stop();
  vi.useRealTimers();
});

describe("the toasts", () => {
  test("show each notification that no card shows, whatever its severity, in the order they came", () => {
    const evaluation: NotificationSource = {
      kind: "r",
      class: ["simpleWarning", "warning", "condition"],
      request: { type: "evaluate", phase: "block_eval" },
    };
    const renderer: NotificationSource = {
      kind: "module",
      module: { object: { block: "b" }, renderer: "blockr.core-basic_table", package: "blockr.core" },
      code: "render_failed",
    };
    const given = holding([
      raised("board", "error"),
      // R's condition, what the block's renderer raised and what the DAG's
      // plugin raised about b show on b's card.
      raised("condition", "warning", { block: "b" }, evaluation),
      raised("renderer", "error", { block: "b" }, renderer),
      raised("plugin", "message", { block: "b" }),
      raised("stack", "warning", { stack: "s" }),
      // The active view holds no panel of the DAG's plugin.
      raised("dag", "error", { plugin: DAG_PLUGIN }),
    ]);
    const toasts = createTestToasts(given.notifications);

    expect(ids(toasts)).toEqual(["board", "stack", "dag"]);
    const shown = toasts.list();
    given.set([...given.notifications.get(), raised("option", "message", { option: "n_rows" })]);
    expect(ids(toasts)).toEqual(["board", "stack", "dag", "option"]);
    expect(toasts.list()[0]).toBe(shown[0]);
  });

  test("call their subscribers as the list changes, and keep the same list until a toast comes, goes or starts to fade", () => {
    const evaluation: NotificationSource = {
      kind: "r",
      class: ["simpleError", "error", "condition"],
      request: { type: "evaluate", phase: "expr" },
    };
    const given = holding([raised("a", "error")]);
    const toasts = createTestToasts(given.notifications);
    const heard = vi.fn();
    const off = toasts.subscribe(heard);
    const before = toasts.list();

    given.set([...given.notifications.get(), raised("b", "error", { block: "b" }, evaluation)]);
    expect(toasts.list()).toBe(before);
    expect(heard).not.toHaveBeenCalled();

    given.set([raised("a", "error"), raised("c", "message")]);
    expect(toasts.list()).not.toBe(before);
    expect(heard).toHaveBeenCalledTimes(1);
    vi.advanceTimersByTime(SHOWN_FOR);
    expect(heard).toHaveBeenCalledTimes(2);

    off();
    given.set([]);
    expect(heard).toHaveBeenCalledTimes(2);
  });

  test("fade for a message after five seconds and then go, while a warning's and an error's stay until the user closes them", () => {
    const given = holding([raised("m", "message"), raised("w", "warning"), raised("e", "error")]);
    const toasts = createTestToasts(given.notifications);

    vi.advanceTimersByTime(SHOWN_FOR - 1);
    expect(ids(toasts)).toEqual(["m", "w", "e"]);
    vi.advanceTimersByTime(1);
    expect(ids(toasts)).toEqual(["m fading", "w", "e"]);
    vi.advanceTimersByTime(FADES_FOR);
    expect(ids(toasts)).toEqual(["w", "e"]);

    vi.advanceTimersByTime(60_000);
    expect(ids(toasts)).toEqual(["w", "e"]);
    toasts.close("e");
    expect(ids(toasts)).toEqual(["w"]);
  });

  test("keep a message's toast while the pointer rests on it or the focus is in it, and start its five seconds again once neither does", () => {
    const given = holding([raised("m", "message")]);
    const toasts = createTestToasts(given.notifications);

    vi.advanceTimersByTime(SHOWN_FOR - 1000);
    toasts.keep("m", "pointer");
    toasts.keep("m", "focus");
    vi.advanceTimersByTime(60_000);
    expect(ids(toasts)).toEqual(["m"]);
    toasts.release("m", "pointer");
    vi.advanceTimersByTime(60_000);
    expect(ids(toasts)).toEqual(["m"]);

    toasts.release("m", "focus");
    vi.advanceTimersByTime(SHOWN_FOR - 1);
    expect(ids(toasts)).toEqual(["m"]);
    vi.advanceTimersByTime(1);
    expect(ids(toasts)).toEqual(["m fading"]);

    // The pointer that comes to rest on a fading toast keeps it.
    toasts.keep("m", "pointer");
    expect(ids(toasts)).toEqual(["m"]);
    vi.advanceTimersByTime(60_000);
    expect(ids(toasts)).toEqual(["m"]);
    toasts.release("m", "pointer");
    vi.advanceTimersByTime(SHOWN_FOR + FADES_FOR);
    expect(ids(toasts)).toEqual([]);
  });

  test("go as soon as core withdraws their notifications, and do not come back while a notification holds once they faded or the user closed them", () => {
    const given = holding([raised("m", "message"), raised("w", "warning"), raised("e", "error")]);
    const toasts = createTestToasts(given.notifications);

    toasts.close("w");
    vi.advanceTimersByTime(SHOWN_FOR + FADES_FOR);
    expect(ids(toasts)).toEqual(["e"]);
    // Core holds the same notifications in a new array.
    given.set([...given.notifications.get()]);
    expect(ids(toasts)).toEqual(["e"]);

    given.set([raised("w", "warning")]);
    expect(ids(toasts)).toEqual([]);
    expect(vi.getTimerCount()).toBe(0);
    // A notification that core raises again once it withdrew it, with the
    // same id, as one of core's own, shows again.
    given.set([raised("m", "message"), raised("e", "error")]);
    expect(ids(toasts)).toEqual(["m", "e"]);
  });

  test("keep their state where core holds a notification anew under the same id, and show what it holds now", () => {
    const given = holding([raised("m", "message")]);
    const toasts = createTestToasts(given.notifications);
    vi.advanceTimersByTime(SHOWN_FOR);
    expect(ids(toasts)).toEqual(["m fading"]);

    // R replied to the same request again, with the same condition.
    const again = { ...raised("m", "message"), message: "message m, again" };
    given.set([again]);
    expect(ids(toasts)).toEqual(["m fading"]);
    expect(toasts.list()[0]?.notification).toBe(again);
    vi.advanceTimersByTime(FADES_FOR);
    expect(ids(toasts)).toEqual([]);
  });

  test("stop following the notifications and the board, and stop their timers, once stopped", () => {
    const given = holding([raised("m", "message")]);
    const board = storing(plainBoard);
    const toasts = createTestToasts(given.notifications, board.store);
    toasts.keep("m", "pointer");

    toasts.stop();
    expect(given.listening()).toBe(0);
    expect(board.listening()).toBe(0);
    toasts.release("m", "pointer");
    vi.advanceTimersByTime(60_000);
    expect(ids(toasts)).toEqual(["m"]);
    expect(vi.getTimerCount()).toBe(0);
  });
});

describe("the toast of a notification about a plugin whose panel the dock draws", () => {
  const dag = { plugin: DAG_PLUGIN };

  test("shows while the active view holds no panel of the plugin, goes once it holds one, and comes back once it holds none again", () => {
    const given = holding([raised("e", "error", dag), raised("m", "message", dag)]);
    const board = storing(boardWith({ main: groupGrid(), side: groupGrid(dag) }, "main"));
    const toasts = createTestToasts(given.notifications, board.store);
    expect(ids(toasts)).toEqual(["e", "m"]);

    vi.advanceTimersByTime(SHOWN_FOR - 1000);
    board.set(boardWith({ main: groupGrid(), side: groupGrid(dag) }, "side"));
    expect(ids(toasts)).toEqual([]);
    expect(vi.getTimerCount()).toBe(0);

    // The message's toast counts its five seconds anew.
    board.set(boardWith({ main: groupGrid(), side: groupGrid(dag) }, "main"));
    expect(ids(toasts)).toEqual(["e", "m"]);
    vi.advanceTimersByTime(SHOWN_FOR - 1);
    expect(ids(toasts)).toEqual(["e", "m"]);
    vi.advanceTimersByTime(1 + FADES_FOR);
    expect(ids(toasts)).toEqual(["e"]);
  });

  test("goes as the active view comes to hold the panel, which a command adds", () => {
    const given = holding([raised("e", "error", dag)]);
    const board = storing(boardWith({ main: groupGrid() }, "main"));
    const toasts = createTestToasts(given.notifications, board.store);
    expect(ids(toasts)).toEqual(["e"]);

    board.set(boardWith({ main: groupGrid(dag) }, "main"));
    expect(ids(toasts)).toEqual([]);
  });

  test("does not come back once the user closed it or it faded, while its notification holds", () => {
    const given = holding([raised("e", "error", dag), raised("m", "message", dag)]);
    const board = storing(boardWith({ main: groupGrid(), side: groupGrid(dag) }, "main"));
    const toasts = createTestToasts(given.notifications, board.store);

    toasts.close("e");
    vi.advanceTimersByTime(SHOWN_FOR + FADES_FOR);
    expect(ids(toasts)).toEqual([]);
    board.set(boardWith({ main: groupGrid(), side: groupGrid(dag) }, "side"));
    board.set(boardWith({ main: groupGrid(), side: groupGrid(dag) }, "main"));
    expect(ids(toasts)).toEqual([]);
  });

  test("shows whatever the active view holds where the plugin's class has no `ui`, so that the dock draws no panel for it", () => {
    const storage = { plugin: "blockr.core-file_storage_plugin" };
    const given = holding([raised("e", "error", storage)]);
    const board = storing(boardWith({ main: groupGrid(storage) }, "main"));
    const toasts = createTestToasts(given.notifications, board.store);
    expect(ids(toasts)).toEqual(["e"]);
  });
});

describe("the link of a toast that shows a plugin's panel", () => {
  const dag = { plugin: DAG_PLUGIN };
  const show = { id: "blockr.dock-show_panel", args: { panel: dag } };
  const passes = { preflight: () => true };
  const fails = { preflight: () => false };

  test("makes the first view of the navigation that holds the panel the active one, and shows the panel there", () => {
    // The navigation lists main, then the folder f, which holds the view
    // inner, then the view last, and both inner and last hold the panel.
    const board = dockBoard({
      views: {
        last: { name: "Last", folder: null, index: "a2", grid: groupGrid(dag) },
        inner: { name: "Inner", folder: "f", index: "a0", grid: groupGrid(dag) },
        main: { name: "Main", folder: null, index: "a0", grid: groupGrid() },
      },
      folders: { f: { name: "F", folder: null, index: "a1" } },
      active: "main",
      visible: {},
    });
    expect(panelShowing(board, DAG_PLUGIN, fails)).toEqual([{ id: "blockr.dock-activate_view", args: { view: "inner" } }, show]);
  });

  test("only shows the panel where the active view is the first that holds it", () => {
    const board = boardWith({ main: groupGrid(dag), side: groupGrid(dag) }, "main");
    expect(panelShowing(board, DAG_PLUGIN, fails)).toEqual([show]);
  });

  test("shows the panel in the active view where no view holds it, where blockr.dock-show_panel passes its preflight, and is none otherwise", () => {
    const board = boardWith({ main: groupGrid(), side: groupGrid() }, "side");
    expect(panelShowing(board, DAG_PLUGIN, passes)).toEqual([show]);
    // As on a locked board, which adds no panel.
    expect(panelShowing(board, DAG_PLUGIN, fails)).toBeUndefined();
  });

  test("is none on a board that holds no state of the dock's", () => {
    expect(panelShowing({ ...plainBoard, plugins: {} }, DAG_PLUGIN, passes)).toBeUndefined();
  });
});

describe("what a toast names", () => {
  const board: Board = {
    id: "board",
    name: "Iris review",
    blocks: {
      a: { registry_id: "blockr.core-dataset_block", state: {}, name: "Iris", locked: null },
      b: { registry_id: "blockr.core-head_block", state: {}, name: "Head", locked: null },
    },
    links: { ab: { from: "a", to: "b", input: "data" } },
    stacks: { s: { name: "Inputs", blocks: ["a"], color: null } },
    options: {},
    plugins: {},
  };
  const core = {
    options: {
      registered: () => ({ n_rows: { label: "Rows shown" } }) as unknown as ReturnType<CoreApi["options"]["registered"]>,
    },
    plugins: { appPlugins: () => ({ "blockr.dag-dag_plugin": { name: "DAG" } }) },
  };

  test("is the board object that the notification concerns, by the name the page shows for it, by its id where core no longer knows it, or the board", () => {
    expect(
      [
        undefined,
        { option: "n_rows" },
        { option: "zoom" },
        { plugin: "blockr.dag-dag_plugin" },
        { plugin: "blockr.gone-gone_plugin" },
        { stack: "s" },
        { stack: "gone" },
        { link: "ab" },
        { link: "gone" },
        { block: "b" },
        { block: "gone" },
      ].map((about) => concerned(about, core, board)),
    ).toEqual([
      "Board",
      'Board option "Rows shown"',
      'Board option "zoom"',
      'Plugin "DAG"',
      'Plugin "blockr.gone-gone_plugin"',
      'Stack "Inputs"',
      'Stack "gone"',
      'Link from "Iris" to "Head"',
      'Link "gone"',
      'Block "Head"',
      'Block "gone"',
    ]);
  });

  test("beside it, is the package that the notification's source names: the package of the module that made the request, failed or raised it", () => {
    const failed: Notification = {
      id: "failed",
      severity: "error",
      message: "No module.",
      source: { kind: "core", code: "module_failed", module: { object: { plugin: "blockr.dag-dag_plugin" }, package: "blockr.dag" } },
      about: { plugin: "blockr.dag-dag_plugin" },
    };
    const replied: Notification = {
      id: "replied",
      severity: "error",
      message: "No layout.",
      source: {
        kind: "r",
        class: ["simpleError", "error", "condition"],
        request: { type: "plugin", name: "layout" },
        module: { object: { plugin: "blockr.dag-dag_plugin" }, package: "blockr.dag" },
      },
      about: { plugin: "blockr.dag-dag_plugin" },
    };
    // What the store dropped as the board opened, which no module is behind.
    const dropped: Notification = {
      id: "dropped",
      severity: "warning",
      message: "The board opened without: package blockr.io 0.1.0",
      source: { kind: "core", code: "board_objects_dropped" },
    };

    expect([raised("own", "error"), failed, replied, dropped].map(sourcePackage)).toEqual([
      "blockr.dag",
      "blockr.dag",
      "blockr.dag",
      undefined,
    ]);
  });
});
