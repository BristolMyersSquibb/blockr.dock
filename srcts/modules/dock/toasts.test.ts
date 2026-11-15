// The toasts of the dock, followed from notifications that the test sets, as
// CoreApi's would hold them, with the browser's timers faked.

import type { Board, BoardObject, CoreApi, Notification, NotificationSource } from "@blockr/core";
import { afterEach, beforeEach, describe, expect, test, vi } from "vitest";
import { concerned, createToasts, FADES_FOR, SHOWN_FOR, sourcePackage } from "./toasts.tsx";
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

// The DAG's plugin, as the source of what it raises.
const DAG: NotificationSource = {
  kind: "module",
  module: { object: { plugin: "blockr.dag-dag_plugin" }, package: "blockr.dag" },
  code: "link_refused",
};

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
function toastsOf(notifications: Pick<CoreApi["notifications"], "get" | "subscribe">): Toasts {
  const toasts = createToasts(notifications);
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
      // R's condition and what the block's renderer raised show on b's card.
      raised("condition", "warning", { block: "b" }, evaluation),
      raised("renderer", "error", { block: "b" }, renderer),
      // What the DAG's plugin raised about b shows as a toast.
      raised("plugin", "message", { block: "b" }),
      raised("stack", "warning", { stack: "s" }),
    ]);
    const toasts = toastsOf(given.notifications);

    expect(ids(toasts)).toEqual(["board", "plugin", "stack"]);
    const shown = toasts.list();
    given.set([...given.notifications.get(), raised("option", "message", { option: "n_rows" })]);
    expect(ids(toasts)).toEqual(["board", "plugin", "stack", "option"]);
    expect(toasts.list()[0]).toBe(shown[0]);
  });

  test("call their subscribers as the list changes, and keep the same list until a toast comes, goes or starts to fade", () => {
    const evaluation: NotificationSource = {
      kind: "r",
      class: ["simpleError", "error", "condition"],
      request: { type: "evaluate", phase: "expr" },
    };
    const given = holding([raised("a", "error")]);
    const toasts = toastsOf(given.notifications);
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
    const toasts = toastsOf(given.notifications);

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
    const toasts = toastsOf(given.notifications);

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
    const toasts = toastsOf(given.notifications);

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
    const toasts = toastsOf(given.notifications);
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

  test("stop following the notifications, and stop their timers, once stopped", () => {
    const given = holding([raised("m", "message")]);
    const toasts = toastsOf(given.notifications);
    toasts.keep("m", "pointer");

    toasts.stop();
    expect(given.listening()).toBe(0);
    toasts.release("m", "pointer");
    vi.advanceTimersByTime(60_000);
    expect(ids(toasts)).toEqual(["m"]);
    expect(vi.getTimerCount()).toBe(0);
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
