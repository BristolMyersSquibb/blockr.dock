// The toasts of the dock, which show every notification of CoreApi that no
// card shows, as ./notifications.ts sorts them, the way core's basic frontend
// shows its toasts
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2#proposal,
// https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.14#cores-basic-frontend).
// - The toasts sit in the window's bottom right corner, one above the other,
//   in the order that their notifications came, so the latest is lowest.
// - The first line of a toast names what its notification concerns, with the
//   package that the notification's source names. Below it, the toast shows
//   the message as text.
// - A message's toast fades after five seconds, but not while the pointer
//   rests on it or the keyboard's focus is in it. Its five seconds start
//   again once neither is. A warning's or an error's toast stays until the
//   user closes it.
// - A toast goes as soon as core withdraws its notification. A toast that
//   faded or that the user closed does not come back while its notification
//   holds. The dock keeps the ids of those notifications for the opening, and
//   forgets each id once core withdraws its notification.
// - A message's toast is a status, and a warning's or an error's is an alert,
//   which a screen reader reads out at once.
// - The stylesheet draws an error's toast and a warning's in the message style
//   of blockr.ui's design system (./index.css). The design system gives a
//   message no colour, since blue means "on" there, so a message's toast is
//   neutral.

import type { Board, BoardObject, CoreApi, Notification } from "@blockr/core";
import { useCallback, useSyncExternalStore } from "react";
import type { ReactElement } from "react";
import { onCard, plainText } from "./notifications.ts";

// How long a message's toast shows before it fades, and how long it takes to
// fade, in milliseconds.
export const SHOWN_FOR = 5000;
export const FADES_FOR = 300;

export interface Toast {
  readonly notification: Notification;
  // Whether it is fading, once a message's five seconds are up.
  readonly fading: boolean;
}

// What keeps a message's toast from fading.
type Keeper = "pointer" | "focus";

// The toasts of one opening, which a component follows as React's
// useSyncExternalStore follows a store, through `list` and `subscribe`.
export interface Toasts {
  // The toasts that show, the same array until one comes or goes, or starts
  // or stops fading.
  readonly list: () => readonly Toast[];
  // Calls `fn` whenever the list changes.
  readonly subscribe: (fn: () => void) => () => void;
  // Hides the toast of the notification, as the user closes it.
  readonly close: (id: string) => void;
  // Keeps the toast of a message from fading, as the pointer rests on it or
  // the keyboard's focus moves into it.
  readonly keep: (id: string, by: Keeper) => void;
  // Lets the toast fade again, as the pointer or the focus leaves it. Once
  // neither keeps it, its five seconds start again.
  readonly release: (id: string, by: Keeper) => void;
  // Stops following the notifications, and stops every toast's timer.
  readonly stop: () => void;
}

// A toast that shows, with the timer of a message's toast, which counts down
// its five seconds or its fade, and what keeps it from fading.
interface Shown {
  toast: Toast;
  timer: ReturnType<typeof setTimeout> | undefined;
  readonly kept: Set<Keeper>;
}

export function createToasts(notifications: Pick<CoreApi["notifications"], "get" | "subscribe">): Toasts {
  // The toasts that show, by the ids of their notifications.
  const shown = new Map<string, Shown>();
  // The ids of the notifications whose toasts faded or that the user closed,
  // while the notifications hold.
  const gone = new Set<string>();
  const listeners = new Set<() => void>();
  let heard = notifications.get();
  let list: readonly Toast[] = [];
  let stopped = false;

  // Lists the toasts that show, in the order of their notifications, and
  // tells the subscribers where the list changed.
  function publish(): void {
    const next = heard.flatMap((notification) => {
      const toast = shown.get(notification.id)?.toast;
      return toast === undefined ? [] : [toast];
    });
    if (next.length === list.length && next.every((toast, i) => toast === list[i])) return;
    list = next;
    for (const fn of [...listeners]) fn();
  }

  // Starts the five seconds of a message's toast, unless something keeps it.
  function countDown(id: string, toast: Shown): void {
    if (stopped || toast.toast.notification.severity !== "message" || toast.kept.size > 0) return;
    clearTimeout(toast.timer);
    toast.timer = setTimeout(() => {
      toast.toast = { ...toast.toast, fading: true };
      toast.timer = setTimeout(() => {
        hide(id);
      }, FADES_FOR);
      publish();
    }, SHOWN_FOR);
  }

  // Hides the toast, which faded or which the user closed, while its
  // notification holds.
  function hide(id: string): void {
    const toast = shown.get(id);
    if (toast === undefined) return;
    clearTimeout(toast.timer);
    shown.delete(id);
    gone.add(id);
    publish();
  }

  // Follows the notifications that hold: drops the toasts of those that core
  // withdrew, and shows each new one that no card shows.
  function update(): void {
    const holding = new Set(heard.map((notification) => notification.id));
    for (const [id, toast] of shown) {
      if (holding.has(id)) continue;
      clearTimeout(toast.timer);
      shown.delete(id);
    }
    for (const id of gone) {
      if (!holding.has(id)) gone.delete(id);
    }
    for (const notification of heard) {
      if (onCard(notification) || gone.has(notification.id)) continue;
      const toast = shown.get(notification.id);
      if (toast === undefined) {
        const added: Shown = { toast: { notification, fading: false }, timer: undefined, kept: new Set() };
        shown.set(notification.id, added);
        countDown(notification.id, added);
      } else if (toast.toast.notification !== notification) {
        toast.toast = { ...toast.toast, notification };
      }
    }
    publish();
  }

  const off = notifications.subscribe((all) => {
    heard = all;
    update();
  });
  update();

  return {
    list: () => list,
    subscribe(fn) {
      const wrapped = (): void => {
        fn();
      };
      listeners.add(wrapped);
      return () => {
        listeners.delete(wrapped);
      };
    },
    close: hide,
    keep(id, by) {
      const toast = shown.get(id);
      if (toast === undefined) return;
      toast.kept.add(by);
      clearTimeout(toast.timer);
      toast.timer = undefined;
      if (!toast.toast.fading) return;
      toast.toast = { ...toast.toast, fading: false };
      publish();
    },
    release(id, by) {
      const toast = shown.get(id);
      if (toast?.kept.delete(by) !== true) return;
      countDown(id, toast);
    },
    stop() {
      stopped = true;
      off();
      for (const toast of shown.values()) clearTimeout(toast.timer);
    },
  };
}

// What the first line of a toast names: the board object that its
// notification concerns, by the name that the page shows for it, or the board
// for none. A board object that core no longer knows goes by its id.
export function concerned(
  about: BoardObject | undefined,
  core: {
    readonly options: Pick<CoreApi["options"], "registered">;
    readonly plugins: Pick<CoreApi["plugins"], "appPlugins">;
  },
  board: Board,
): string {
  if (about === undefined) return "Board";
  if ("option" in about) return `Board option "${own(core.options.registered(), about.option)?.label ?? about.option}"`;
  if ("plugin" in about) return `Plugin "${own(core.plugins.appPlugins(), about.plugin)?.name ?? about.plugin}"`;
  if ("stack" in about) return `Stack "${own(board.stacks, about.stack)?.name ?? about.stack}"`;
  const block = (id: string): string => `"${own(board.blocks, id)?.name ?? id}"`;
  if ("block" in about) return `Block ${block(about.block)}`;
  const link = own(board.links, about.link);
  return link === undefined ? `Link "${about.link}"` : `Link from ${block(link.from)} to ${block(link.to)}`;
}

// The package that the notification's source names, if any: the package of
// the module behind it, which made the request that R replied to, whose code
// failed, or which raised the notification.
export function sourcePackage(notification: Notification): string | undefined {
  return notification.source.module?.package;
}

// The entry of the record under the key, where the record holds one of its
// own.
function own<T>(record: Readonly<Record<string, T>>, key: string): T | undefined {
  return Object.hasOwn(record, key) ? record[key] : undefined;
}

export interface ToastListProps {
  readonly core: CoreApi;
  readonly toasts: Toasts;
}

// The toasts that show, one above the other in the window's bottom right
// corner, the latest lowest, or nothing while none shows.
// The toasts follow the board, so that a block renamed while its toast shows
// goes by its new name there.
export function ToastList({ core, toasts }: ToastListProps): ReactElement | null {
  const list = useSyncExternalStore(toasts.subscribe, toasts.list);
  const board = useBoard(core);
  if (list.length === 0) return null;
  return (
    <section className="blockr-dock-toasts" aria-label="Notifications">
      {list.map((toast) => (
        <ToastItem
          key={toast.notification.id}
          toast={toast}
          title={concerned(toast.notification.about, core, board)}
          toasts={toasts}
        />
      ))}
    </section>
  );
}

interface ToastItemProps {
  readonly toast: Toast;
  readonly title: string;
  readonly toasts: Toasts;
}

// A toast: what its notification concerns, with the package that its source
// names, its message, and a button that closes it.
function ToastItem({ toast, title, toasts }: ToastItemProps): ReactElement {
  const { notification, fading } = toast;
  const { id, severity } = notification;
  const pkg = sourcePackage(notification);
  return (
    <div
      className="blockr-dock-toast"
      role={severity === "message" ? "status" : "alert"}
      data-severity={severity}
      data-fading={fading ? "" : undefined}
      onPointerEnter={() => {
        toasts.keep(id, "pointer");
      }}
      onPointerLeave={() => {
        toasts.release(id, "pointer");
      }}
      onFocus={() => {
        toasts.keep(id, "focus");
      }}
      onBlur={(event) => {
        if (!(event.relatedTarget instanceof Node && event.currentTarget.contains(event.relatedTarget))) {
          toasts.release(id, "focus");
        }
      }}
    >
      <div className="blockr-dock-toast-text">
        <p className="blockr-dock-toast-title">
          {title}
          {pkg === undefined ? null : <span className="blockr-dock-toast-package"> · {pkg}</span>}
        </p>
        <p className="blockr-dock-toast-message">{plainText(notification.message)}</p>
      </div>
      <button
        type="button"
        className="blockr-dock-toast-close"
        aria-label="Close"
        onClick={() => {
          toasts.close(id);
        }}
      >
        <span aria-hidden="true">×</span>
      </button>
    </div>
  );
}

// The board, which the store keeps the same object until it announces a
// change.
function useBoard(core: CoreApi): Board {
  const subscribe = useCallback(
    (fn: () => void) =>
      core.store.subscribe(() => {
        fn();
      }),
    [core],
  );
  return useSyncExternalStore(subscribe, () => core.store.board());
}
