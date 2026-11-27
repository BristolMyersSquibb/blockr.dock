// The toasts of the dock, which show every notification of CoreApi that no
// card and no panel of the active view shows, as ./notifications.ts sorts
// them, the way core's basic frontend shows its toasts
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
// - The toast of a notification about a plugin whose panel the dock draws
//   shows while the active view holds no panel of the plugin
//   (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.4#places-besides-the-panel).
//   Its link "Show panel" makes the first view of the navigation that holds
//   the panel the active one and shows the panel there. Where no view holds
//   the panel, the link shows it in the active view, which adds it at the
//   default place, as the dock's command show_panel does.
//   The toast goes once the active view holds the panel, since the panel's
//   tab then shows a dot (./dock.tsx). It comes back, with its five seconds
//   anew for a message, where the active view changes to one that holds no
//   panel of the plugin, unless it faded or the user closed it before.
//   On a locked board, where no view holds the panel, the toast has no link,
//   since a locked dock adds no panel.
// - A message's toast is a status, and a warning's or an error's is an alert,
//   which a screen reader reads out at once.
// - The stylesheet draws an error's toast and a warning's in the message style
//   of blockr.ui's design system (./index.css). The design system gives a
//   message no colour, since blue means "on" there, so a message's toast is
//   neutral.

import type { Board, BoardObject, CoreApi, Notification, PluginClassId } from "@blockr/core";
import { useBoard } from "@blockr/core/react";
import { useSyncExternalStore } from "react";
import type { ReactElement } from "react";
import { dockState } from "../../commands/index.ts";
import { idText, places, viewOrder } from "../../state/index.ts";
import type { Panel } from "../../state/index.ts";
import { runEdit } from "./context.ts";
import type { Batch } from "./context.ts";
import { asToast, panelPlugin } from "./notifications.ts";

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

// Follows the notifications of CoreApi, and the board, on which the active
// view decides whether a notification about a plugin shows as a toast, given
// the plugins whose panels the dock draws.
export function createToasts(
  core: {
    readonly notifications: Pick<CoreApi["notifications"], "get" | "subscribe">;
    readonly store: Pick<CoreApi["store"], "board" | "subscribe">;
  },
  drawn: ReadonlyMap<PluginClassId, string>,
): Toasts {
  const { notifications, store } = core;
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

  // Follows the notifications that hold, on the board as it is: drops the
  // toasts of those that core withdrew and of those that a panel of the
  // active view now shows, and shows each new one that no card and no panel
  // of the active view shows.
  function update(): void {
    const board = store.board();
    const toasted = new Set(heard.filter((notification) => asToast(notification, board, drawn)).map((notification) => notification.id));
    for (const [id, toast] of shown) {
      if (toasted.has(id)) continue;
      clearTimeout(toast.timer);
      shown.delete(id);
    }
    const holding = new Set(heard.map((notification) => notification.id));
    for (const id of gone) {
      if (!holding.has(id)) gone.delete(id);
    }
    for (const notification of heard) {
      if (!toasted.has(notification.id) || gone.has(notification.id)) continue;
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

  const offNotifications = notifications.subscribe((all) => {
    heard = all;
    update();
  });
  const offBoard = store.subscribe(() => {
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
      offNotifications();
      offBoard();
      for (const toast of shown.values()) clearTimeout(toast.timer);
    },
  };
}

// What the first line of a toast names: the board object that its
// notification concerns, by the name that the page shows for it, or the board
// for none. A board object that core no longer knows goes by its id, a
// plugin's by the id of its class, as core's messages name an id.
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
  if ("plugin" in about) return `Plugin "${core.plugins.appPlugins().get(about.plugin)?.name ?? idText(about.plugin)}"`;
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

// The commands that show the plugin's panel, which the link of a toast runs as
// one step: they make the first view of the navigation that holds the panel
// the active one, where it is not, and show the panel there, or, where no view
// holds the panel, show it in the active view, which adds it at the default
// place.
// They are the dock's commands activate_view and show_panel, neither of which
// takes an undo step, so the step takes none.
// Where no view holds the panel and show_panel fails its preflight, as on a
// locked board, the dock cannot show the panel, so the toast has no link.
export function panelShowing(
  board: Board,
  plugin: PluginClassId,
  core: { readonly id: CoreApi["id"]; readonly commands: Pick<CoreApi["commands"], "preflight"> },
): Batch | undefined {
  const state = dockState(board);
  if (state === undefined) return undefined;
  // The args hold the plugin's Id as the record that it serializes as.
  const panel: Panel = { plugin };
  const showPanel = core.id("blockr.dock", "show_panel");
  const show = { id: showPanel, args: { panel } };
  const view = viewOrder(state).find((id) => {
    const held = state.views[id];
    return held !== undefined && places(held.grid, panel);
  });
  if (view === undefined) return core.commands.preflight(showPanel, { panel }) ? [show] : undefined;
  return view === state.active ? [show] : [{ id: core.id("blockr.dock", "activate_view"), args: { view } }, show];
}

export interface ToastListProps {
  readonly core: CoreApi;
  readonly toasts: Toasts;
  // The app's plugins whose class has a `ui`, each with its name, whose panels
  // the dock draws.
  readonly drawn: ReadonlyMap<PluginClassId, string>;
}

// The toasts that show, one above the other in the window's bottom right
// corner, the latest lowest, or nothing while none shows.
// The toasts follow the board, so that a block renamed while its toast shows
// goes by its new name there.
export function ToastList({ core, toasts, drawn }: ToastListProps): ReactElement | null {
  const list = useSyncExternalStore(toasts.subscribe, toasts.list);
  const board = useBoard(core);
  if (list.length === 0) return null;
  return (
    <section className="blockr-dock-toasts" aria-label="Notifications">
      {list.map((toast) => {
        const plugin = panelPlugin(toast.notification, drawn);
        const showing = plugin === undefined ? undefined : panelShowing(board, plugin, core);
        return (
          <ToastItem
            key={toast.notification.id}
            toast={toast}
            title={concerned(toast.notification.about, core, board)}
            toasts={toasts}
            show={
              showing === undefined
                ? undefined
                : () => {
                    runEdit(core, showing);
                  }
            }
          />
        );
      })}
    </section>
  );
}

interface ToastItemProps {
  readonly toast: Toast;
  readonly title: string;
  readonly toasts: Toasts;
  // Shows the panel of the plugin that the notification concerns, where the
  // toast has a link that shows it.
  readonly show: (() => void) | undefined;
}

// A toast: what its notification concerns, with the package that its source
// names, its message, the link that shows the panel of the plugin that it
// concerns, where it has one, and a button that closes it.
function ToastItem({ toast, title, toasts, show }: ToastItemProps): ReactElement {
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
        <p className="blockr-dock-toast-message">{notification.message}</p>
        {show === undefined ? null : (
          <button type="button" className="blockr-dock-toast-link" onClick={show}>
            Show panel
          </button>
        )}
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
