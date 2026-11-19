// Where the dock shows each notification of CoreApi, and how it shows the
// message of one
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.7#notifications,
// https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.4#places-besides-the-panel):
// - A notification about a block shows on the block's card, whoever raised
//   it: R, core, one of the block's modules or a plugin. The card shows those
//   of the severities that the board option show_conditions names.
// - A notification about a plugin whose panel the dock draws, as for a plugin
//   whose class has a `ui`, shows in the plugin's panel, which the plugin
//   draws, while the active view holds the panel. The panel's tab then shows a
//   dot while a warning or an error about the plugin holds, whose messages its
//   tooltip holds (./dock.tsx).
// - Where the active view holds no panel of the plugin, the notification
//   shows as a toast, with a link that shows the panel (./toasts.tsx).
//   Once the active view holds the panel, the toast goes, and it comes back
//   where the active view changes to one that holds no panel of the plugin.
// - Every other notification shows as a toast: those about the board, a link,
//   a stack, a board option, or a plugin without a `ui`, such as the storage
//   plugin, which has no panel.
// So each notification shows in one place.
// - The dock shows a message as text, without the ANSI escapes that a
//   condition's message holds where R runs with colours on, as cli styles it
//   there.

import type { Board, Json, Notification, PluginClassId, Severity } from "@blockr/core";
import { activeView } from "../../commands/index.ts";
import { places } from "../../state/index.ts";

// Whether the notification shows on the card of the block that it concerns,
// rather than as a toast.
export function onCard({ about }: Notification): boolean {
  return about !== undefined && "block" in about;
}

// Whether the value of the board option show_conditions names the severity,
// so that a card or a tab shows the notifications of that severity.
export function showsSeverity(showConditions: Json | undefined, severity: Severity): boolean {
  return Array.isArray(showConditions) && showConditions.includes(severity);
}

// The plugin that the notification concerns, where the dock draws a panel for
// the plugin, given the plugins whose panels the dock draws.
export function panelPlugin(
  { about }: Notification,
  drawn: ReadonlyMap<PluginClassId, string>,
): PluginClassId | undefined {
  return about !== undefined && "plugin" in about && drawn.has(about.plugin) ? about.plugin : undefined;
}

// Whether the active view holds the panel of the plugin, in a tab group or on
// a rail, collapsed or open.
export function inActiveView(board: Board, plugin: PluginClassId): boolean {
  const view = activeView(board);
  return view !== undefined && places(view.grid, { plugin });
}

// Whether the notification shows as a toast: it shows on no card, and in no
// panel that the active view holds.
export function asToast(notification: Notification, board: Board, drawn: ReadonlyMap<PluginClassId, string>): boolean {
  if (onCard(notification)) return false;
  const plugin = panelPlugin(notification, drawn);
  return plugin === undefined || !inActiveView(board, plugin);
}

// The severities of the notifications that a panel's tab tells of with its
// dot, the worst first.
const TAB_SEVERITIES: readonly Severity[] = ["error", "warning"];

// The notifications that the tab of a plugin's panel tells of with its dot,
// of all that hold: the errors about the plugin, then its warnings, each in
// the order they came, whatever the board option show_conditions names.
export function tabNotifications(all: readonly Notification[], plugin: PluginClassId): Notification[] {
  const concerning = all.filter(({ about }) => about !== undefined && "plugin" in about && about.plugin === plugin);
  return TAB_SEVERITIES.flatMap((severity) => concerning.filter((notification) => notification.severity === severity));
}

// The ANSI escapes that a message may hold:
// - Control sequences, which open with ESC and "[" or with the one-byte CSI,
//   as cli's colours and styles do.
// - Operating system commands, which open with ESC and "]" and end with BEL or
//   with ESC and "\", as cli's hyperlinks do.
// eslint-disable-next-line no-control-regex -- ANSI escapes open with the control characters ESC and CSI.
const ANSI = /(?:\u001b\[|\u009b)[0-?]*[ -/]*[@-~]|\u001b\][^\u0007\u001b]*(?:\u0007|\u001b\\)/g;

// A notification's message as the dock shows it: the text alone, without
// ANSI escapes.
export function plainText(message: string): string {
  return message.replace(ANSI, "");
}
