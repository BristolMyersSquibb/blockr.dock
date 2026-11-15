// Where the dock shows each notification of CoreApi, and how it shows the
// message of one
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.7#notifications):
// - A notification about a block shows on the block's card, unless a plugin
//   raised it. So the card shows the conditions that R raised for the block,
//   what core raised about the block, and what the block's own modules
//   raised, such as a renderer that cannot draw an output. It shows those of
//   the severities that the board option show_conditions names.
// - Every other notification shows as a toast (./toasts.tsx): what a plugin
//   raised, even about a block, and every notification about no block.
// So each notification shows in one place.
// - The dock shows a message as text, without the ANSI escapes that a
//   condition's message holds where R runs with colours on, as cli styles it
//   there.

import type { Notification } from "@blockr/core";

// Whether the notification shows on the card of the block that it concerns,
// rather than as a toast.
export function onCard(notification: Notification): boolean {
  const { about, source } = notification;
  if (about === undefined || !("block" in about)) return false;
  return source.kind !== "module" || !("plugin" in source.module.object);
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
