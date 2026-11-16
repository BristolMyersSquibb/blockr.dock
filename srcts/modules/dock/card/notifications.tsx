// The notifications about a block that its card shows, placed by their
// severity (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.7#notifications):
// - The errors and the warnings show above the card's sections, whatever the
//   sections show, in the message style of blockr.ui's design system, the
//   danger set for an error and the amber set for a warning (./card.css).
// - The messages fold under the output behind their count, as "2 messages",
//   which opens and closes them, and they start closed.
// Each shows its message as text, in the order that the notifications came.
// The card shows those that ../notifications.ts gives the card, of the
// severities that the board option show_conditions names.

import type { Notification } from "@blockr/core";
import type { ReactElement } from "react";
import { Chevron } from "../icons.tsx";
import { plainText } from "../notifications.ts";

// The errors and the warnings, or nothing while there are none.
export function ErrorsAndWarnings({ notifications }: { notifications: readonly Notification[] }): ReactElement | null {
  if (notifications.length === 0) return null;
  return <Listed notifications={notifications} label="Errors and warnings" />;
}

// The messages, folded behind their count, or nothing while there are none.
export function Messages({ notifications }: { notifications: readonly Notification[] }): ReactElement | null {
  const n = notifications.length;
  if (n === 0) return null;
  return (
    <details className="blockr-card-messages">
      <summary className="blockr-card-messages-count">
        {n === 1 ? "1 message" : `${String(n)} messages`}
        <Chevron />
      </summary>
      <Listed notifications={notifications} />
    </details>
  );
}

function Listed({ notifications, label }: { notifications: readonly Notification[]; label?: string }): ReactElement {
  return (
    <ul className="blockr-card-notifications" aria-label={label}>
      {notifications.map(({ id, severity, message }) => (
        <li key={id} className="blockr-card-notification" data-severity={severity}>
          {plainText(message)}
        </li>
      ))}
    </ul>
  );
}
