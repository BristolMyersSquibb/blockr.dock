// The notifications about a block that its card shows, placed by their
// severity (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.7#notifications):
// - The errors show above the card's sections, whatever the sections show, in
//   the message style of blockr.ui's design system, in its danger set
//   (./card.css).
// - The warnings and the messages fold under the output behind their count by
//   severity, as "1 warning, 2 messages", which opens and closes them, as
//   today's card folds them, and they start closed.
//   The warnings come first, in the amber set of the message style, and the
//   messages after them.
// Each shows its message as text, in the order that the notifications of its
// severity came.
// The card shows every notification that ../notifications.ts gives the card,
// whatever its severity.

import type { Notification } from "@blockr/core";
import type { ReactElement } from "react";
import { Chevron } from "../icons.tsx";
import { plainText } from "../notifications.ts";

// The errors, or nothing while there are none.
export function Errors({ notifications }: { notifications: readonly Notification[] }): ReactElement | null {
  if (notifications.length === 0) return null;
  return <Listed notifications={notifications} label="Errors" />;
}

// The warnings and the messages, folded behind their count, or nothing while
// there are none.
export function WarningsAndMessages({ notifications }: { notifications: readonly Notification[] }): ReactElement | null {
  const warnings = notifications.filter((notification) => notification.severity === "warning");
  const messages = notifications.filter((notification) => notification.severity === "message");
  if (warnings.length + messages.length === 0) return null;
  const count = [counted(warnings.length, "warning"), counted(messages.length, "message")].filter((part) => part !== null);
  return (
    <details className="blockr-card-messages">
      <summary className="blockr-card-messages-count">
        {count.join(", ")}
        <Chevron />
      </summary>
      <Listed notifications={[...warnings, ...messages]} />
    </details>
  );
}

// How many there are of a severity, as "2 messages", or nothing for none.
function counted(n: number, severity: string): string | null {
  if (n === 0) return null;
  return `${String(n)} ${severity}${n === 1 ? "" : "s"}`;
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
