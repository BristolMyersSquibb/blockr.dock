// A text field that edits a name in place, as the board's name on the bar
// (./navbar.tsx) and the name of a view or a folder in the views menu
// (./views-menu.tsx) have it.
// It takes the focus as it shows, with its text selected.
// Enter or leaving the field submits what it holds, unless it is empty or
// unchanged, and Escape gives up, as core's basic frontend has it.
// Escape gives up the edit alone, so that a menu that holds the field stays
// open.

import { useLayoutEffect, useRef, useState } from "react";
import type { ReactElement } from "react";

export interface NameFieldProps {
  readonly name: string;
  // What the field edits, for a screen reader, such as "Board name".
  readonly label: string;
  readonly className: string;
  readonly onSubmit: (name: string) => void;
  // Called once the edit ends, whether or not the field submitted.
  readonly onDone: () => void;
}

export function NameField({ name, label, className, onSubmit, onDone }: NameFieldProps): ReactElement {
  const [text, setText] = useState(name);
  const ref = useRef<HTMLInputElement>(null);
  const done = useRef(false);

  useLayoutEffect(() => {
    ref.current?.focus();
    ref.current?.select();
  }, []);

  function finish(submit: boolean): void {
    if (done.current) return;
    done.current = true;
    const trimmed = text.trim();
    if (submit && trimmed !== "" && trimmed !== name) onSubmit(trimmed);
    onDone();
  }

  return (
    <input
      ref={ref}
      type="text"
      aria-label={label}
      className={className}
      value={text}
      spellCheck={false}
      onChange={(event) => {
        setText(event.target.value);
      }}
      onKeyDown={(event) => {
        if (event.key === "Enter" && !event.nativeEvent.isComposing) finish(true);
        else if (event.key === "Escape") {
          event.preventDefault();
          event.stopPropagation();
          finish(false);
        }
      }}
      onBlur={() => {
        finish(true);
      }}
    />
  );
}
