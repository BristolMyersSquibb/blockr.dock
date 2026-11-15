// The block's menu, which the "…" button opens, drawn as the design system's
// menus of actions are
// (https://github.com/BristolMyersSquibb/blockr.ui/blob/e14e8310481e98b156871bf96669b41cb88c8435/vignettes/articles/design-system.Rmd#L1062-L1165).
// It floats in the dock's layer below the button, and above it where there is
// no room below.
// The focus stays on the menu while it is open, and the row that the
// keyboard is on is the menu's active descendant:
// - The arrows move the row, Home and End jump to the first and the last,
//   and Enter or Space picks it, as a click does.
// - Opened from the keyboard, the menu starts on its first row, and the
//   pointer moves the same row.
// - Escape closes the menu and gives the focus back to the button.
// - A click elsewhere closes it, and so does Tab, which moves the focus on
//   from the button.
// - A pick closes it, gives the focus back to the button, without a ring
//   after a click, and then runs the row.

import { Fragment, useEffect, useId, useLayoutEffect, useState } from "react";
import type { KeyboardEvent, ReactElement, ReactNode, RefObject } from "react";
import { createPortal } from "react-dom";
import { useDock } from "../context.ts";
import { useFloating } from "./floating.ts";
import { menuSpot } from "./place.ts";

export interface MenuRow {
  readonly key: string;
  readonly label: string;
  // Text after the label in a monospaced face, as the block's id.
  readonly meta?: string;
  readonly icon?: ReactNode;
  // Whether the row destroys something, which it shows under the pointer.
  readonly danger?: boolean;
  // Whether a divider comes before the row.
  readonly divided?: boolean;
  readonly onSelect: () => void;
}

// How a menu closes: whether the focus goes back to the button, and whether a
// click closed it, after which the focus shows no ring.
export interface Closing {
  readonly restore: boolean;
  readonly pointer: boolean;
}

// Closes a menu as a click or the focus lands outside it and its button.
// A menu that has gone closes nothing, as where the "+" menu takes the focus
// as it replaces the block's menu, before React has removed the block's
// menu's listeners.
export function useDismiss(menu: RefObject<HTMLElement | null>, anchor: HTMLElement, close: (how: Closing) => void): void {
  useEffect(() => {
    const doc = anchor.ownerDocument;
    const outside = (target: EventTarget | null) =>
      menu.current !== null && !(target instanceof Node && (anchor.contains(target) || menu.current.contains(target)));
    const onPointerDown = (event: PointerEvent) => {
      if (outside(event.target)) close({ restore: false, pointer: true });
    };
    const onFocusIn = (event: FocusEvent) => {
      if (outside(event.target)) close({ restore: false, pointer: false });
    };
    doc.addEventListener("pointerdown", onPointerDown, true);
    doc.addEventListener("focusin", onFocusIn);
    return () => {
      doc.removeEventListener("pointerdown", onPointerDown, true);
      doc.removeEventListener("focusin", onFocusIn);
    };
  }, [menu, anchor, close]);
}

// The index of the row after `at`, or before it for a step of -1, from the
// last row round to the first, or the first or the last where no row is
// active.
export function stepped(at: number | null, step: 1 | -1, count: number): number {
  if (at === null) return step === 1 ? 0 : count - 1;
  return (at + step + count) % count;
}

export function ActionMenu({
  anchor,
  label,
  rows,
  keyboard,
  onClose,
}: {
  anchor: HTMLElement;
  label: string;
  rows: readonly MenuRow[];
  // Whether the keyboard opened the menu, which starts it on its first row.
  keyboard: boolean;
  onClose: (how: Closing) => void;
}): ReactElement | null {
  const { layer } = useDock();
  const ref = useFloating<HTMLDivElement>(anchor, menuSpot);
  const id = useId();
  const [active, setActive] = useState<number | null>(keyboard ? 0 : null);
  useDismiss(ref, anchor, onClose);
  useLayoutEffect(() => {
    ref.current?.focus({ preventScroll: true });
  }, [ref]);

  const pick = (row: MenuRow, pointer: boolean) => {
    onClose({ restore: true, pointer });
    row.onSelect();
  };

  const onKeyDown = (event: KeyboardEvent<HTMLDivElement>) => {
    const row = active === null ? undefined : rows[active];
    switch (event.key) {
      case "ArrowDown":
      case "ArrowUp":
        setActive(stepped(active, event.key === "ArrowDown" ? 1 : -1, rows.length));
        break;
      case "Home":
        setActive(0);
        break;
      case "End":
        setActive(rows.length - 1);
        break;
      case "Enter":
      case " ":
        if (row !== undefined) pick(row, false);
        break;
      case "Escape":
        onClose({ restore: true, pointer: false });
        break;
      case "Tab":
        // The focus moves on from the button, as Tab goes on from there.
        anchor.focus({ preventScroll: true });
        onClose({ restore: false, pointer: false });
        return;
      default:
        return;
    }
    event.preventDefault();
    event.stopPropagation();
  };

  if (layer === null) return null;
  return createPortal(
    <div
      ref={ref}
      role="menu"
      aria-label={label}
      tabIndex={-1}
      className="blockr-card-menu"
      aria-activedescendant={active === null ? undefined : `${id}-${String(active)}`}
      onKeyDown={onKeyDown}
    >
      {rows.map((row, i) => (
        <Fragment key={row.key}>
          {row.divided === true ? <hr className="blockr-card-menu-divider" /> : null}
          <button
            type="button"
            role="menuitem"
            tabIndex={-1}
            id={`${id}-${String(i)}`}
            className="blockr-card-menu-row"
            data-active={i === active ? "" : undefined}
            data-danger={row.danger === true ? "" : undefined}
            // The focus stays on the menu.
            onPointerDown={(event) => {
              event.preventDefault();
            }}
            onPointerMove={() => {
              if (active !== i) setActive(i);
            }}
            onClick={(event) => {
              pick(row, event.detail !== 0);
            }}
          >
            {row.icon === undefined ? null : (
              <span className="blockr-card-menu-icon" aria-hidden="true">
                {row.icon}
              </span>
            )}
            <span className="blockr-card-menu-label">{row.label}</span>
            {row.meta === undefined ? null : <span className="blockr-card-menu-meta">{row.meta}</span>}
          </button>
        </Fragment>
      ))}
    </div>,
    layer,
  );
}
