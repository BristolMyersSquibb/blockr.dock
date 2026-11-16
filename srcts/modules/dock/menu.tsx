// The dock's menus, drawn as the design system's menus are
// (https://github.com/BristolMyersSquibb/blockr.ui/blob/e14e8310481e98b156871bf96669b41cb88c8435/vignettes/articles/design-system.Rmd#L1062-L1165):
// the panel picker (./picker.tsx), and the menu and the "+" menu of a block's
// card (./card).
//
// A menu is a popover, which the browser draws in the top layer, so that no
// panel clips it, and which it closes as the user clicks outside it.
// CSS anchor positioning places it 4px below the button that opened it, lined
// up with the button's left edge, or with its right edge for a button at the
// end of a row, and above the button where the window has no room below.
// In a browser without anchor positioning, script places it so (./place.ts).
//
// A menu may have a caption, a search on top, and rows in groups, a group
// under its title.
// The search keeps the rows whose label, detail or search text holds every
// word typed.
// Where a menu has no row to list at all, a note says why in place of its
// search and its rows.
//
// The keys are the design system's: the focus stays in the search, or on the
// menu where it has none, and the row that the keyboard is on is the menu's
// active descendant.
// - The arrows move the row, from the last round to the first, and Enter picks
//   it, as a click does.
// - In a menu without a search, Home and End jump to the first and the last
//   row, and Space picks too.
// - A menu with a search starts on its first row, so that Enter picks the
//   first row that the search keeps, and a menu without one does where the
//   keyboard opened it.
// - The pointer moves the same row.
// - Escape closes the menu, and the focus goes back to its button.
// - Tab closes it, and the focus moves on from its button.
// - A pick closes the menu, the focus goes back to its button, without a ring
//   after a click, and then the row runs.

import { Fragment, useEffect, useId, useLayoutEffect, useMemo, useRef, useState } from "react";
import type { KeyboardEvent, MouseEvent, ReactElement, ReactNode, RefObject } from "react";
import { useFloating, usePopover } from "./floating.ts";
import { menuSpot } from "./place.ts";
import type { Align, Box, Size } from "./place.ts";

export interface MenuRow {
  readonly key: string;
  readonly label: string;
  // What comes before the label: an icon, or the mark of a block's class.
  readonly lead?: ReactNode;
  // Muted text after the label, as the name of a block's class.
  readonly detail?: string;
  // A neutral badge after the label, as the package of a block class.
  readonly badge?: string;
  // Text after the label in a monospaced face, as the id of a block.
  readonly meta?: string;
  // Text by which the search finds the row besides its label and its detail,
  // as an id.
  readonly search?: string;
  // Whether the row destroys something, which it shows as the active row.
  readonly danger?: boolean;
  // Whether a divider comes before the row.
  readonly divided?: boolean;
  readonly onSelect: () => void;
}

export interface MenuGroup {
  readonly key: string;
  readonly title?: string;
  readonly rows: readonly MenuRow[];
}

// How a menu closes: whether the focus goes back to its button, and whether a
// click closed it, after which the focus shows no ring.
export interface Closing {
  readonly restore: boolean;
  readonly pointer: boolean;
}

export interface MenuProps {
  // The button that opened the menu.
  readonly anchor: HTMLElement;
  // The menu's name, which its list takes.
  readonly label: string;
  readonly align?: Align;
  readonly caption?: string;
  // The search, by its name and what it says where it keeps no row.
  readonly search?: { readonly label: string; readonly empty: string };
  readonly groups: readonly MenuGroup[];
  // What the menu says where it has no row to list.
  readonly note?: string;
  // Whether the keyboard opened the menu.
  readonly keyboard: boolean;
  readonly onClose: (how: Closing) => void;
}

// The anchor name that the button which opened a menu takes, by which CSS
// places the menu.
// One menu shows at a time, since showing one closes the other.
const ANCHOR = "--blockr-dock-menu";

// Whether a row holds every word of what the user searched, in its label, its
// detail or its search text, in any case.
export function matches(row: MenuRow, searched: string): boolean {
  const words = searched.toLowerCase().split(/\s+/).filter((word) => word !== "");
  const text = [row.label, row.detail ?? "", row.search ?? ""].join(" ").toLowerCase();
  return words.every((word) => text.includes(word));
}

// The index of the row after `at`, or before it for a step of -1, from the
// last row round to the first, or the first or the last where no row is
// active.
export function stepped(at: number | null, step: 1 | -1, count: number): number {
  if (at === null) return step === 1 ? 0 : count - 1;
  return (at + step + count) % count;
}

// Whether the browser places a menu by CSS anchor positioning.
function anchored(doc: Document): boolean {
  return doc.defaultView?.CSS.supports("position-area", "block-end") === true;
}

// Closes a menu as a click or the focus lands outside it and its button, as
// the browser closes a popover by a click outside it.
// A menu that has gone closes nothing, as where the next menu takes the focus
// before React has removed the listeners of the menu that it replaced.
function useDismiss(menu: RefObject<HTMLElement | null>, anchor: HTMLElement, close: (how: Closing) => void): void {
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

export function Menu({ anchor, label, align = "start", caption, search, groups, note, keyboard, onClose }: MenuProps): ReactElement {
  const ref = useRef<HTMLDivElement>(null);
  const field = useRef<HTMLInputElement>(null);
  const id = useId();
  const [searched, setSearched] = useState("");
  const [active, setActive] = useState<number | null>(search !== undefined || keyboard ? 0 : null);
  const [placement] = useState(() =>
    anchored(anchor.ownerDocument) ? null : (trigger: Box, size: Size, view: Size) => menuSpot(trigger, size, view, align),
  );

  const listed = groups.some((group) => group.rows.length > 0);
  const shown = useMemo(
    () =>
      groups
        .map((group) => ({ ...group, rows: group.rows.filter((row) => matches(row, searched)) }))
        .filter((group) => group.rows.length > 0),
    [groups, searched],
  );
  const rows = shown.flatMap((group) => group.rows);
  const rowId = (i: number) => `${id}-row-${String(i)}`;
  const activeId = active !== null && active < rows.length ? rowId(active) : undefined;

  // Safari does not focus a button that the user clicks, so the menu does,
  // and the browser gives the focus back to the button as the menu closes.
  useLayoutEffect(() => {
    if (!keyboard) anchor.focus({ preventScroll: true, focusVisible: false });
    anchor.style.setProperty("anchor-name", ANCHOR);
    return () => {
      anchor.style.removeProperty("anchor-name");
    };
  }, [anchor, keyboard]);
  usePopover(ref, anchor);
  useFloating(ref, anchor, placement);
  useLayoutEffect(() => {
    (field.current ?? ref.current)?.focus({ preventScroll: true });
  }, []);
  useDismiss(ref, anchor, onClose);

  // The active row stays in view as the arrows move it.
  useLayoutEffect(() => {
    if (activeId !== undefined) ref.current?.querySelector(`[id="${activeId}"]`)?.scrollIntoView({ block: "nearest" });
  }, [activeId]);

  const pick = (row: MenuRow, pointer: boolean) => {
    onClose({ restore: true, pointer });
    row.onSelect();
  };

  const onKeyDown = (event: KeyboardEvent<HTMLDivElement>) => {
    if (event.nativeEvent.isComposing) return;
    const row = active === null ? undefined : rows[active];
    switch (event.key) {
      case "ArrowDown":
      case "ArrowUp":
        if (rows.length > 0) setActive(stepped(active, event.key === "ArrowDown" ? 1 : -1, rows.length));
        break;
      case "Home":
      case "End":
        if (search !== undefined) return;
        setActive(event.key === "Home" ? 0 : rows.length - 1);
        break;
      case " ":
        if (search !== undefined) return;
        if (row !== undefined) pick(row, false);
        break;
      case "Enter":
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

  // The index of the first row of each group among the rows shown.
  const starts = shown.map((_, g) => shown.slice(0, g).reduce((count, group) => count + group.rows.length, 0));
  const drawRow = (row: MenuRow, i: number) => {
    return (
      <Fragment key={row.key}>
        {row.divided === true && search === undefined ? <hr className="blockr-dock-menu-divider" /> : null}
        <div
          id={rowId(i)}
          role={search === undefined ? "menuitem" : "option"}
          aria-selected={search === undefined ? undefined : i === active}
          className="blockr-dock-menu-row"
          data-active={i === active ? "" : undefined}
          data-danger={row.danger === true ? "" : undefined}
          // The focus stays in the menu.
          onPointerDown={(event) => {
            event.preventDefault();
          }}
          onPointerMove={() => {
            if (active !== i) setActive(i);
          }}
          onClick={(event: MouseEvent) => {
            pick(row, event.detail !== 0);
          }}
        >
          {row.lead === undefined ? null : (
            <span className="blockr-dock-menu-lead" aria-hidden="true">
              {row.lead}
            </span>
          )}
          <span className="blockr-dock-menu-label">{row.label}</span>
          {row.detail === undefined ? null : <span className="blockr-dock-menu-detail">{row.detail}</span>}
          {row.badge === undefined ? null : <span className="blockr-dock-menu-badge">{row.badge}</span>}
          {row.meta === undefined ? null : <span className="blockr-dock-menu-meta">{row.meta}</span>}
        </div>
      </Fragment>
    );
  };
  const drawGroup = (group: MenuGroup, g: number) => {
    const drawn = group.rows.map((row, r) => drawRow(row, (starts[g] ?? 0) + r));
    return group.title === undefined ? (
      <Fragment key={group.key}>{drawn}</Fragment>
    ) : (
      <div key={group.key} role="group" aria-labelledby={`${id}-${group.key}`}>
        <div id={`${id}-${group.key}`} role="presentation" className="blockr-dock-menu-title">
          {group.title}
        </div>
        {drawn}
      </div>
    );
  };

  return (
    <div
      ref={ref}
      popover="auto"
      className="blockr-dock-menu"
      data-align={align}
      tabIndex={-1}
      onKeyDown={onKeyDown}
      onToggle={(event) => {
        if (event.newState === "closed") onClose({ restore: false, pointer: false });
      }}
      {...(search === undefined && listed ? { role: "menu", "aria-label": label, "aria-activedescendant": activeId } : {})}
    >
      {caption === undefined ? null : <p className="blockr-dock-menu-caption">{caption}</p>}
      {!listed ? (
        <p className="blockr-dock-menu-note">{note}</p>
      ) : search === undefined ? (
        shown.map(drawGroup)
      ) : (
        <>
          <div className="blockr-dock-menu-search">
            <input
              ref={field}
              type="search"
              role="combobox"
              aria-label={search.label}
              aria-expanded="true"
              aria-controls={`${id}-list`}
              aria-autocomplete="list"
              aria-activedescendant={activeId}
              placeholder={search.label}
              autoComplete="off"
              spellCheck={false}
              value={searched}
              onChange={(event) => {
                setSearched(event.target.value);
                setActive(0);
              }}
            />
          </div>
          <div id={`${id}-list`} role="listbox" aria-label={label}>
            {shown.map(drawGroup)}
          </div>
          {rows.length === 0 ? <p className="blockr-dock-menu-note">{search.empty}</p> : null}
        </>
      )}
    </div>
  );
}

// The props of a button that opens a menu, which it closes again where the
// menu is open, given whether the menu is open, and what opens and what closes
// it.
// The browser may close the menu as the pointer goes down on the button, which
// lies outside the menu, so the click that follows closes it rather than opens
// it again.
export function useMenuButton(open: boolean, show: (button: HTMLElement, keyboard: boolean) => void, close: () => void) {
  const pressedOpen = useRef(false);
  return {
    "aria-haspopup": "menu",
    "aria-expanded": open,
    onPointerDown: () => {
      pressedOpen.current = open;
    },
    onClick: (event: MouseEvent<HTMLElement>) => {
      // A click that the keyboard made, with Enter or Space, has no detail,
      // and no pointer went down for it.
      const keyboard = event.detail === 0;
      const wasOpen = open || (!keyboard && pressedOpen.current);
      pressedOpen.current = false;
      if (wasOpen) close();
      else show(event.currentTarget, keyboard);
    },
    onKeyDown: (event: KeyboardEvent<HTMLElement>) => {
      if (event.key !== "ArrowDown" || open) return;
      event.preventDefault();
      show(event.currentTarget, true);
    },
  } as const;
}
