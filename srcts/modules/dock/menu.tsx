// The dock's menus, drawn as the design system's menus are
// (https://github.com/BristolMyersSquibb/blockr.ui/blob/e14e8310481e98b156871bf96669b41cb88c8435/vignettes/articles/design-system.Rmd#L1062-L1165):
// the panel picker (./picker.tsx), the menus of the bar (./navbar.tsx), and
// the menu and the "+" menu of a block's card (./card).
// The dock draws them itself, since blockr.ui gives the client no menus yet.
//
// A menu is a popover, which the browser draws in the top layer, so that
// nothing on the page clips it, and which it closes as the user clicks
// outside it.
// CSS anchor positioning places it 4px below the button that opened it, lined
// up with the button's left edge, or with its right edge for a button at the
// end of a row, and above the button where the window has no room below.
// A menu inside a menu sits 4px beside the row that opened it, and flips to
// its other side where the window has no room.
// In a browser without anchor positioning, script places it so (./place.ts).
//
// A menu may have a caption, a search on top, and rows in groups, a group
// under its title.
// The search keeps the rows whose label, detail or search text holds every
// word typed.
// Where a menu has no row to list at all, a note says why in place of its
// search and its rows.
// A row is one of these:
// - An action, which runs as the user picks it, and which is greyed out while
//   it is disabled, with the reason as its tooltip.
//   An action may be the current pick of a set, as the active view in the
//   views menu, set in weight 600, or a toggle, which ends on a check while it
//   is on, as the design system's rows show them.
// - A link, which opens in a new tab.
// - A status, text in the muted colour, which runs nothing, and which the
//   arrows pass over.
// - A menu, which opens beside the row that holds it.
// - A fold, which opens and closes in place as the user picks it, as a folder
//   in the views menu does (./views-menu.tsx).
//   The rows that it holds follow it while it is open, one level deeper.
//   Its chevron points right while it is closed and down while it is open,
//   and every row of a group that has a fold leaves room for that chevron, so
//   that the labels line up.
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
// - ArrowRight, Enter or a click opens the menu of a row that holds one, and
//   ArrowLeft or Escape closes it again, with the focus back on the menu that
//   holds the row.
// - ArrowRight opens a closed fold, or moves into an open one, and ArrowLeft
//   closes an open fold, or moves from a row inside a fold to the fold.
//   Enter, Space and a click open and close a fold.
// - A menu that the keyboard opened starts on its current pick, where it has
//   one.
// - Escape closes a menu that a button opened, and the focus goes back to the
//   button.
// - Tab closes every menu, and the focus moves on from the button.
// - A pick closes the menu and the menus that it opened from, the focus goes
//   back to the button, without a ring after a click, and then the row runs.

import { Fragment, useCallback, useEffect, useId, useLayoutEffect, useMemo, useRef, useState } from "react";
import type { CSSProperties, KeyboardEvent, MouseEvent, ReactElement, ReactNode, RefObject, SyntheticEvent } from "react";
import { useFloating, usePopover } from "./floating.ts";
import { CheckIcon, Chevron } from "./icons.tsx";
import { besideSpot, menuSpot } from "./place.ts";
import type { Align, Box, Size } from "./place.ts";
import { ariaShortcuts } from "./shortcuts.ts";

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
  // The row's tooltip, as why a disabled row is disabled.
  readonly tooltip?: string;
  // The shortcut that runs the row from anywhere on the page, which a screen
  // reader announces (./shortcuts.ts).
  readonly shortcut?: string;
  // Whether the row is listed, but runs nothing.
  readonly disabled?: boolean;
  // A link, which opens in a new tab.
  readonly href?: string;
  // A status, which runs nothing.
  readonly status?: boolean;
  // A menu, which opens beside the row.
  readonly rows?: readonly MenuRow[];
  // Whether an action is the current pick of a set, where it is one of a set.
  readonly current?: boolean;
  // Whether a toggle is on, where the action is one.
  readonly checked?: boolean;
  // A fold, which opens and closes in place, with whether it is open and what
  // opens and closes it.
  // The rows that it holds follow it in the list while it is open, each one
  // level deeper.
  readonly fold?: { readonly open: boolean; readonly onToggle: (open: boolean) => void };
  // How many folds hold the row, 0 unless given.
  readonly depth?: number;
  // What an action runs.
  readonly onSelect?: () => void;
}

export interface MenuGroup {
  readonly key: string;
  readonly title?: string;
  readonly rows: readonly MenuRow[];
}

// How a menu closes: whether the focus goes back to its button, or to the
// menu that holds its row, and whether a click closed it, after which the
// focus shows no ring.
export interface Closing {
  readonly restore: boolean;
  readonly pointer: boolean;
}

export interface MenuProps {
  // The button that opened the menu, or the row for a menu inside a menu.
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
  // How deep the menu sits: 0 for a menu that a button opened, 1 for a menu
  // inside it, and so on.
  readonly depth?: number;
  // Closes this menu.
  readonly onClose: (how: Closing) => void;
  // Closes every menu from the one that the button opened, as a pick does.
  readonly onCloseAll?: (how: Closing) => void;
}

// The menu of a row that is open, with the row, and whether the keyboard
// opened it.
interface Sub {
  readonly key: string;
  readonly row: HTMLElement;
  readonly keyboard: boolean;
}

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

// The index of the fold that holds the row at `at`, among the rows that the
// arrows stop on, or -1 where no fold holds it.
function foldOf(rows: readonly MenuRow[], at: number): number {
  const depth = rows[at]?.depth ?? 0;
  for (let i = at - 1; i >= 0 && depth > 0; i -= 1) {
    if ((rows[i]?.depth ?? 0) < depth) return i;
  }
  return -1;
}

// Whether the browser places a menu by CSS anchor positioning.
function anchored(doc: Document): boolean {
  return doc.defaultView?.CSS.supports("position-area", "block-end") === true;
}

// The anchor name that the button or the row which opened a menu takes, by
// which CSS places the menu.
// One menu of each depth shows at a time, since showing one closes every other
// but those that it opens from.
function anchorName(depth: number): string {
  return `--blockr-dock-menu-${String(depth)}`;
}

// Whether the arrows stop on a row, as on every row but a status.
const reachable = (row: MenuRow) => row.status !== true;

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

export interface MenuSurface {
  // How deep the menu sits: 0 for a menu that a button opened.
  readonly depth: number;
  readonly align: Align;
  // Whether the keyboard opened the menu.
  readonly keyboard: boolean;
  readonly onClose: (how: Closing) => void;
}

// Shows the element of the ref as the popover of a menu, by the button or the
// row that opened it, from before the browser paints it on, and closes it as
// a click or the focus lands outside it and its button.
// The button or the row takes the anchor name of the menu's depth, by which
// CSS anchor positioning places the popover, and script places it where the
// browser has no anchor positioning (./place.ts).
// Safari does not focus a button that the user clicks, so the menu does, and
// the browser gives the focus back to the button as the menu closes.
// A popover of the views menu that is no menu, its manage mode, shows the same
// way (./views-menu.tsx).
export function useMenuSurface(ref: RefObject<HTMLElement | null>, anchor: HTMLElement, { depth, align, keyboard, onClose }: MenuSurface): void {
  const [placement] = useState(() => {
    if (anchored(anchor.ownerDocument)) return null;
    return depth === 0 ? (trigger: Box, size: Size, view: Size) => menuSpot(trigger, size, view, align) : besideSpot;
  });
  useLayoutEffect(() => {
    const name = anchorName(depth);
    if (depth === 0 && !keyboard) anchor.focus({ preventScroll: true, focusVisible: false });
    anchor.style.setProperty("anchor-name", name);
    ref.current?.style.setProperty("position-anchor", name);
    return () => {
      anchor.style.removeProperty("anchor-name");
    };
  }, [ref, anchor, keyboard, depth]);
  usePopover(ref, anchor);
  useFloating(ref, anchor, placement);
  useDismiss(ref, anchor, onClose);
}

export function Menu({
  anchor,
  label,
  align = "start",
  caption,
  search,
  groups,
  note,
  keyboard,
  depth = 0,
  onClose,
  onCloseAll,
}: MenuProps): ReactElement {
  const ref = useRef<HTMLDivElement>(null);
  const field = useRef<HTMLInputElement>(null);
  const id = useId();
  const closeAll = onCloseAll ?? onClose;
  const [searched, setSearched] = useState("");
  const [active, setActive] = useState<number | null>(() => {
    if (search !== undefined) return 0;
    if (!keyboard) return null;
    return Math.max(
      0,
      groups
        .flatMap((group) => group.rows)
        .filter(reachable)
        .findIndex((row) => row.current === true),
    );
  });
  const [sub, setSub] = useState<Sub | null>(null);

  const listed = groups.some((group) => group.rows.length > 0);
  const shown = useMemo(
    () =>
      groups
        .map((group) => ({ ...group, rows: group.rows.filter((row) => matches(row, searched)) }))
        .filter((group) => group.rows.length > 0),
    [groups, searched],
  );
  const rows = shown.flatMap((group) => group.rows).filter(reachable);
  const rowId = (i: number) => `${id}-row-${String(i)}`;
  const activeId = active !== null && active < rows.length ? rowId(active) : undefined;

  useMenuSurface(ref, anchor, { depth, align, keyboard, onClose });
  useLayoutEffect(() => {
    (field.current ?? ref.current)?.focus({ preventScroll: true });
  }, []);

  // The active row stays in view as the arrows move it.
  useLayoutEffect(() => {
    if (activeId !== undefined) ref.current?.querySelector(`[id="${activeId}"]`)?.scrollIntoView({ block: "nearest" });
  }, [activeId]);

  // Closes the menu of a row, and gives the focus back to this menu, where the
  // row stays the active one.
  const closeSub = useCallback((how: Closing) => {
    setSub(null);
    if (how.restore) ref.current?.focus({ preventScroll: true });
  }, []);

  // The element of the row of the index given.
  const rowElement = (i: number) => ref.current?.querySelector<HTMLElement>(`[id="${rowId(i)}"]`) ?? null;

  const pick = (row: MenuRow, i: number, pointer: boolean) => {
    if (row.disabled === true) return;
    if (row.fold !== undefined) {
      setActive(i);
      row.fold.onToggle(!row.fold.open);
      return;
    }
    if (row.rows !== undefined) {
      const element = rowElement(i);
      if (element !== null) {
        setActive(i);
        setSub({ key: row.key, row: element, keyboard: !pointer });
      }
      return;
    }
    if (row.href !== undefined) {
      // The link opens as the browser follows it, which a click on it does.
      rowElement(i)?.click();
      return;
    }
    closeAll({ restore: true, pointer });
    row.onSelect?.();
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
      case "ArrowRight":
        if (search !== undefined || row === undefined || active === null) return;
        if (row.fold !== undefined) {
          // An open fold holds the row after it, where it holds any.
          if (!row.fold.open) row.fold.onToggle(true);
          else if ((rows[active + 1]?.depth ?? 0) > (row.depth ?? 0)) setActive(active + 1);
          break;
        }
        if (row.rows === undefined) return;
        pick(row, active, false);
        break;
      case "ArrowLeft": {
        if (search !== undefined) return;
        const fold = active === null ? -1 : foldOf(rows, active);
        if (row?.fold?.open === true) row.fold.onToggle(false);
        else if (fold >= 0) setActive(fold);
        else if (depth === 0) return;
        else onClose({ restore: true, pointer: false });
        break;
      }
      case " ":
        if (search !== undefined) return;
        if (row !== undefined && active !== null) pick(row, active, false);
        break;
      case "Enter":
        if (row !== undefined && active !== null) pick(row, active, false);
        break;
      case "Escape":
        onClose({ restore: true, pointer: false });
        break;
      case "Tab":
        // The focus moves on from the button that opened the menus, as Tab
        // goes on from there.
        closeAll({ restore: true, pointer: false });
        return;
      default:
        return;
    }
    event.preventDefault();
    event.stopPropagation();
  };

  // The index of the first reachable row of each group among the rows shown.
  const starts = shown.map((_, g) =>
    shown.slice(0, g).reduce((count, group) => count + group.rows.filter(reachable).length, 0),
  );

  // Draws a row, which leaves room for the chevron of a fold where its group
  // has a fold.
  const drawRow = (row: MenuRow, i: number, folds: boolean) => {
    const common = {
      id: rowId(i),
      className: "blockr-dock-menu-row",
      style: row.depth === undefined ? undefined : ({ "--depth": row.depth } as CSSProperties),
      "data-active": i === active ? "" : undefined,
      "data-danger": row.danger === true ? "" : undefined,
      title: row.tooltip,
      // The focus stays in the menu.
      onPointerDown: (event: SyntheticEvent) => {
        event.preventDefault();
      },
      onPointerMove: () => {
        if (active !== i) setActive(i);
      },
    };
    const content = (
      <>
        {row.fold !== undefined ? (
          <span className="blockr-dock-menu-fold" aria-hidden="true">
            <Chevron />
          </span>
        ) : folds ? (
          <span className="blockr-dock-menu-fold" aria-hidden="true" />
        ) : null}
        {row.lead === undefined ? null : (
          <span className="blockr-dock-menu-lead" aria-hidden="true">
            {row.lead}
          </span>
        )}
        <span className="blockr-dock-menu-label">{row.label}</span>
        {row.detail === undefined ? null : <span className="blockr-dock-menu-detail">{row.detail}</span>}
        {row.badge === undefined ? null : <span className="blockr-dock-menu-badge">{row.badge}</span>}
        {row.meta === undefined ? null : <span className="blockr-dock-menu-meta">{row.meta}</span>}
        {row.checked === true ? <CheckIcon /> : null}
      </>
    );
    if (search !== undefined) {
      return (
        <div
          {...common}
          role="option"
          aria-selected={i === active}
          onClick={(event: MouseEvent) => {
            pick(row, i, event.detail !== 0);
          }}
        >
          {content}
        </div>
      );
    }
    if (row.href !== undefined) {
      return (
        <a
          {...common}
          role="menuitem"
          tabIndex={-1}
          href={row.href}
          target="_blank"
          rel="noopener noreferrer"
          onClick={(event: MouseEvent) => {
            // The menus close once the browser has followed the link, which it
            // does only from a link that is still on the page.
            const pointer = event.detail !== 0;
            setTimeout(() => {
              closeAll({ restore: true, pointer });
            }, 0);
          }}
        >
          {content}
        </a>
      );
    }
    if (row.rows !== undefined) {
      const open = sub?.key === row.key;
      return (
        <div role="none">
          <button
            {...common}
            type="button"
            role="menuitem"
            tabIndex={-1}
            aria-haspopup="menu"
            aria-expanded={open}
            onClick={(event: MouseEvent) => {
              if (open) closeSub({ restore: false, pointer: true });
              else pick(row, i, event.detail !== 0);
            }}
          >
            {content}
            <Chevron right />
          </button>
          {open ? (
            <Menu
              anchor={sub.row}
              label={row.label}
              groups={[{ key: row.key, rows: row.rows }]}
              keyboard={sub.keyboard}
              depth={depth + 1}
              onClose={closeSub}
              onCloseAll={closeAll}
            />
          ) : null}
        </div>
      );
    }
    return (
      <button
        {...common}
        type="button"
        role={row.current !== undefined ? "menuitemradio" : row.checked !== undefined ? "menuitemcheckbox" : "menuitem"}
        tabIndex={-1}
        aria-checked={row.current ?? row.checked}
        aria-expanded={row.fold?.open}
        aria-disabled={row.disabled === true ? true : undefined}
        aria-keyshortcuts={ariaShortcuts(row.shortcut)}
        onClick={(event: MouseEvent) => {
          pick(row, i, event.detail !== 0);
        }}
      >
        {content}
      </button>
    );
  };

  const drawGroup = (group: MenuGroup, g: number) => {
    const folds = group.rows.some((row) => row.fold !== undefined);
    const drawn = group.rows.map((row, r) => {
      const element =
        row.status === true ? (
          <div role="none" className="blockr-dock-menu-status" title={row.tooltip}>
            {row.label}
          </div>
        ) : (
          drawRow(row, (starts[g] ?? 0) + group.rows.slice(0, r).filter(reachable).length, folds)
        );
      return (
        <Fragment key={row.key}>
          {row.divided === true && search === undefined ? <hr className="blockr-dock-menu-divider" /> : null}
          {element}
        </Fragment>
      );
    });
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
      data-depth={depth}
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

// What a menu of the bar holds, as ./navbar.tsx gives it: links, buttons,
// statuses and menus inside the menu.
export type MenuEntry =
  | { readonly kind: "link"; readonly id: string; readonly label: string; readonly href: string }
  | {
      readonly kind: "button";
      readonly id: string;
      readonly label: string;
      readonly tooltip?: string;
      readonly shortcut?: string;
      readonly disabled?: boolean;
      readonly run: () => void;
    }
  | { readonly kind: "status"; readonly id: string; readonly text: string; readonly tooltip?: string }
  | { readonly kind: "menu"; readonly id: string; readonly label: string; readonly entries: readonly MenuEntry[] };

// The rows of the entries of a menu of the bar.
function entryRows(entries: readonly MenuEntry[]): MenuRow[] {
  return entries.map((entry): MenuRow => {
    switch (entry.kind) {
      case "link":
        return { key: entry.id, label: entry.label, href: entry.href };
      case "button":
        return {
          key: entry.id,
          label: entry.label,
          ...(entry.tooltip === undefined ? {} : { tooltip: entry.tooltip }),
          ...(entry.shortcut === undefined ? {} : { shortcut: entry.shortcut }),
          ...(entry.disabled === true ? { disabled: true } : {}),
          onSelect: entry.run,
        };
      case "status":
        return { key: entry.id, label: entry.text, status: true, ...(entry.tooltip === undefined ? {} : { tooltip: entry.tooltip }) };
      case "menu":
        return { key: entry.id, label: entry.label, rows: entryRows(entry.entries) };
    }
  });
}

export interface MenuButtonProps {
  readonly label: string;
  readonly entries: readonly MenuEntry[];
  // The class of the button, which draws it as the bar's other buttons.
  readonly className: string;
  // Sets the button's tooltip as the pointer comes onto it or the focus moves
  // into it, such as the label in full where it is cut short.
  readonly onTip?: (button: HTMLElement) => void;
}

// A menu of the bar, as a quiet button with its label and the design system's
// chevron, which opens one of the dock's menus.
export function MenuButton({ label, entries, className, onTip }: MenuButtonProps): ReactElement {
  const [open, setOpen] = useState<{ readonly anchor: HTMLElement; readonly keyboard: boolean } | null>(null);
  const button = useRef<HTMLButtonElement>(null);
  const props = useMenuButton(
    open !== null,
    (anchor, keyboard) => {
      setOpen({ anchor, keyboard });
    },
    () => {
      setOpen(null);
    },
  );
  const close = useCallback((how: Closing) => {
    setOpen(null);
    if (how.restore) button.current?.focus({ preventScroll: true, focusVisible: !how.pointer });
  }, []);
  const tip = (event: SyntheticEvent<HTMLElement>) => {
    onTip?.(event.currentTarget);
  };

  return (
    <>
      <button ref={button} type="button" className={className} {...props} onPointerEnter={tip} onFocus={tip}>
        <span className="blockr-dock-bar-label">{label}</span>
        <Chevron />
      </button>
      {open === null ? null : (
        <Menu
          anchor={open.anchor}
          label={label}
          groups={[{ key: "entries", rows: entryRows(entries) }]}
          keyboard={open.keyboard}
          onClose={close}
        />
      )}
    </>
  );
}
