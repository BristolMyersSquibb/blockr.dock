// The menus of the bar: a menu of links that an app script gives, and a menu
// among the storage controls (./navbar.tsx)
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.5#the-storage-plugins-controls).
// The dock draws them itself, in the look of a menu of actions in blockr.ui's
// design system, since blockr.ui gives the client no menus yet.
//
// A menu is a quiet button with its label and the design system's chevron,
// which opens a popover that lists what the menu holds:
// - A link, which opens in a new tab.
// - A button, which runs as the user picks it, and which is greyed out while
//   it is disabled.
// - A status, text in the muted colour, which runs nothing.
// - A menu, which opens beside the row that holds it.
// Each row shows its tooltip where it has one.
// A pick closes the menu, and the menus that it opened from.
//
// The browser draws the popover in the top layer, so nothing on the page clips
// it.
// CSS anchor positioning places it below its button, or beside its row for a
// menu inside a menu, and flips it where the window has no room.
// In a browser without anchor positioning, script places it (./popover.ts).
// A menu closes when the user picks, clicks outside it, presses Escape or
// moves the focus out of it, and the focus goes back to what opened it.
// Its keys are those of a menu button
// (https://www.w3.org/WAI/ARIA/apg/patterns/menu-button/):
// - Enter, Space and ArrowDown on the button open the menu, with the focus on
//   its first row. A click opens it with the focus on the menu itself, so that
//   no row looks picked.
// - ArrowDown and ArrowUp move through the rows, and Home and End move to the
//   first and the last.
// - Enter and Space pick a row, and ArrowRight opens a menu inside the menu.
//   ArrowLeft closes that menu again, with the focus back on its row.

import { useCallback, useLayoutEffect, useRef, useState } from "react";
import type { FocusEvent, KeyboardEvent, MouseEvent, ReactElement, RefObject, SyntheticEvent } from "react";
import { Chevron } from "./icons.tsx";
import { place } from "./popover.ts";
import { ariaShortcuts } from "./shortcuts.ts";

// What a menu holds.
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

// How a menu opened: from the keyboard, or by a click.
interface Opened {
  readonly keyboard: boolean;
}

// The rows of a menu, those inside a menu that it holds left out.
const ROWS = ':scope > [role="menuitem"], :scope > [role="none"] > [role="menuitem"]';

function rowsOf(list: HTMLElement): HTMLElement[] {
  return [...list.querySelectorAll<HTMLElement>(ROWS)];
}

// The anchor name that the button or the row which opened a menu takes, by
// which CSS places the menu.
// Only one menu of each depth is open at a time, since a popover that opens
// closes every other but those that it opens from.
function anchorName(depth: number): string {
  return `--blockr-dock-menu-${String(depth)}`;
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

// A menu, as a button that opens it.
export function MenuButton({ label, entries, className, onTip }: MenuButtonProps): ReactElement {
  const [opened, setOpened] = useState<Opened | undefined>(undefined);
  const button = useRef<HTMLButtonElement>(null);
  const list = useRef<HTMLDivElement>(null);
  const open = opened !== undefined;
  // Whether the menu was open as the pointer went down on the button.
  // The browser may close the menu then, as the pointer goes down outside it,
  // so the click that follows closes it rather than opens it again.
  const pressedOpen = useRef(false);
  const close = useCallback(() => {
    setOpened(undefined);
  }, []);
  usePopover(list, button, opened, 0);

  const tip = (event: SyntheticEvent<HTMLElement>) => {
    onTip?.(event.currentTarget);
  };

  return (
    <>
      <button
        ref={button}
        type="button"
        className={className}
        aria-haspopup="menu"
        aria-expanded={open}
        onPointerEnter={tip}
        onFocus={tip}
        onPointerDown={() => {
          pressedOpen.current = open;
        }}
        onClick={(event: MouseEvent<HTMLButtonElement>) => {
          // A click that the keyboard made, with Enter or Space, has no detail,
          // and no pointer went down for it.
          const keyboard = event.detail === 0;
          const wasOpen = open || (!keyboard && pressedOpen.current);
          pressedOpen.current = false;
          setOpened(wasOpen ? undefined : { keyboard });
        }}
        onKeyDown={(event: KeyboardEvent<HTMLButtonElement>) => {
          if (event.key !== "ArrowDown") return;
          event.preventDefault();
          const first = list.current === null ? undefined : rowsOf(list.current)[0];
          if (open) first?.focus();
          else setOpened({ keyboard: true });
        }}
      >
        <span className="blockr-dock-bar-label">{label}</span>
        <Chevron />
      </button>
      <MenuList list={list} label={label} opened={opened} close={close} closeAll={close} entries={entries} depth={0} />
    </>
  );
}

// Shows the popover of a menu while it is open, by the button or the row that
// opened it, and moves the focus into it.
function usePopover(
  list: RefObject<HTMLDivElement | null>,
  trigger: RefObject<HTMLElement | null>,
  opened: Opened | undefined,
  depth: number,
): void {
  useLayoutEffect(() => {
    const el = list.current;
    const at = trigger.current;
    if (el === null || at === null || opened === undefined) return undefined;
    // Safari does not focus a button that the user clicks, so the dock does,
    // and the browser gives the focus back to the button as the menu closes.
    if (!opened.keyboard) at.focus({ focusVisible: false });
    const name = anchorName(depth);
    at.style.setProperty("anchor-name", name);
    el.style.setProperty("position-anchor", name);
    el.showPopover({ source: at });
    place(el, at, depth === 0 ? "below" : "beside");
    const first = rowsOf(el)[0];
    if (opened.keyboard && first !== undefined) first.focus();
    else el.focus({ focusVisible: false });
    return () => {
      at.style.removeProperty("anchor-name");
      el.hidePopover();
    };
  }, [list, trigger, opened, depth]);
}

interface MenuListProps {
  readonly list: RefObject<HTMLDivElement | null>;
  readonly label: string;
  readonly opened: Opened | undefined;
  // Closes this menu, and every menu that it opened.
  readonly close: () => void;
  // Closes the menu that the bar's button opened, and every menu inside it.
  readonly closeAll: () => void;
  readonly entries: readonly MenuEntry[];
  readonly depth: number;
}

// The popover of a menu, which lists its rows while the menu is open.
function MenuList({ list, label, opened, close, closeAll, entries, depth }: MenuListProps): ReactElement {
  return (
    <div
      ref={list}
      popover="auto"
      role="menu"
      aria-label={label}
      tabIndex={-1}
      className="blockr-dock-menu"
      data-depth={depth}
      onToggle={(event) => {
        if (event.newState === "closed") close();
      }}
      onBlur={(event: FocusEvent<HTMLDivElement>) => {
        const next = event.relatedTarget;
        if (next instanceof Node && !event.currentTarget.contains(next)) close();
      }}
      onKeyDown={moveThrough}
    >
      {opened === undefined
        ? null
        : entries.map((entry) => <Row key={entry.id} entry={entry} depth={depth} closeAll={closeAll} />)}
    </div>
  );
}

// Moves the focus through the rows of the menu whose popover hears the key.
// A menu inside it hears its own keys first, and keeps them.
function moveThrough(event: KeyboardEvent<HTMLDivElement>): void {
  const rows = rowsOf(event.currentTarget);
  const at = rows.findIndex((row) => row === event.target);
  const last = rows.length - 1;
  let next: HTMLElement | undefined;
  if (event.key === "ArrowDown") next = rows[at < 0 || at === last ? 0 : at + 1];
  else if (event.key === "ArrowUp") next = rows[at <= 0 ? last : at - 1];
  else if (event.key === "Home") next = rows[0];
  else if (event.key === "End") next = rows[last];
  else return;
  event.preventDefault();
  event.stopPropagation();
  next?.focus();
}

function Row({ entry, depth, closeAll }: { readonly entry: MenuEntry; readonly depth: number; readonly closeAll: () => void }): ReactElement {
  switch (entry.kind) {
    case "link":
      return (
        <a
          role="menuitem"
          tabIndex={-1}
          className="blockr-dock-menu-item"
          href={entry.href}
          target="_blank"
          rel="noopener noreferrer"
          onClick={closeAll}
          onKeyDown={(event) => {
            // Space picks a row too, which a link does not take by itself.
            if (event.key !== " ") return;
            event.preventDefault();
            event.currentTarget.click();
          }}
        >
          <span className="blockr-dock-menu-label">{entry.label}</span>
        </a>
      );
    case "button": {
      const disabled = entry.disabled === true;
      return (
        <button
          type="button"
          role="menuitem"
          tabIndex={-1}
          className="blockr-dock-menu-item"
          aria-disabled={disabled ? true : undefined}
          title={entry.tooltip}
          aria-keyshortcuts={ariaShortcuts(entry.shortcut)}
          onClick={() => {
            if (disabled) return;
            closeAll();
            entry.run();
          }}
        >
          <span className="blockr-dock-menu-label">{entry.label}</span>
        </button>
      );
    }
    case "status":
      return (
        <div role="none" className="blockr-dock-menu-status" title={entry.tooltip}>
          {entry.text}
        </div>
      );
    case "menu":
      return <SubMenu entry={entry} depth={depth + 1} closeAll={closeAll} />;
  }
}

// A menu inside a menu: a row that opens it beside itself.
function SubMenu({
  entry,
  depth,
  closeAll,
}: {
  readonly entry: Extract<MenuEntry, { kind: "menu" }>;
  readonly depth: number;
  readonly closeAll: () => void;
}): ReactElement {
  const [opened, setOpened] = useState<Opened | undefined>(undefined);
  const row = useRef<HTMLButtonElement>(null);
  const list = useRef<HTMLDivElement>(null);
  const open = opened !== undefined;
  const close = useCallback(() => {
    setOpened(undefined);
  }, []);
  usePopover(list, row, opened, depth);

  return (
    <div role="none" className="blockr-dock-menu-group">
      <button
        ref={row}
        type="button"
        role="menuitem"
        tabIndex={-1}
        className="blockr-dock-menu-item"
        aria-haspopup="menu"
        aria-expanded={open}
        onClick={(event) => {
          setOpened(open ? undefined : { keyboard: event.detail === 0 });
        }}
        onKeyDown={(event) => {
          if (event.key !== "ArrowRight") return;
          event.preventDefault();
          event.stopPropagation();
          setOpened({ keyboard: true });
        }}
      >
        <span className="blockr-dock-menu-label">{entry.label}</span>
        <Chevron right />
      </button>
      <div
        className="blockr-dock-menu-nest"
        onKeyDown={(event) => {
          if (event.key !== "ArrowLeft") return;
          event.preventDefault();
          event.stopPropagation();
          close();
          row.current?.focus();
        }}
      >
        <MenuList list={list} label={entry.label} opened={opened} close={close} closeAll={closeAll} entries={entry.entries} depth={depth} />
      </div>
    </div>
  );
}
