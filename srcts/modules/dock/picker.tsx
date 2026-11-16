// The panel picker, which lists what the active view does not show, and adds
// the panel that the user picks to the view
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.4#a-plugins-panel).
// It lists the board's blocks that the view does not show, each by its name
// with the name of its block class beside it, in the order of the board.
// After them, under "Plugins", it lists the app's plugins with a `ui` that the
// view does not show, by their names, in the order of the app's plugins.
// A search field at its top filters them by name, by the name of a block's
// class and by id, so that the picker serves a board of many blocks.
// Today's Blockr.menu shows one only where it lists more than eight, at
// 655e323.
// Where the picker has nothing to list, it says why in place of the list.
//
// Two buttons open it: the "+" in the header of each tab group, and the "Add
// panel" button of an empty view.
// A pick runs blockr.dock-add_panel on the active view.
// From a tab group's "+", the pick goes near the panel that the group shows,
// so the panel becomes a new tab in that group.
// From an empty view, the panel goes to the default place.
// Where the board refuses the command, core tells the user why, as an error of
// the code board_edit_failed.
//
// The picker is a popover.
// The browser draws it in the top layer, so no tab group clips it.
// It closes when the user picks, clicks outside it, presses Escape or moves
// the focus out of it.
// CSS anchor positioning places it below the button that opened it, or above
// the button where the window has no room below.
// In a browser without anchor positioning, script places it.
// Its keys are those of a menu button
// (https://www.w3.org/WAI/ARIA/apg/patterns/menu-button/):
// - A click, Enter, Space and ArrowDown on the button open the picker, with
//   the focus on its search field.
// - ArrowDown and ArrowUp move through the entries, and Home and End move to
//   the first and the last.
// - ArrowDown in the search field moves to the first entry, and Enter there
//   picks it.
// - Escape closes the picker, and the focus goes back to the button.

import type { BlockRegistry, Board, Json, PluginClassId } from "@blockr/core";
import { createContext, useCallback, useContext, useId, useLayoutEffect, useMemo, useRef, useState } from "react";
import type { KeyboardEvent, MouseEvent, ReactElement, ReactNode } from "react";
import { activeView, stateOf } from "../../commands/index.ts";
import { places } from "../../state/index.ts";
import type { Panel } from "../../state/index.ts";
import { useBoard, useDock } from "./context.ts";

// An entry of the picker: the panel of a block or of a plugin, its name, and,
// for a block, the name of its block class.
// The search finds an entry by the id of the block or the plugin's class as
// well.
interface PickerEntry {
  readonly panel: Panel;
  readonly id: string;
  readonly name: string;
  readonly type?: string;
}

interface PickerEntries {
  readonly blocks: readonly PickerEntry[];
  readonly plugins: readonly PickerEntry[];
}

// The entries of the picker on a board, where the dock draws the panels of the
// plugins given: those that the active view does not show, none where there is
// no active view.
function pickerEntries(board: Board, registry: BlockRegistry, drawn: ReadonlyMap<PluginClassId, string>): PickerEntries {
  const view = activeView(board);
  if (view === undefined) return { blocks: [], plugins: [] };
  const blocks = Object.entries(board.blocks).flatMap(([block, held]): PickerEntry[] => {
    if (places(view.grid, { block })) return [];
    const type = registry[held.registry_id]?.name;
    return [{ panel: { block }, id: block, name: held.name, ...(type === undefined ? {} : { type }) }];
  });
  const plugins = [...drawn].flatMap(([plugin, name]): PickerEntry[] =>
    places(view.grid, { plugin }) ? [] : [{ panel: { plugin }, id: plugin, name }],
  );
  return { blocks, plugins };
}

// The entries that a search finds, every entry for an empty search.
function searched(entries: PickerEntries, query: string): PickerEntries {
  const needle = query.trim().toLowerCase();
  if (needle === "") return entries;
  const finds = (entry: PickerEntry) =>
    [entry.name, entry.type ?? "", entry.id].some((text) => text.toLowerCase().includes(needle));
  return { blocks: entries.blocks.filter(finds), plugins: entries.plugins.filter(finds) };
}

// The open picker.
interface Picking {
  // The button that opened it, with an id of the button's own.
  readonly id: string;
  readonly button: HTMLElement;
  // The panel near which a pick goes, the one that the group of a tab group's
  // "+" shows, or undefined for the default place.
  readonly near: Panel | undefined;
  // Whether the keyboard opened it.
  readonly keyboard: boolean;
}

interface PickerState {
  readonly picking: Picking | undefined;
  readonly setPicking: (picking: Picking | undefined) => void;
  // Moves the focus into the open picker.
  readonly enter: () => void;
}

const PickerContext = createContext<PickerState | null>(null);

function usePicker(): PickerState {
  const value = useContext(PickerContext);
  if (value === null) throw new Error("A button of the panel picker is drawn outside the picker.");
  return value;
}

// The anchor name that the button which opened the picker takes, by which CSS
// places the picker below it.
const ANCHOR = "--blockr-dock-picker";
const SEARCH = ".blockr-dock-picker-search";
const ITEM = '[role="menuitem"]';

// Moves the focus into the picker, onto its search field, which it has unless
// it has nothing to list.
function focusInto(el: HTMLElement | null): void {
  el?.querySelector<HTMLElement>(SEARCH)?.focus();
}

// Places the picker below the button, or above it where the window has no
// room below, as CSS places it in a browser with anchor positioning.
function place(el: HTMLElement, button: HTMLElement): void {
  const win = el.ownerDocument.defaultView;
  if (win === null || win.CSS.supports("position-area", "block-end")) return;
  const gap = 4;
  const at = button.getBoundingClientRect();
  const { width, height } = el.getBoundingClientRect();
  const above = at.top - gap - height;
  const top = at.bottom + gap + height > win.innerHeight && above >= 0 ? above : at.bottom + gap;
  const left = Math.max(0, Math.min(at.left, win.innerWidth - width));
  el.style.left = `${String(left)}px`;
  el.style.top = `${String(top)}px`;
}

// The picker, which the buttons among its children open.
export function PanelPicker({ children }: { readonly children: ReactNode }): ReactElement {
  const [picking, setPicking] = useState<Picking | undefined>(undefined);
  const ref = useRef<HTMLDivElement>(null);
  const enter = useCallback(() => {
    focusInto(ref.current);
  }, []);
  const state = useMemo(() => ({ picking, setPicking, enter }), [picking, enter]);

  useLayoutEffect(() => {
    const el = ref.current;
    if (el === null || picking === undefined) return undefined;
    const { button, keyboard } = picking;
    // Safari does not focus a button that the user clicks, so the dock does,
    // and the browser gives the focus back to the button as the picker
    // closes.
    if (!keyboard) button.focus({ focusVisible: false });
    button.style.setProperty("anchor-name", ANCHOR);
    el.showPopover({ source: button });
    place(el, button);
    focusInto(el);
    return () => {
      button.style.removeProperty("anchor-name");
      el.hidePopover();
    };
  }, [picking]);

  return (
    <PickerContext value={state}>
      {children}
      <div
        ref={ref}
        popover="auto"
        className="blockr-dock-picker"
        onToggle={(event) => {
          if (event.newState === "closed") setPicking(undefined);
        }}
        onBlur={(event) => {
          const next = event.relatedTarget;
          if (next instanceof Node && !event.currentTarget.contains(next)) setPicking(undefined);
        }}
      >
        {picking === undefined ? null : <PickerMenu near={picking.near} />}
      </div>
    </PickerContext>
  );
}

// The props of a button that opens the picker.
// The function `near` gives the panel near which a pick goes, or undefined for
// the default place, as the user opens the picker.
export function usePickerButton(near: () => Panel | undefined) {
  const { picking, setPicking, enter } = usePicker();
  const id = useId();
  const open = picking?.id === id;
  // Whether the picker was open for this button as the pointer went down on
  // it.
  // The browser may close the picker then, as the pointer goes down outside
  // it, so the click that follows closes it rather than opens it again.
  const pressedOpen = useRef(false);
  const show = (button: HTMLElement, keyboard: boolean) => {
    setPicking({ id, button, near: near(), keyboard });
  };
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
      if (wasOpen) setPicking(undefined);
      else show(event.currentTarget, keyboard);
    },
    onKeyDown: (event: KeyboardEvent<HTMLElement>) => {
      if (event.key !== "ArrowDown") return;
      event.preventDefault();
      if (open) enter();
      else show(event.currentTarget, true);
    },
  } as const;
}

// What the open picker shows, which follows the board.
function PickerMenu({ near }: { readonly near: Panel | undefined }): ReactElement {
  const { core, drawn } = useDock();
  const { setPicking } = usePicker();
  const board = useBoard(core);
  const [query, setQuery] = useState("");
  const caption = useId();
  const heading = useId();
  const menu = useRef<HTMLDivElement>(null);
  const search = useRef<HTMLInputElement>(null);

  const listed = useMemo(() => pickerEntries(board, core.blockRegistry(), drawn), [board, core, drawn]);
  const shown = useMemo(() => searched(listed, query), [listed, query]);
  const count = listed.blocks.length + listed.plugins.length;

  const pick = (panel: Panel) => {
    setPicking(undefined);
    const view = stateOf(core.store.board())?.active ?? null;
    if (view === null) return;
    const args: Json = near === undefined ? { view, panel } : { view, panel, near, side: "within" };
    void core.commands.run("blockr.dock-add_panel", args).catch(() => {
      // Core tells the user why the board refused the command.
    });
  };

  const captioned = (
    <p id={caption} className="blockr-dock-picker-caption">
      Show in this view
    </p>
  );
  if (count === 0) {
    return (
      <>
        {captioned}
        <p className="blockr-dock-picker-note">{nothingLeft(board, drawn)}</p>
      </>
    );
  }

  const first = shown.blocks[0] ?? shown.plugins[0];
  const items = () => [...(menu.current?.querySelectorAll<HTMLElement>(ITEM) ?? [])];

  const onSearchKey = (event: KeyboardEvent<HTMLInputElement>) => {
    if (event.nativeEvent.isComposing) return;
    if (event.key === "ArrowDown") {
      event.preventDefault();
      items()[0]?.focus();
    } else if (event.key === "Enter" && first !== undefined) {
      event.preventDefault();
      pick(first.panel);
    }
  };

  const onMenuKey = (event: KeyboardEvent<HTMLDivElement>) => {
    const all = items();
    const at = all.findIndex((item) => item === event.target);
    if (at < 0) return;
    const last = all.length - 1;
    let next: HTMLElement | null | undefined;
    if (event.key === "ArrowDown") next = all[at === last ? 0 : at + 1];
    else if (event.key === "ArrowUp") next = at === 0 ? search.current : all[at - 1];
    else if (event.key === "Home") next = all[0];
    else if (event.key === "End") next = all[last];
    else return;
    event.preventDefault();
    next?.focus();
  };

  return (
    <>
      {captioned}
      <input
        ref={search}
        type="search"
        className="blockr-dock-picker-search"
        aria-label="Search"
        placeholder="Search"
        autoComplete="off"
        spellCheck={false}
        value={query}
        onChange={(event) => {
          setQuery(event.target.value);
        }}
        onKeyDown={onSearchKey}
      />
      <div ref={menu} role="menu" aria-labelledby={caption} className="blockr-dock-picker-menu" onKeyDown={onMenuKey}>
        {shown.blocks.map((entry) => (
          <Item key={entry.id} entry={entry} pick={pick} />
        ))}
        {shown.plugins.length === 0 ? null : (
          <div role="group" aria-labelledby={heading}>
            <div id={heading} aria-hidden="true" className="blockr-dock-picker-heading">
              Plugins
            </div>
            {shown.plugins.map((entry) => (
              <Item key={entry.id} entry={entry} pick={pick} />
            ))}
          </div>
        )}
      </div>
      {first === undefined ? <p className="blockr-dock-picker-note">No panel matches.</p> : null}
    </>
  );
}

function Item({ entry, pick }: { readonly entry: PickerEntry; readonly pick: (panel: Panel) => void }): ReactElement {
  return (
    <button
      type="button"
      role="menuitem"
      tabIndex={-1}
      className="blockr-dock-picker-item"
      onClick={() => {
        pick(entry.panel);
      }}
    >
      <span className="blockr-dock-picker-name">{entry.name}</span>
      {entry.type === undefined ? null : <span className="blockr-dock-picker-type">{entry.type}</span>}
    </button>
  );
}

// Why the picker has nothing to list, as today's suggest_panels_to_add() says
// it, at 655e323.
function nothingLeft(board: Board, drawn: ReadonlyMap<PluginClassId, string>): string {
  return Object.keys(board.blocks).length === 0 && drawn.size === 0
    ? "The board has no blocks yet. Add a new block to get started."
    : "All blocks and plugins are already in this view. Add a new block to the board first.";
}
