// The dock's options sidebar, which the options button on the bar opens
// (./navbar.tsx), and which shows each board option that the board holds on
// the page of its category
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.4#settings).
// The board holds every board option that something on the page requires, its
// default included where nobody set one, so the sidebar shows those of the
// dock plugin's class, of every other plugin class that runs, of the renderers
// whose outputs show and of the blocks' UIs.
// It follows the board, so a board option that comes onto the board later, as
// n_rows does once a table draws, shows at once.
// It skips a board option that no loaded package defines, since core mounts no
// UI for one.
// It shows them on a locked board too, since the board lock leaves board
// options open
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.11#board-options-on-a-locked-board).
//
// The sidebar opens on the list of categories, whose first row takes the
// focus:
// 1. A click on a row shows the category's page, titled with the category and
//    with a back arrow. Core mounts the UI of each of the page's board options
//    with mountOption(). The back arrow takes the focus, and the page's first
//    field takes it once core has drawn one.
// 2. The back arrow or Escape returns to the list, and the row of the page
//    takes the focus. Core ends the page's mounts.
// 3. On the list, Escape closes the sidebar. The close button closes it from
//    either.
// Once the sidebar closes, the focus goes back to where it was as the sidebar
// opened, such as the options button, unless the user has moved it out of the
// sidebar.
// The list shows the categories in alphabetical order, and each page shows its
// board options in the alphabetical order of their labels. The order in which
// the board holds its board options is no guide, since core puts each default
// on the board once the default resolves.
//
// The sidebar carries over today's, at 655e323 (R/sidebar-server.R, with
// inst/assets/js/sidebar-server.js and options-sidebar.js):
// - It lies at the window's right edge, 420 pixels wide, from the top of the
//   window to its bottom, with a header as high as the bar.
// - Unpinned, it lies over the bar and the dock. Escape and a press outside it
//   close it. A press on the options button, which toggles it, does not count
//   as outside, and neither does a press on a toast, which lies over it.
// - The pin button pins it. Pinned, it is part of the page: the bar and the
//   dock make room for it, and only its close button closes it, as blockr.ui's
//   design system has a side panel
//   (https://bristolmyerssquibb.github.io/blockr.ui/articles/design-system.html#floating).
// - Tab and Shift+Tab keep the focus inside it, pinned or not.
// A board option's UI may open a popover or a modal dialog in the sidebar.
// Escape then closes that, and the sidebar leaves the key alone.
// Today's sidebar ends its list with the code export. That row goes, since
// code export becomes blockr.code's plugin. The sidebar shows no field for the
// board's name either, since the board's name is on the bar.

import type { Board, Option, OptionId } from "@blockr/core";
import { useCallback, useEffect, useLayoutEffect, useRef, useSyncExternalStore } from "react";
import type { ReactElement } from "react";
import { useBoard, useDock } from "./context.ts";
import { Chevron, CloseIcon, PinIcon } from "./icons.tsx";
import { Mounted } from "./mounted.tsx";
import type { Sidebar } from "./sidebar.ts";

// The title of the list of categories, as today's.
const TITLE = "Board options";

// What Tab moves the focus to, as today's sidebar finds it.
const FOCUSABLE = [
  "a[href]",
  "button:not([disabled])",
  'input:not([disabled]):not([type="hidden"])',
  "select:not([disabled])",
  "textarea:not([disabled])",
  '[tabindex]:not([tabindex="-1"])',
].join(", ");

// The fields of a page, the first of which takes the focus as the page opens,
// as today's.
const FIELDS = "input, select, textarea, button";

// What closes before the sidebar on Escape: an open popover and a modal
// dialog.
const OVERLAYS = [":popover-open", "dialog:modal"];

// The categories of the board options that the board holds, in alphabetical
// order, each with the ids of its board options, in the alphabetical order of
// their labels, or of their ids where two labels are the same.
// A board option that no loaded package defines is left out.
function categories(board: Board, registered: Readonly<Record<OptionId, Option>>): ReadonlyMap<string, readonly OptionId[]> {
  const held = Object.keys(board.options).flatMap((id) => {
    const def = Object.hasOwn(registered, id) ? registered[id] : undefined;
    return def === undefined ? [] : [{ id, label: def.label, category: def.category }];
  });
  held.sort((a, b) => a.category.localeCompare(b.category) || a.label.localeCompare(b.label) || a.id.localeCompare(b.id));
  const grouped = new Map<string, OptionId[]>();
  for (const { id, category } of held) grouped.set(category, [...(grouped.get(category) ?? []), id]);
  return grouped;
}

export function OptionsSidebar({ sidebar }: { readonly sidebar: Sidebar }): ReactElement {
  const { core } = useDock();
  const board = useBoard(core);
  const { open, pinned, page } = useSyncExternalStore(sidebar.subscribe, sidebar.state);
  const grouped = categories(board, core.options.registered());
  const ids = page === undefined ? undefined : grouped.get(page);
  const ref = useRef<HTMLDivElement>(null);
  const title = `${sidebar.id}-title`;

  // A page whose category the board no longer holds gives way to the list.
  const gone = page !== undefined && ids === undefined;
  useEffect(() => {
    if (gone) sidebar.show(undefined);
  }, [sidebar, gone]);

  useFocus(ref, open, ids === undefined ? undefined : page);
  useKeys(ref, sidebar);
  usePressOutside(ref, sidebar, open && !pinned);

  return (
    <div
      ref={ref}
      id={sidebar.id}
      className="blockr-dock-sidebar"
      role="dialog"
      aria-labelledby={title}
      tabIndex={-1}
      inert={!open}
      data-open={open ? "" : undefined}
    >
      <header className="blockr-dock-sidebar-header">
        {ids === undefined ? null : (
          <button
            type="button"
            className="blockr-dock-sidebar-button blockr-dock-sidebar-back"
            aria-label="Back"
            title="Back"
            onClick={() => {
              sidebar.show(undefined);
            }}
          >
            <Chevron point="left" />
          </button>
        )}
        <h2 id={title} className="blockr-dock-sidebar-title">
          {ids === undefined ? TITLE : page}
        </h2>
        <button
          type="button"
          className="blockr-dock-sidebar-button blockr-dock-sidebar-pin"
          aria-label="Pin sidebar"
          aria-pressed={pinned}
          title="Pin"
          onClick={() => {
            sidebar.pin(!pinned);
          }}
        >
          <PinIcon />
        </button>
        <button
          type="button"
          className="blockr-dock-sidebar-button"
          aria-label="Close sidebar"
          title="Close"
          onClick={() => {
            sidebar.set(false);
          }}
        >
          <CloseIcon />
        </button>
      </header>
      <div className="blockr-dock-sidebar-body">
        {ids === undefined ? <CategoryList grouped={grouped} sidebar={sidebar} /> : <OptionsPage ids={ids} />}
      </div>
    </div>
  );
}

// The list of categories, a row for each, which opens the category's page.
function CategoryList({
  grouped,
  sidebar,
}: {
  readonly grouped: ReadonlyMap<string, readonly OptionId[]>;
  readonly sidebar: Sidebar;
}): ReactElement {
  if (grouped.size === 0) return <p className="blockr-dock-sidebar-note">The board holds no board options.</p>;
  return (
    <div className="blockr-dock-sidebar-list">
      {[...grouped.keys()].map((category) => (
        <button
          key={category}
          type="button"
          className="blockr-dock-sidebar-row"
          data-category={category}
          onClick={() => {
            sidebar.show(category);
          }}
        >
          <span className="blockr-dock-sidebar-row-name">{category}</span>
          <Chevron point="right" />
        </button>
      ))}
    </div>
  );
}

// The page of a category, with the UI of each of its board options, one under
// the other.
function OptionsPage({ ids }: { readonly ids: readonly OptionId[] }): ReactElement {
  return (
    <div className="blockr-dock-sidebar-page">
      {ids.map((id) => (
        <OptionFrame key={id} option={id} />
      ))}
    </div>
  );
}

// The element in which core mounts the UI of a board option, or the renderer
// that the app names for it (./mounted.tsx).
function OptionFrame({ option }: { readonly option: OptionId }): ReactElement {
  const { core } = useDock();
  const mount = useCallback((el: HTMLElement) => core.mountOption(el, option), [core, option]);
  return <Mounted mount={mount} className="blockr-dock-sidebar-option" data={{ "data-option": option }} />;
}

// Moves the focus as the sidebar opens and closes, and as its page changes,
// as the sidebar's comment at the top of this file says.
function useFocus(ref: { readonly current: HTMLElement | null }, open: boolean, page: string | undefined): void {
  // Where the focus was as the sidebar opened.
  const before = useRef<Element | null>(null);
  // The page that the sidebar showed when it last drew.
  const shown = useRef<string | undefined>(undefined);

  useLayoutEffect(() => {
    const root = ref.current;
    if (root === null) return;
    const doc = root.ownerDocument;
    if (open) {
      before.current = doc.activeElement;
      const first = root.querySelector<HTMLElement>(`.blockr-dock-sidebar-body :is(${FOCUSABLE})`);
      (first ?? root).focus();
      return;
    }
    const back = before.current;
    before.current = null;
    // Where the sidebar held the focus, it is in the sidebar still, or
    // nowhere, as where the sidebar has turned inert.
    const active = doc.activeElement;
    const held = active === null || active === doc.body || root.contains(active);
    if (held && back instanceof HTMLElement && back.isConnected) back.focus();
  }, [ref, open]);

  useLayoutEffect(() => {
    const root = ref.current;
    const was = shown.current;
    shown.current = page;
    if (root === null || !open || was === page) return undefined;
    if (page === undefined) {
      const row = [...root.querySelectorAll<HTMLElement>(".blockr-dock-sidebar-row")].find((r) => r.dataset.category === was);
      (row ?? root).focus();
      return undefined;
    }
    const back = root.querySelector<HTMLElement>(".blockr-dock-sidebar-back");
    const fields = root.querySelector<HTMLElement>(".blockr-dock-sidebar-page");
    if (back === null || fields === null) return undefined;
    back.focus();
    return focusFirstField(fields, back);
  }, [ref, open, page]);
}

// Gives the first field of the page the focus once core has drawn one, where
// the focus is still on `from`.
// The returned function stops waiting for a field.
function focusFirstField(fields: HTMLElement, from: HTMLElement): () => void {
  const doc = fields.ownerDocument;
  const attempt = (): boolean => {
    const field = fields.querySelector<HTMLElement>(FIELDS);
    if (field === null) return false;
    if (doc.activeElement === from) field.focus();
    return true;
  };
  if (attempt()) return () => undefined;
  const Observer = doc.defaultView?.MutationObserver;
  if (Observer === undefined) return () => undefined;
  const observer = new Observer(() => {
    if (attempt()) observer.disconnect();
  });
  observer.observe(fields, { childList: true, subtree: true });
  return () => {
    observer.disconnect();
  };
}

// Hears Escape, and Tab and Shift+Tab, in the sidebar.
// It listens on the sidebar's element itself, rather than through React,
// since the UIs of board options draw in React roots of their own.
function useKeys(ref: { readonly current: HTMLElement | null }, sidebar: Sidebar): void {
  useEffect(() => {
    const root = ref.current;
    if (root === null) return undefined;
    const onKey = (event: KeyboardEvent): void => {
      if (event.defaultPrevented) return;
      if (event.key === "Tab") {
        keepFocus(root, event);
        return;
      }
      if (event.key !== "Escape" || holdsOverlay(root)) return;
      const { page, pinned } = sidebar.state();
      if (page !== undefined) sidebar.show(undefined);
      else if (!pinned) sidebar.set(false);
      else return;
      event.preventDefault();
    };
    root.addEventListener("keydown", onKey);
    return () => {
      root.removeEventListener("keydown", onKey);
    };
  }, [ref, sidebar]);
}

// Keeps the focus in the sidebar where Tab or Shift+Tab would take it out, by
// moving it to the sidebar's other end.
// A control that a board option's UI hides takes no part, since Tab skips it.
function keepFocus(root: HTMLElement, event: KeyboardEvent): void {
  const focusable = [...root.querySelectorAll<HTMLElement>(FOCUSABLE)].filter(
    (el) => el.checkVisibility({ visibilityProperty: true }) && el.closest("[inert]") === null,
  );
  const first = focusable[0];
  const last = focusable.at(-1);
  const active = root.ownerDocument.activeElement;
  if (first === undefined || last === undefined) {
    event.preventDefault();
    root.focus();
  } else if (event.shiftKey && (active === first || active === root)) {
    event.preventDefault();
    last.focus();
  } else if (!event.shiftKey && (active === last || active === root)) {
    event.preventDefault();
    first.focus();
  }
}

// Whether the sidebar holds an open popover or a modal dialog.
// A browser that does not know one of the selectors opens no such overlay.
function holdsOverlay(root: HTMLElement): boolean {
  return OVERLAYS.some((selector) => {
    try {
      return root.querySelector(selector) !== null;
    } catch {
      return false;
    }
  });
}

// Closes the sidebar on a press outside it, while `armed`.
// A press on the options button, which names the sidebar in aria-controls, or
// on a toast, does not count as outside.
function usePressOutside(ref: { readonly current: HTMLElement | null }, sidebar: Sidebar, armed: boolean): void {
  useEffect(() => {
    const root = ref.current;
    if (root === null || !armed) return undefined;
    const doc = root.ownerDocument;
    const onPress = (event: PointerEvent): void => {
      const target = event.target;
      if (!(target instanceof Element) || root.contains(target)) return;
      if (target.closest(`[aria-controls="${sidebar.id}"], .blockr-dock-toasts`) !== null) return;
      sidebar.set(false);
    };
    doc.addEventListener("pointerdown", onPress, true);
    return () => {
      doc.removeEventListener("pointerdown", onPress, true);
    };
  }, [ref, sidebar, armed]);
}
