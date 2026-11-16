// The dock's options sidebar, which the options button on the bar opens
// (./navbar.tsx), and which shows each board option that the board holds under
// its category
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
// The sidebar lists the categories as an accordion of `details` elements, in
// alphabetical order, of which the user may expand several at once:
// 1. A click on a category expands it in place. Core mounts the UI of each of
//    its board options with mountOption(), in the alphabetical order of their
//    labels.
// 2. Another click collapses it. Core ends those mounts once the category has
//    folded shut.
// The categories that the user expanded stay expanded as the sidebar closes
// (./sidebar.ts).
// The order in which the board holds its board options is no guide, since
// core puts each default on the board once the default resolves.
// The pages put each board option on the page of its category, which today's
// sidebar opens one at a time. The accordion shows several categories at once,
// with no page to step into and back out of.
//
// As the sidebar opens, its first category takes the focus. Once the sidebar
// closes, the focus goes back to where it was as the sidebar opened, such as
// the options button, unless the user has moved it out of the sidebar.
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
import { useCallback, useEffect, useLayoutEffect, useRef, useState, useSyncExternalStore } from "react";
import type { ReactElement } from "react";
import { useBoard, useDock } from "./context.ts";
import { Chevron, CloseIcon, PinIcon } from "./icons.tsx";
import { Mounted } from "./mounted.tsx";
import type { Sidebar } from "./sidebar.ts";

// The sidebar's title, as today's.
const TITLE = "Board options";

// What Tab moves the focus to, as today's sidebar finds it, and the summary
// of a category.
const FOCUSABLE = [
  "a[href]",
  "button:not([disabled])",
  'input:not([disabled]):not([type="hidden"])',
  "select:not([disabled])",
  "textarea:not([disabled])",
  '[tabindex]:not([tabindex="-1"])',
  "details > summary:first-of-type",
].join(", ");

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
  const { open, pinned, expanded } = useSyncExternalStore(sidebar.subscribe, sidebar.state);
  const grouped = categories(board, core.options.registered());
  const ref = useRef<HTMLDivElement>(null);
  const title = `${sidebar.id}-title`;

  useFocus(ref, open);
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
        <h2 id={title} className="blockr-dock-sidebar-title">
          {TITLE}
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
        {grouped.size === 0 ? (
          <p className="blockr-dock-sidebar-note">The board holds no board options.</p>
        ) : (
          <div className="blockr-dock-sidebar-categories">
            {[...grouped].map(([category, ids]) => (
              <Category key={category} category={category} ids={ids} expanded={expanded.includes(category)} sidebar={sidebar} />
            ))}
          </div>
        )}
      </div>
    </div>
  );
}

interface CategoryProps {
  readonly category: string;
  // The ids of its board options, in their order.
  readonly ids: readonly OptionId[];
  readonly expanded: boolean;
  readonly sidebar: Sidebar;
}

// A category, whose content holds the UI of each of its board options while
// the category is expanded, and while it folds shut.
function Category({ category, ids, expanded, sidebar }: CategoryProps): ReactElement {
  // Whether the category folds shut, while which its content holds the UIs
  // still.
  const [folding, setFolding] = useState(false);
  // How many times the category has collapsed, so that a fold that the user
  // cut short by expanding and collapsing it again ends with the last one.
  const folds = useRef(0);

  return (
    <details
      className="blockr-dock-sidebar-category"
      open={expanded}
      onToggle={(event) => {
        const details = event.currentTarget;
        sidebar.expand(category, details.open);
        if (details.open) return;
        folds.current += 1;
        const fold = folds.current;
        setFolding(true);
        void folded(details).then(() => {
          if (folds.current === fold) setFolding(false);
        });
      }}
    >
      <summary className="blockr-dock-sidebar-summary">
        <Chevron />
        <span className="blockr-dock-sidebar-summary-name">{category}</span>
      </summary>
      {expanded || folding ? (
        <div className="blockr-dock-sidebar-options">
          {ids.map((id) => (
            <OptionFrame key={id} option={id} />
          ))}
        </div>
      ) : null}
    </details>
  );
}

// Resolves once the category's content has folded shut, which takes as long
// as the longest transition that the content's style gives, or at once where
// it gives none, as under reduced motion.
// The browser lists no animation of the content, its ::details-content, in
// getAnimations(), and fires no transition event for it, so the style is all
// that says how long the fold takes.
async function folded(el: HTMLElement): Promise<void> {
  const style = el.ownerDocument.defaultView?.getComputedStyle(el, "::details-content");
  const ms = style === undefined ? 0 : longest(style.transitionDuration, style.transitionDelay);
  await new Promise((resolve) => {
    setTimeout(resolve, ms);
  });
}

// The longest transition, in milliseconds, of those whose durations and delays
// a computed style lists, such as "0.22s, 0.22s" and "0s".
// A list shorter than the other repeats, as CSS repeats it.
function longest(durations: string, delays: string): number {
  const ms = (list: string): number[] =>
    list.split(",").map((item) => {
      const time = Number.parseFloat(item);
      if (!Number.isFinite(time)) return 0;
      return item.trim().endsWith("ms") ? time : time * 1000;
    });
  const duration = ms(durations);
  const delay = ms(delays);
  return Math.max(0, ...duration.map((d, i) => d + (delay[i % delay.length] ?? 0)));
}

// The element in which core mounts the UI of a board option, or the renderer
// that the app names for it (./mounted.tsx).
function OptionFrame({ option }: { readonly option: OptionId }): ReactElement {
  const { core } = useDock();
  const mount = useCallback((el: HTMLElement) => core.mountOption(el, option), [core, option]);
  return <Mounted mount={mount} className="blockr-dock-sidebar-option" data={{ "data-option": option }} />;
}

// Gives the sidebar's first category the focus as the sidebar opens, and the
// focus back to where it was as the sidebar closes, as the comment at the top
// of this file says.
function useFocus(ref: { readonly current: HTMLElement | null }, open: boolean): void {
  // Where the focus was as the sidebar opened.
  const before = useRef<Element | null>(null);

  useLayoutEffect(() => {
    const root = ref.current;
    if (root === null) return;
    const doc = root.ownerDocument;
    if (open) {
      before.current = doc.activeElement;
      const first = root.querySelector(".blockr-dock-sidebar-body")?.querySelector<HTMLElement>(FOCUSABLE);
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
      if (event.key !== "Escape" || holdsOverlay(root) || sidebar.state().pinned) return;
      sidebar.set(false);
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
