// The bar across the top of the dock, which draws the navbar items in one row,
// in their order (./navbar-items.ts)
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.5#what-the-bar-holds).
// An item with nothing to show draws nothing, so it takes no room.
// The bar follows blockr.ui's design system: 48 pixels high on the surface, a
// rule under it, every control 30 pixels high, and tokens only (./index.css).
//
// The dock's own parts:
// - The logo, which turns while R evaluates a block that the dock demands
//   (./busy.ts). Its tooltip says "Computing" while it turns.
// - The board's name. A click turns it into a field, which runs core's
//   command blockr.core-rename_board with the name that the user gives. Enter
//   and leaving the field rename the board, unless the name is empty or
//   unchanged, and Escape gives up. Under the board lock, the name only shows.
// - The storage plugin's controls, those that no storage item places apart
//   (./storage.ts).
// - Undo and Redo, two quiet buttons with their labels, as the storage
//   plugin's buttons beside them have, which run the store's undo() and
//   redo(), each greyed out while the store has nothing to take back or to
//   apply again. Their keys are bound on the document (./shortcuts.ts), and
//   each tooltip names its shortcut. Both stay under the board lock, since
//   what that lock leaves open still takes undo steps.
// - The spacer, which takes the free space, so that what follows it sits at
//   the right end of the bar.
// - The views menu, with its line of tabs while the board option view_tabs is
//   on (./views-menu.tsx). The bar grows by the line, under its row.
// - The read-only pill, which shows while the board lock is on.
// - The options button, a quiet button with its label as well, which opens and
//   closes the options sidebar (./options-sidebar.tsx).
// The storage plugin's buttons, Undo, Redo and the options button are the
// Button of core's React SDK, in the bar's look (./index.css).
// Those with a tooltip keep it as the browser's (./attributes.tsx).
// While the options sidebar is pinned, the bar makes room for it at its right
// end (./index.css).
// Core tells the user why the store refused a rename, an undo or a redo,
// through its notification board_edit_failed, which the dock shows as a toast.
//
// Besides the dock's own parts:
// - A storage item draws the storage plugin's control of its id, in its own
//   place, or nothing where the plugin gives no control of that id.
// - A link opens in a new tab, and a menu holds links and further menus
//   (./menu.tsx).
// - A module item loads the module that a package offers by its id, with
//   core's module(). The module meets the contract of a UI module, with
//   CoreApi in place of a host: it declares the major of core's JS API that it
//   was built against, as `export const api = 1`, and its default export has a
//   render(), which the dock calls with the item's element, CoreApi and a
//   signal. The dock aborts the signal and runs the cleanup that render()
//   returns as it takes the bar down. Where the module fails to load, declares
//   another major, or throws in render(), a warning takes its place, whose
//   tooltip names the module, and the rest of the bar works.
//
// On a narrow bar, the board's name and the storage plugin's controls give way
// first, by cutting their labels short, while the other items keep their
// width, so that the bar stays on one line. A label cut short shows in full as
// the tooltip, where the control has no tooltip of its own.

import type { CoreApi, StorageControl } from "@blockr/core";
import { Button, useBoard } from "@blockr/core/react";
import { useCallback, useEffect, useReducer, useRef, useState, useSyncExternalStore } from "react";
import type { CSSProperties, ReactElement } from "react";
import { isObject } from "../../state/index.ts";
import { API } from "./api.ts";
import { withAttributes } from "./attributes.tsx";
import type { Busy } from "./busy.ts";
import { useDock } from "./context.ts";
import { asError, errorMessage } from "./errors.ts";
import { LockIcon, WarningIcon } from "./icons.tsx";
import { MenuButton } from "./menu.tsx";
import type { MenuEntry } from "./menu.tsx";
import { NameField } from "./name-field.tsx";
import { holdsPart } from "./navbar-items.ts";
import type { LinkItem, MenuItem, ModuleItem, NavbarItem, PartId } from "./navbar-items.ts";
import type { OpenFolders } from "./open-folders.ts";
import { ariaShortcuts, shortcutLabel } from "./shortcuts.ts";
import type { Sidebar } from "./sidebar.ts";
import { drawnControls, runButton } from "./storage.ts";
import type { Drawn } from "./storage.ts";
import { cutTip, setTip } from "./tips.ts";
import { useViewTabs, Views } from "./views-menu.tsx";


export interface NavbarProps {
  readonly items: readonly NavbarItem[];
  readonly busy: Busy;
  readonly sidebar: Sidebar;
  // Whether the page runs on a Mac, where the tooltips name Mod as Command.
  readonly mac: boolean;
  readonly openFolders: OpenFolders;
}

export function Navbar({ items, busy, sidebar, mac, openFolders }: NavbarProps): ReactElement {
  const { core } = useDock();
  const storage = useStorage(core);
  const { pinned } = useSyncExternalStore(sidebar.subscribe, sidebar.state);
  // The ids of the controls that storage items place apart.
  const apart = new Set(items.flatMap((item) => (item.kind === "storage" ? [item.id] : [])));
  // Whether the views menu draws its line of tabs, under the bar's row.
  const tabs = useViewTabs(core) && holdsPart(items, "views");
  return (
    <header className="blockr-dock-bar" data-tabs={tabs ? "" : undefined} data-pushed={pinned ? "" : undefined}>
      {items.map((item) => (
        <Item
          key={item.id}
          item={item}
          storage={storage}
          apart={apart}
          busy={busy}
          sidebar={sidebar}
          mac={mac}
          openFolders={openFolders}
        />
      ))}
    </header>
  );
}

interface ItemProps {
  readonly item: NavbarItem;
  readonly storage: Drawn | undefined;
  readonly apart: ReadonlySet<string>;
  readonly busy: Busy;
  readonly sidebar: Sidebar;
  readonly mac: boolean;
  readonly openFolders: OpenFolders;
}

function Item({ item, storage, apart, busy, sidebar, mac, openFolders }: ItemProps): ReactElement | null {
  switch (item.kind) {
    case "part":
      return (
        <Part id={item.id} storage={storage} apart={apart} busy={busy} sidebar={sidebar} mac={mac} openFolders={openFolders} />
      );
    case "storage": {
      const control = storage?.failed === undefined ? storage?.controls.find((c) => c.id === item.id) : undefined;
      return control === undefined ? null : <StorageControlView control={control} />;
    }
    case "link":
      return <Link item={item} />;
    case "menu":
      return <MenuButton label={item.label} entries={linkEntries(item.items)} className="blockr-dock-bar-button" />;
    case "module":
      return <ModuleSlot item={item} />;
  }
}

function Part({ id, storage, apart, busy, sidebar, mac, openFolders }: Omit<ItemProps, "item"> & { readonly id: PartId }): ReactElement | null {
  const { core } = useDock();
  switch (id) {
    case "logo":
      return <Logo busy={busy} />;
    case "name":
      return <BoardName />;
    case "storage":
      return <StorageControls storage={storage} apart={apart} />;
    case "undo_redo":
      return <UndoRedo mac={mac} />;
    case "spacer":
      return <span className="blockr-dock-bar-spacer" aria-hidden="true" />;
    case "views":
      return <Views openFolders={openFolders} />;
    case "read_only":
      return core.lock.board ? <ReadOnlyPill /> : null;
    case "options":
      return <OptionsButton sidebar={sidebar} />;
  }
}

// The squares of the logo, a 3 by 3 grid with two left out, which draw an R,
// as (column, row), in the order in which one stroke draws the R: up the
// stem, across the top, back to the middle, out the leg.
// The busy animation fades and fills them in that order, by each square's
// place, --i (./index.css).
const CELLS: readonly (readonly [number, number])[] = [
  [0, 2],
  [0, 1],
  [0, 0],
  [1, 0],
  [2, 0],
  [1, 1],
  [2, 2],
];

function Logo({ busy }: { readonly busy: Busy }): ReactElement {
  const turning = useSyncExternalStore(busy.subscribe, busy.get);
  return (
    <span className={turning ? "blockr-dock-logo is-computing" : "blockr-dock-logo"} title={turning ? "Computing" : undefined}>
      <svg className="blockr-dock-logo-mark" width="20" height="20" viewBox="0 0 224 224" aria-hidden="true" focusable="false">
        {CELLS.map(([x, y], i) => (
          <rect key={i} style={{ "--i": i } as CSSProperties} x={x * 80} y={y * 80} width="64" height="64" rx="7" />
        ))}
      </svg>
      <span className="blockr-dock-visually-hidden" role="status">
        {turning ? "Computing" : ""}
      </span>
    </span>
  );
}

function BoardName(): ReactElement {
  const { core } = useDock();
  const board = useBoard(core);
  const [renaming, setRenaming] = useState(false);
  const locked = core.lock.board;

  if (renaming) {
    return (
      <NameField
        name={board.name}
        label="Board name"
        className="blockr-dock-bar-field"
        onSubmit={(name) => {
          // Core tells the user why the store refused the name.
          core.commands.run("blockr.core-rename_board", { name }).catch(() => undefined);
        }}
        onDone={() => {
          setRenaming(false);
        }}
      />
    );
  }
  // The tooltip names the gesture, and leads with the name in full where the
  // bar cuts it short, as blockr.ui's design system has it.
  const tip = cutTip((cut) => {
    if (locked) return cut ? board.name : undefined;
    return cut ? `${board.name}\nClick to rename` : "Click to rename";
  });
  if (locked) {
    return (
      <span className="blockr-dock-bar-name blockr-dock-bar-shrinks" {...tip}>
        <span className="blockr-dock-bar-label">{board.name}</span>
      </span>
    );
  }
  return (
    <button
      type="button"
      className="blockr-dock-bar-name blockr-dock-bar-shrinks"
      aria-label={`Board name: ${board.name}. Rename`}
      onClick={() => {
        setRenaming(true);
      }}
      {...tip}
    >
      <span className="blockr-dock-bar-label">{board.name}</span>
    </button>
  );
}

// The storage controls that no storage item places apart, in the storage
// plugin's order, or a warning in their place where controls() threw.
function StorageControls({ storage, apart }: { readonly storage: Drawn | undefined; readonly apart: ReadonlySet<string> }): ReactElement | null {
  if (storage === undefined) return null;
  if (storage.failed !== undefined) {
    return <Warning message={`The storage controls of ${storage.package} failed: ${storage.failed}`} />;
  }
  const shown = storage.controls.filter((control) => !apart.has(control.id));
  if (shown.length === 0) return null;
  return (
    <div className="blockr-dock-bar-storage blockr-dock-bar-shrinks">
      {shown.map((control) => (
        <StorageControlView key={control.id} control={control} />
      ))}
    </div>
  );
}

// A storage control, in the look of the bar:
// - A button is a quiet button with its label, greyed out while it is
//   disabled, which runs the control within the click, since the file picker
//   of core's Open opens only within the user's click.
// - A menu is a quiet button with its label and the chevron, which opens a
//   menu that holds its items (./menu.tsx).
// - A status is text in the muted colour.
function StorageControlView({ control }: { readonly control: StorageControl }): ReactElement {
  const { core } = useDock();
  const tip = control.kind === "menu" || control.tooltip === undefined ? cutTip((cut) => (cut ? labelOf(control) : undefined)) : {};
  switch (control.kind) {
    case "button":
      return (
        <Button
          variant="quiet"
          className="blockr-dock-bar-button blockr-dock-bar-shrinks"
          isDisabled={control.disabled === true}
          render={withAttributes({ title: control.tooltip, "aria-keyshortcuts": ariaShortcuts(control.shortcut) })}
          onPress={() => {
            void runButton(core, control);
          }}
          {...tip}
        >
          <span className="blockr-dock-bar-label">{control.label}</span>
        </Button>
      );
    case "menu":
      return (
        <MenuButton
          label={control.label}
          entries={storageEntries(core, control.items)}
          className="blockr-dock-bar-button blockr-dock-bar-shrinks"
          onTip={(button) => {
            setTip(button, (cut) => (cut ? control.label : undefined));
          }}
        />
      );
    case "status":
      return (
        <span className="blockr-dock-bar-status blockr-dock-bar-shrinks" title={control.tooltip} {...tip}>
          <span className="blockr-dock-bar-label">{control.text}</span>
        </span>
      );
  }
}

function labelOf(control: StorageControl): string {
  return control.kind === "status" ? control.text : control.label;
}

// What a storage menu holds, as the menu lists it.
function storageEntries(core: CoreApi, items: readonly StorageControl[]): MenuEntry[] {
  return items.map((item): MenuEntry => {
    switch (item.kind) {
      case "button":
        return {
          kind: "button",
          id: item.id,
          label: item.label,
          ...(item.tooltip === undefined ? {} : { tooltip: item.tooltip }),
          ...(item.shortcut === undefined ? {} : { shortcut: item.shortcut }),
          ...(item.disabled === undefined ? {} : { disabled: item.disabled }),
          run: () => {
            void runButton(core, item);
          },
        };
      case "menu":
        return { kind: "menu", id: item.id, label: item.label, entries: storageEntries(core, item.items) };
      case "status":
        return { kind: "status", id: item.id, text: item.text, ...(item.tooltip === undefined ? {} : { tooltip: item.tooltip }) };
    }
  });
}

// What a menu of links holds, as the menu lists it.
function linkEntries(items: readonly (LinkItem | MenuItem)[]): MenuEntry[] {
  return items.map((item): MenuEntry =>
    item.kind === "link"
      ? { kind: "link", id: item.id, label: item.label, href: item.href }
      : { kind: "menu", id: item.id, label: item.label, entries: linkEntries(item.items) },
  );
}

function Link({ item }: { readonly item: LinkItem }): ReactElement {
  return (
    <a className="blockr-dock-bar-button" href={item.href} target="_blank" rel="noopener noreferrer">
      <span className="blockr-dock-bar-label">{item.label}</span>
    </a>
  );
}

function UndoRedo({ mac }: { readonly mac: boolean }): ReactElement {
  const { core } = useDock();
  const { canUndo, canRedo } = useHistory(core);
  const step = (back: boolean) => {
    try {
      if (back) core.store.undo();
      else core.store.redo();
    } catch {
      // Core has told the user why the store refused it.
    }
  };
  return (
    <div className="blockr-dock-bar-steps">
      <Button
        variant="quiet"
        className="blockr-dock-bar-button"
        isDisabled={!canUndo}
        render={withAttributes({ title: `Undo (${shortcutLabel("Mod+Z", mac)})`, "aria-keyshortcuts": "Control+Z Meta+Z" })}
        onPress={() => {
          step(true);
        }}
      >
        Undo
      </Button>
      <Button
        variant="quiet"
        className="blockr-dock-bar-button"
        isDisabled={!canRedo}
        render={withAttributes({
          title: `Redo (${shortcutLabel("Mod+Shift+Z", mac)})`,
          "aria-keyshortcuts": "Control+Shift+Z Meta+Shift+Z Control+Y",
        })}
        onPress={() => {
          step(false);
        }}
      >
        Redo
      </Button>
    </div>
  );
}

// The read-only pill, with today's label and tooltip, as
// read_only_navbar_ui() in R/dock-navbar.R has them, at 655e323.
function ReadOnlyPill(): ReactElement {
  return (
    <span className="blockr-dock-bar-pill" role="status" aria-label="Read-only mode" title="Editing is disabled by this deployment.">
      <LockIcon />
      <span>Read-only</span>
    </span>
  );
}

function OptionsButton({ sidebar }: { readonly sidebar: Sidebar }): ReactElement {
  const { open } = useSyncExternalStore(sidebar.subscribe, sidebar.state);
  return (
    <Button
      variant="quiet"
      className="blockr-dock-bar-button blockr-dock-bar-options"
      aria-expanded={open}
      aria-controls={sidebar.id}
      onPress={() => {
        sidebar.set(!open);
      }}
    >
      Options
    </Button>
  );
}

// The element of a module item, in which the module draws, or a warning in
// its place where the module cannot draw.
function ModuleSlot({ item }: { readonly item: ModuleItem }): ReactElement {
  const { core } = useDock();
  const ref = useRef<HTMLSpanElement>(null);
  const [failure, setFailure] = useState<string | undefined>(undefined);

  useEffect(() => {
    const el = ref.current;
    if (el === null) return undefined;
    const controller = new AbortController();
    const { signal } = controller;
    // Whether the bar has gone, read afresh after each await.
    const gone = () => signal.aborted;
    let cleanup: (() => void) | undefined;
    const draw = async () => {
      const mod = navbarModule(await core.module(item.module));
      if (gone()) return;
      const done = await mod.render({ el, core, signal });
      if (gone()) done?.();
      else cleanup = done;
    };
    draw().catch((error: unknown) => {
      if (gone()) return;
      const message = `The module ${item.module} of the navbar item "${item.id}" cannot draw: ${errorMessage(error)}`;
      core.log("blockr.dock").error(new Error(message, { cause: error }));
      setFailure(message);
    });
    return () => {
      controller.abort();
      try {
        cleanup?.();
      } catch (error) {
        core.log("blockr.dock").error(asError(error));
      }
    };
  }, [core, item.id, item.module]);

  if (failure !== undefined) return <Warning message={failure} />;
  return <span ref={ref} className="blockr-dock-bar-module" data-module={item.module} />;
}

// What a module on the bar exports as its default.
interface NavbarModule {
  render(args: {
    readonly el: HTMLElement;
    readonly core: CoreApi;
    readonly signal: AbortSignal;
  }): (() => void) | undefined | Promise<(() => void) | undefined>;
}

// The module that a module item names, from what it exports, or why it cannot
// draw: it declares no major of core's JS API, another major than the dock's,
// or no default export with a render() function.
function navbarModule(exports: unknown): NavbarModule {
  if (!isObject(exports) || typeof exports.api !== "number") {
    throw new Error("It declares no major of core's JS API, as `export const api = 1`.");
  }
  if (exports.api !== API) {
    throw new Error(`It was built against major ${String(exports.api)} of core's JS API, where the dock's is ${String(API)}.`);
  }
  const mod = exports.default;
  if (!isObject(mod) || typeof mod.render !== "function") throw new Error("It has no default export with a render() function.");
  return mod as unknown as NavbarModule;
}

// A warning in the place of an item that cannot draw, whose tooltip says why.
function Warning({ message }: { readonly message: string }): ReactElement {
  return (
    <span className="blockr-dock-bar-warning" role="img" aria-label={message} title={message}>
      <WarningIcon />
    </span>
  );
}

// The storage controls of the app's storage plugin, which the bar draws again
// whenever the storage plugin's subscribe() calls back and as the board
// changes.
// The dock logs each problem that it finds as it reads them once, rather than
// as each drawing finds it again.
function useStorage(core: CoreApi): Drawn | undefined {
  const storage = core.plugins.storage();
  const [, redraw] = useReducer((n: number) => n + 1, 0);
  useEffect(() => {
    if (storage?.subscribe === undefined) return undefined;
    try {
      return storage.subscribe(redraw);
    } catch (error) {
      core.log("blockr.dock").error(asError(error));
      return undefined;
    }
  }, [core, storage]);
  useBoard(core);
  const drawn = drawnControls(core);
  const problem = drawn?.problem;
  useEffect(() => {
    if (problem !== undefined) core.log("blockr.dock").error(problem);
  }, [core, problem]);
  return drawn;
}

// Whether the store's history holds a step to undo and one to redo, whoever
// took them.
function useHistory(core: CoreApi): { readonly canUndo: boolean; readonly canRedo: boolean } {
  const subscribe = useCallback(
    (changed: () => void) =>
      core.store.subscribe(() => {
        changed();
      }),
    [core],
  );
  const canUndo = useSyncExternalStore(subscribe, () => core.store.canUndo());
  const canRedo = useSyncExternalStore(subscribe, () => core.store.canRedo());
  return { canUndo, canRedo };
}
