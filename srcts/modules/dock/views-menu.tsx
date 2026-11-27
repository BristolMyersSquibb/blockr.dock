// The views menu, the dock's own part `views` of the bar (./navbar.tsx)
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.5#the-docks-own-parts).
// It is one of the dock's menus (./menu.tsx), which lists the views and
// folders of the navigation, each in the order that its `index` gives within
// its folder, with what an open folder holds below the folder's row
// (./views-list.ts).
// A folder is a fold of the menu, which opens and closes in place.
// Which folders are open stays on the screen, outside the board
// (./open-folders.ts).
// As the menu opens, the folders that hold the active view open, so that the
// menu shows the active view, whose row it sets as the current pick.
// A pick of a view runs the dock's command activate_view, under every lock.
// The bar shows the active view's name on the menu's button, and a short rule
// after it sets the menu apart from the parts after it.
//
// Without the board lock, the menu also adds, renames, moves and removes views
// and folders, through the dock's commands, in its manage mode, as today's
// views menu adds, renames, reorders and removes views (view_nav_ui() in
// R/dock-view.R and inst/assets/js/view-binding.js, at 655e323):
// - The row "Manage views", below the list, turns the menu to manage mode, a
//   popover of its own in the menu's place, and Done turns it back.
//   Closing it leaves manage mode too, so that the menu opens on the list.
// - In manage mode, each row has a grip, by which a drag moves the view or
//   folder (./views-list.ts), and a cross, which asks in the row before it
//   removes the view or folder. The last view has no cross, since the dock
//   refuses to remove it.
// - A click on a name turns it into a field, which renames the view or folder
//   as the board's name on the bar renames the board (./name-field.tsx).
// - "New view" adds a view last at the top and makes it the active view, and
//   "New folder" adds a folder last at the top. Each opens its new name in a
//   field.
// - The row "Show views as tabs" turns the board option view_tabs on and off,
//   through core's command set_option, as today's row does, and ends on a
//   check while it is on.
// Manage mode is a dialog, through whose controls Tab moves, and which Escape
// closes, but in a field or a row that asks, where Escape gives up the edit or
// the question alone.
//
// While the board option view_tabs is on, the views menu leaves the bar's row
// for a line of tabs under it, followed by the menu as a chevron alone, as
// today.
// The line holds a tab for each view and folder at the top of the navigation.
// A view's tab runs activate_view, and a folder's tab opens a menu of what the
// folder holds, in which a folder opens and closes as in the views menu, so
// that the line stays one row however deep the folders go.
// The line marks the active view's tab, or the tab of the folder that holds
// the active view.
// Where the tabs outgrow the line, it scrolls sideways, and it brings the
// marked tab into view as the active view changes.

import type { CommandId, CoreApi, Json } from "@blockr/core";
import { useBoard } from "@blockr/core/react";
import { useCallback, useLayoutEffect, useRef, useState, useSyncExternalStore } from "react";
import type { CSSProperties, FocusEvent, PointerEvent, ReactElement } from "react";
import { dockState } from "../../commands/index.ts";
import { foldersHolding, inFolder, sameEntry } from "../../state/index.ts";
import type { DockState, Entry, FolderId, ViewId } from "../../state/index.ts";
import { runEdit, useDock } from "./context.ts";
import { Chevron, GripIcon, PlusIcon, RemoveIcon, SlidersIcon } from "./icons.tsx";
import { Menu, useMenuButton, useMenuSurface } from "./menu.tsx";
import type { Closing, MenuRow } from "./menu.tsx";
import { NameField } from "./name-field.tsx";
import type { OpenFolders } from "./open-folders.ts";
import { cutTip } from "./tips.ts";
import { dropAt, rowsOf } from "./views-list.ts";
import type { Drop, Mark, Row, Span } from "./views-list.ts";

// How near the top or the bottom edge of manage mode a drag scrolls it, and by
// how much at each move of the pointer, in pixels.
const EDGE = 24;
const STEP = 8;

// Whether the board option view_tabs is on, which the component that reads it
// draws again as it changes.
export function useViewTabs(core: CoreApi): boolean {
  const subscribe = useCallback(
    (changed: () => void) =>
      core.store.subscribe(() => {
        changed();
      }),
    [core],
  );
  return useSyncExternalStore(subscribe, () => core.store.board().options.view_tabs === true);
}

export function Views({ openFolders }: { readonly openFolders: OpenFolders }): ReactElement | null {
  const { core } = useDock();
  const board = useBoard(core);
  const state = dockState(board);
  if (state === undefined) return null;
  const tabs = board.options.view_tabs === true;
  // The menu keeps its place among the children as the line of tabs comes and
  // goes, so that its button keeps the focus.
  return (
    <div className="blockr-dock-bar-views" data-tabs={tabs ? "" : undefined}>
      {tabs ? <TabLine state={state} openFolders={openFolders} /> : null}
      <ViewsMenu state={state} openFolders={openFolders} tabs={tabs} />
      {tabs ? null : <span className="blockr-dock-bar-rule" aria-hidden="true" />}
    </div>
  );
}

// The views menu while it is open: by the button that opened it, whether the
// keyboard opened it, and whether it is in manage mode.
interface Opened {
  readonly anchor: HTMLElement;
  readonly keyboard: boolean;
  readonly managing: boolean;
}

interface ViewsMenuProps {
  readonly state: DockState;
  readonly openFolders: OpenFolders;
  // Whether the views show as a line of tabs, beside which the menu's button
  // is a chevron alone.
  readonly tabs: boolean;
}

function ViewsMenu({ state, openFolders, tabs }: ViewsMenuProps): ReactElement {
  const { core } = useDock();
  const open = useOpenFolders(openFolders);
  const [opened, setOpened] = useState<Opened | null>(null);
  const button = useRef<HTMLButtonElement>(null);
  const props = useMenuButton(
    opened !== null,
    (anchor, keyboard) => {
      openFolders.open(activeHolders(state));
      setOpened({ anchor, keyboard, managing: false });
    },
    () => {
      setOpened(null);
    },
  );
  const close = useCallback((how: Closing) => {
    setOpened(null);
    if (how.restore) button.current?.focus({ preventScroll: true, focusVisible: !how.pointer });
  }, []);
  const active = state.active === null ? undefined : state.views[state.active];
  const name = active?.name ?? "Views";

  const actions = (at: Opened): MenuRow[] =>
    core.lock.board
      ? []
      : [
          {
            key: "manage",
            label: "Manage views",
            lead: <SlidersIcon />,
            divided: true,
            onSelect: () => {
              setOpened({ ...at, managing: true });
            },
          },
          {
            key: "tabs",
            label: "Show views as tabs",
            checked: tabs,
            onSelect: () => {
              runEdit(core, core.id("blockr.core", "set_option"), { id: "view_tabs", value: !tabs });
            },
          },
        ];

  return (
    <>
      <button
        ref={button}
        type="button"
        className={tabs ? "blockr-dock-bar-button blockr-dock-views-chevron" : "blockr-dock-bar-button blockr-dock-views-button"}
        aria-label={tabs ? "Views" : `Views: ${name}`}
        {...(tabs ? { title: "Views" } : cutTip((cut) => (cut ? name : undefined)))}
        {...props}
      >
        {tabs ? null : <span className="blockr-dock-bar-label">{name}</span>}
        <Chevron />
      </button>
      {opened === null ? null : opened.managing ? (
        <Manage
          anchor={opened.anchor}
          keyboard={opened.keyboard}
          state={state}
          openFolders={openFolders}
          onClose={close}
          onDone={() => {
            setOpened({ ...opened, managing: false });
          }}
        />
      ) : (
        <Menu
          anchor={opened.anchor}
          label="Views"
          align="end"
          groups={[
            { key: "views", rows: viewRows(core, state, null, open, openFolders) },
            { key: "actions", rows: actions(opened) },
          ]}
          keyboard={opened.keyboard}
          onClose={close}
        />
      )}
    </>
  );
}

// The rows of a menu that lists the views and folders of a folder, or of the
// top for null, given the folders that are open: a view's row, the current
// pick where the view is active, makes the view active, and a folder's row
// is a fold.
function viewRows(
  core: CoreApi,
  state: DockState,
  root: FolderId | null,
  open: ReadonlySet<FolderId>,
  openFolders: OpenFolders,
): MenuRow[] {
  return rowsOf(state, root, open).map(({ entry, depth, open: unfolded }): MenuRow => {
    const common = { key: keyOf(entry), label: nameOf(state, entry), depth };
    if ("view" in entry) {
      return {
        ...common,
        current: entry.view === state.active,
        onSelect: () => {
          activate(core, state, entry.view);
        },
      };
    }
    return {
      ...common,
      fold: {
        open: unfolded,
        onToggle: (on) => {
          openFolders.set(entry.folder, on);
        },
      },
    };
  });
}

// A drag of a view or a folder by its grip, with where it would land as the
// pointer lets go.
interface Drag {
  readonly entry: Entry;
  readonly drop: Drop | undefined;
}

interface ManageProps {
  // The button that opened the views menu.
  readonly anchor: HTMLElement;
  readonly keyboard: boolean;
  readonly state: DockState;
  readonly openFolders: OpenFolders;
  readonly onClose: (how: Closing) => void;
  readonly onDone: () => void;
}

// The views menu in manage mode, a popover in the menu's place: every view
// and folder that the menu shows, each with its grip, its name and its cross,
// and below them the buttons that add a view or a folder, and Done.
// A row is renamed, or asks whether to remove it, one at a time.
function Manage({ anchor, keyboard, state, openFolders, onClose, onDone }: ManageProps): ReactElement {
  const { core } = useDock();
  const ref = useRef<HTMLDivElement>(null);
  const open = useOpenFolders(openFolders);
  const rows = rowsOf(state, null, open);
  const slot = rows.some((row) => "folder" in row.entry);
  const [renaming, setRenaming] = useState<string | undefined>(undefined);
  const [confirming, setConfirming] = useState<string | undefined>(undefined);
  const [drag, setDrag] = useState<Drag | undefined>(undefined);
  // The drag as the pointer last moved, which a drop reads before React draws
  // that move.
  const dragging = useRef<Drag | undefined>(undefined);
  const rowList = useRef<HTMLDivElement>(null);

  useMenuSurface(ref, anchor, { depth: 0, align: "end", keyboard, onClose });
  useLayoutEffect(() => {
    ref.current?.focus({ preventScroll: true });
  }, []);

  // The popover takes the focus from a row that goes, so that it keeps it.
  const keepFocus = () => {
    ref.current?.focus({ preventScroll: true, focusVisible: false });
  };

  const follow = (next: Drag | undefined) => {
    dragging.current = next;
    setDrag(next);
  };

  const addView = () => {
    void applied(core, core.id("blockr.dock", "add_view"), {}).then((done) => {
      const added = done ? dockState(core.store.board())?.active : null;
      if (added !== undefined && added !== null) setRenaming(keyOf({ view: added }));
    });
  };

  const addFolder = () => {
    const before = new Set(Object.keys(state.folders));
    void applied(core, core.id("blockr.dock", "add_folder"), {}).then((done) => {
      const after = done ? Object.keys(dockState(core.store.board())?.folders ?? {}) : [];
      const added = after.find((id) => !before.has(id));
      if (added !== undefined) setRenaming(keyOf({ folder: added }));
    });
  };

  const grip = (entry: Entry): GripHandlers => ({
    onPointerDown(event) {
      if (event.button !== 0) return;
      event.preventDefault();
      event.currentTarget.setPointerCapture(event.pointerId);
      setConfirming(undefined);
      follow({ entry, drop: undefined });
    },
    onPointerMove(event) {
      const el = rowList.current;
      if (dragging.current === undefined || el === null) return;
      scrollNearEdge(ref.current, event.clientY);
      follow({ entry, drop: dropAt(state, rows, spansOf(el), event.clientY, entry) });
    },
    onPointerUp() {
      const drop = dragging.current?.drop;
      follow(undefined);
      if (drop !== undefined) move(core, openFolders, entry, drop);
    },
    onPointerCancel() {
      follow(undefined);
    },
  });

  return (
    <div
      ref={ref}
      popover="auto"
      role="dialog"
      aria-label="Manage views"
      tabIndex={-1}
      className="blockr-dock-menu blockr-dock-views-manage"
      data-align="end"
      data-depth={0}
      onToggle={(event) => {
        if (event.newState === "closed") onClose({ restore: false, pointer: false });
      }}
      onKeyDown={(event) => {
        if (event.key !== "Escape") return;
        event.preventDefault();
        event.stopPropagation();
        onClose({ restore: true, pointer: false });
      }}
    >
      <div ref={rowList} className="blockr-dock-views-rows">
        {rows.map((row, i) => {
          const { entry } = row;
          const key = keyOf(entry);
          const name = nameOf(state, entry);
          if (confirming === key) {
            return (
              <ConfirmRow
                key={key}
                name={name}
                depth={row.depth}
                onRemove={() => {
                  keepFocus();
                  setConfirming(undefined);
                  remove(core, entry);
                }}
                onCancel={() => {
                  setConfirming(undefined);
                }}
              />
            );
          }
          return (
            <ManagedRow
              key={key}
              row={row}
              name={name}
              slot={slot}
              renaming={renaming === key}
              removable={removable(core, entry)}
              dragging={drag !== undefined && sameEntry(drag.entry, entry)}
              mark={drag?.drop?.mark.row === i ? drag.drop.mark : undefined}
              grip={grip(entry)}
              onFold={() => {
                if ("folder" in entry) openFolders.set(entry.folder, !row.open);
              }}
              onRename={() => {
                setConfirming(undefined);
                setRenaming(key);
              }}
              onRenamed={(next) => {
                rename(core, entry, next);
              }}
              onRenameDone={() => {
                setRenaming(undefined);
              }}
              onConfirm={() => {
                setRenaming(undefined);
                setConfirming(key);
              }}
            />
          );
        })}
      </div>
      <hr className="blockr-dock-menu-divider" />
      <div className="blockr-dock-views-foot">
        <button type="button" className="blockr-dock-views-add" onClick={addView}>
          <PlusIcon />
          New view
        </button>
        <button type="button" className="blockr-dock-views-add" onClick={addFolder}>
          <PlusIcon />
          New folder
        </button>
        <button
          type="button"
          className="blockr-dock-views-done"
          onClick={() => {
            keepFocus();
            onDone();
          }}
        >
          Done
        </button>
      </div>
    </div>
  );
}

// The handlers of a row's grip, through which a drag moves its view or
// folder: the grip keeps the pointer from the moment it goes down on it
// until it lets go.
interface GripHandlers {
  readonly onPointerDown: (event: PointerEvent<HTMLSpanElement>) => void;
  readonly onPointerMove: (event: PointerEvent<HTMLSpanElement>) => void;
  readonly onPointerUp: () => void;
  readonly onPointerCancel: () => void;
}

interface ManagedRowProps {
  readonly row: Row;
  readonly name: string;
  // Whether the row leaves room for a folder's chevron, since a folder's row
  // shows.
  readonly slot: boolean;
  readonly renaming: boolean;
  readonly removable: boolean;
  // Whether a drag moves the row's view or folder.
  readonly dragging: boolean;
  // Where a drag would land, where the row marks it.
  readonly mark: Mark | undefined;
  readonly grip: GripHandlers;
  readonly onFold: () => void;
  readonly onRename: () => void;
  readonly onRenamed: (name: string) => void;
  readonly onRenameDone: () => void;
  readonly onConfirm: () => void;
}

// A row of the menu in manage mode.
// Where the keyboard ends a rename, with Enter or Escape, the name takes the
// focus back from the field, so that the keyboard stays on the row.
function ManagedRow({
  row,
  name,
  slot,
  renaming,
  removable,
  dragging,
  mark,
  grip,
  onFold,
  onRename,
  onRenamed,
  onRenameDone,
  onConfirm,
}: ManagedRowProps): ReactElement {
  const el = useRef<HTMLDivElement>(null);
  const nameButton = useRef<HTMLButtonElement>(null);
  const refocus = useRef(false);
  const { entry } = row;
  const isView = "view" in entry;

  useLayoutEffect(() => {
    if (renaming || !refocus.current) return;
    refocus.current = false;
    nameButton.current?.focus();
  }, [renaming]);

  const style = { ...depthStyle(row.depth), ...(mark === undefined ? {} : { "--drop-depth": mark.depth }) } as CSSProperties;
  return (
    <div
      ref={el}
      className="blockr-dock-views-managed"
      data-row=""
      data-drop={mark?.at}
      data-dragging={dragging ? "" : undefined}
      style={style}
    >
      <span className="blockr-dock-views-grip" aria-hidden="true" {...grip}>
        <GripIcon />
      </span>
      {isView ? (
        slot ? (
          <span className="blockr-dock-views-slot" />
        ) : null
      ) : (
        <button
          type="button"
          className="blockr-dock-views-fold"
          aria-expanded={row.open}
          aria-label={`${row.open ? "Close" : "Open"} the folder ${name}`}
          onClick={onFold}
        >
          <Chevron />
        </button>
      )}
      {renaming ? (
        <NameField
          name={name}
          label={isView ? "View name" : "Folder name"}
          className="blockr-dock-views-field"
          onSubmit={onRenamed}
          onDone={() => {
            const doc = el.current?.ownerDocument;
            refocus.current = doc !== undefined && el.current?.contains(doc.activeElement) === true;
            onRenameDone();
          }}
        />
      ) : (
        <button
          ref={nameButton}
          type="button"
          className="blockr-dock-views-name"
          onClick={onRename}
          {...cutTip((cut) => (cut ? `${name}\nClick to rename` : "Click to rename"))}
        >
          {name}
        </button>
      )}
      {removable ? (
        <button
          type="button"
          className="blockr-dock-views-remove"
          aria-label={`Remove ${name}`}
          title={isView ? "Remove view" : "Remove folder"}
          onClick={onConfirm}
        >
          <RemoveIcon />
        </button>
      ) : null}
    </div>
  );
}

interface ConfirmRowProps {
  readonly name: string;
  readonly depth: number;
  readonly onRemove: () => void;
  readonly onCancel: () => void;
}

// A row that asks whether to remove its view or folder, in the row's place,
// as today's views menu asks.
// Its Remove button takes the focus, and only that button removes.
// Escape, or the focus leaving the row, as a click elsewhere moves it, takes
// the question back.
function ConfirmRow({ name, depth, onRemove, onCancel }: ConfirmRowProps): ReactElement {
  const removeButton = useRef<HTMLButtonElement>(null);
  useLayoutEffect(() => {
    removeButton.current?.focus();
  }, []);
  return (
    <div
      className="blockr-dock-views-managed"
      data-row=""
      data-confirming=""
      style={depthStyle(depth)}
      onBlur={(event: FocusEvent<HTMLDivElement>) => {
        const next = event.relatedTarget;
        if (!(next instanceof Node) || !event.currentTarget.contains(next)) onCancel();
      }}
      onKeyDown={(event) => {
        if (event.key !== "Escape") return;
        event.preventDefault();
        event.stopPropagation();
        onCancel();
      }}
    >
      <span className="blockr-dock-views-confirm-text">{`Remove “${name}”?`}</span>
      <button ref={removeButton} type="button" className="blockr-dock-views-confirm" onClick={onRemove}>
        Remove
      </button>
    </div>
  );
}

interface TabLineProps {
  readonly state: DockState;
  readonly openFolders: OpenFolders;
}

// The line of tabs under the bar, while the board option view_tabs is on.
function TabLine({ state, openFolders }: TabLineProps): ReactElement {
  const { core } = useDock();
  const line = useRef<HTMLElement>(null);
  const { active } = state;
  // The folder at the top that holds the active view, whose tab the line
  // marks.
  const outer = activeHolders(state).at(-1);

  useLayoutEffect(() => {
    reveal(line.current);
  }, [active, outer]);

  return (
    <nav ref={line} className="blockr-dock-tabs" aria-label="Views">
      {inFolder(state, null).map((entry) =>
        "view" in entry ? (
          <button
            key={keyOf(entry)}
            type="button"
            className="blockr-dock-tab"
            aria-current={entry.view === active ? "page" : undefined}
            onClick={() => {
              activate(core, state, entry.view);
            }}
          >
            {nameOf(state, entry)}
          </button>
        ) : (
          <FolderTab key={keyOf(entry)} state={state} folder={entry.folder} current={entry.folder === outer} openFolders={openFolders} />
        ),
      )}
    </nav>
  );
}

interface FolderTabProps {
  readonly state: DockState;
  readonly folder: FolderId;
  // Whether the folder holds the active view.
  readonly current: boolean;
  readonly openFolders: OpenFolders;
}

// The tab of a folder, which opens a menu of what the folder holds.
function FolderTab({ state, folder, current, openFolders }: FolderTabProps): ReactElement {
  const { core } = useDock();
  const open = useOpenFolders(openFolders);
  const [opened, setOpened] = useState<{ readonly anchor: HTMLElement; readonly keyboard: boolean } | null>(null);
  const button = useRef<HTMLButtonElement>(null);
  const props = useMenuButton(
    opened !== null,
    (anchor, keyboard) => {
      openFolders.open(activeHolders(state));
      setOpened({ anchor, keyboard });
    },
    () => {
      setOpened(null);
    },
  );
  const close = useCallback((how: Closing) => {
    setOpened(null);
    if (how.restore) button.current?.focus({ preventScroll: true, focusVisible: !how.pointer });
  }, []);
  const name = nameOf(state, { folder });
  return (
    <>
      <button ref={button} type="button" className="blockr-dock-tab" aria-current={current ? "true" : undefined} {...props}>
        {name}
        <Chevron />
      </button>
      {opened === null ? null : (
        <Menu
          anchor={opened.anchor}
          label={name}
          groups={[{ key: "views", rows: viewRows(core, state, folder, open, openFolders) }]}
          note="This folder is empty."
          keyboard={opened.keyboard}
          onClose={close}
        />
      )}
    </>
  );
}

// The folders that are open, which the component draws again as one opens or
// closes.
function useOpenFolders(openFolders: OpenFolders): ReadonlySet<FolderId> {
  return useSyncExternalStore(openFolders.subscribe, openFolders.get);
}

// The folders that hold the active view.
function activeHolders(state: DockState): FolderId[] {
  return state.active === null ? [] : foldersHolding(state, { view: state.active });
}

// Scrolls the line of tabs sideways, and only sideways, so that its marked
// tab shows, while the page stays where it is.
function reveal(line: HTMLElement | null): void {
  if (line === null) return;
  const tab = line.querySelector<HTMLElement>("[aria-current]");
  if (tab === null) return;
  const bounds = line.getBoundingClientRect();
  const at = tab.getBoundingClientRect();
  if (at.left < bounds.left) line.scrollLeft -= bounds.left - at.left;
  else if (at.right > bounds.right) line.scrollLeft += at.right - bounds.right;
}

// Scrolls manage mode as a drag nears its top or its bottom edge, so that a
// drag reaches the rows out of view.
function scrollNearEdge(panel: HTMLElement | null, y: number): void {
  if (panel === null) return;
  const { top, bottom } = panel.getBoundingClientRect();
  if (y < top + EDGE) panel.scrollTop -= STEP;
  else if (y > bottom - EDGE) panel.scrollTop += STEP;
}

// Where each row of manage mode sits on the screen, in their order.
function spansOf(rowList: HTMLElement): Span[] {
  return [...rowList.querySelectorAll<HTMLElement>(":scope > [data-row]")].map((row) => {
    const { top, bottom } = row.getBoundingClientRect();
    return { top, bottom };
  });
}

// Runs one of the dock's commands as an edit, and gives whether the board
// took it, where core tells the user why it refused it.
async function applied(core: CoreApi, id: CommandId, args: Readonly<Record<string, Json>>): Promise<boolean> {
  return await core.commands.run(id, args).then(
    () => true,
    () => false,
  );
}

// Makes a view the active one, unless it is already.
function activate(core: CoreApi, state: DockState, view: ViewId): void {
  if (view !== state.active) runEdit(core, core.id("blockr.dock", "activate_view"), { view });
}

function rename(core: CoreApi, entry: Entry, name: string): void {
  if ("view" in entry) runEdit(core, core.id("blockr.dock", "rename_view"), { view: entry.view, name });
  else runEdit(core, core.id("blockr.dock", "rename_folder"), { folder: entry.folder, name });
}

function remove(core: CoreApi, entry: Entry): void {
  if ("view" in entry) runEdit(core, core.id("blockr.dock", "remove_view"), { view: entry.view });
  else runEdit(core, core.id("blockr.dock", "remove_folder"), { folder: entry.folder });
}

// Whether the dock removes a view or a folder: it refuses the last view.
function removable(core: CoreApi, entry: Entry): boolean {
  return "view" in entry
    ? core.commands.preflight(core.id("blockr.dock", "remove_view"), { view: entry.view })
    : core.commands.preflight(core.id("blockr.dock", "remove_folder"), { folder: entry.folder });
}

// Moves a view or a folder where a drop lands, and opens the folder that
// takes it, so that manage mode shows it there.
function move(core: CoreApi, openFolders: OpenFolders, entry: Entry, drop: Drop): void {
  const args: Record<string, Json> =
    "view" in entry ? { view: entry.view, folder: drop.folder } : { folder: entry.folder, into: drop.folder };
  if (drop.before !== undefined) args.before = drop.before;
  const id = core.id("blockr.dock", "view" in entry ? "move_view" : "move_folder");
  if (!core.commands.preflight(id, args)) return;
  if (drop.folder !== null) openFolders.open([drop.folder]);
  runEdit(core, id, args);
}

// The name of a view or a folder.
function nameOf(state: DockState, entry: Entry): string {
  return ("view" in entry ? state.views[entry.view]?.name : state.folders[entry.folder]?.name) ?? "";
}

// The key of a view's or a folder's row, which differs between a view and a
// folder of the same id.
function keyOf(entry: Entry): string {
  return "view" in entry ? `view:${entry.view}` : `folder:${entry.folder}`;
}

// How deep a row of manage mode sits, which indents it.
function depthStyle(depth: number): CSSProperties {
  return { "--depth": depth } as CSSProperties;
}
