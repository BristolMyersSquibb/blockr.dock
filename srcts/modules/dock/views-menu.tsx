// The views menu, the dock's own part `views` of the bar (./navbar.tsx)
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.5#the-docks-own-parts).
// It lists the views and folders of the navigation, each in the order that its
// `index` gives within its folder, with what an open folder holds below the
// folder's row (./views-list.ts).
// A click on a folder's row opens or closes it in place.
// Which folders are open stays on the screen, outside the board
// (./open-folders.ts).
// As the menu opens, the folders that hold the active view open, so that the
// menu shows the active view, whose row it sets in weight 600, as blockr.ui's
// design system marks the current pick.
// A pick of a view runs blockr.dock-activate_view, under every lock, and
// closes the menu.
// The bar shows the active view's name on the menu's button, and a short rule
// after it sets the menu apart from the parts after it.
//
// Without the board lock, the menu also adds, renames, moves and removes views
// and folders, through the dock's commands, in its manage mode, as today's
// views menu adds, renames, reorders and removes views (view_nav_ui() in
// R/dock-view.R and inst/assets/js/view-binding.js, at 655e323):
// - The row "Manage views", below the list, turns the menu to manage mode, and
//   Done turns it back. Closing the menu leaves manage mode too.
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
//   through core's command blockr.core-set_option, as today's row does.
//
// While the board option view_tabs is on, the views menu leaves the bar's row
// for a line of tabs under it, followed by the menu as a chevron alone, as
// today.
// The line holds a tab for each view and folder at the top of the navigation.
// A view's tab runs blockr.dock-activate_view, and a folder's tab opens a menu
// of what the folder holds, in which a folder opens and closes as in the
// views menu, so that the line stays one row however deep the folders go.
// The line marks the active view's tab, or the tab of the folder that holds
// the active view.
// Where the tabs outgrow the line, it scrolls sideways, and it brings the
// marked tab into view as the active view changes.

import type { CoreApi, Json } from "@blockr/core";
import { useCallback, useLayoutEffect, useRef, useState, useSyncExternalStore } from "react";
import type { CSSProperties, FocusEvent, KeyboardEvent, PointerEvent, ReactElement, RefObject } from "react";
import { dockState } from "../../commands/index.ts";
import { foldersHolding, inFolder, sameEntry } from "../../state/index.ts";
import type { DockState, Entry, FolderId, ViewId } from "../../state/index.ts";
import { useBoard, useDock } from "./context.ts";
import { CheckIcon, Chevron, GripIcon, PlusIcon, RemoveIcon, SlidersIcon } from "./icons.tsx";
import { MenuPopover, moveFocus, useMenuButton } from "./menu.tsx";
import { NameField } from "./name-field.tsx";
import type { OpenFolders } from "./open-folders.ts";
import { cutTip } from "./tips.ts";
import { dropAt, rowsOf } from "./views-list.ts";
import type { Drop, Mark, Row, Span } from "./views-list.ts";

const ACTIVATE_VIEW = "blockr.dock-activate_view";
const ADD_VIEW = "blockr.dock-add_view";
const ADD_FOLDER = "blockr.dock-add_folder";
const RENAME_VIEW = "blockr.dock-rename_view";
const RENAME_FOLDER = "blockr.dock-rename_folder";
const REMOVE_VIEW = "blockr.dock-remove_view";
const REMOVE_FOLDER = "blockr.dock-remove_folder";
const MOVE_VIEW = "blockr.dock-move_view";
const MOVE_FOLDER = "blockr.dock-move_folder";
const SET_OPTION = "blockr.core-set_option";

// The rows of a menu of views and folders that the keys move through: those
// of every open folder among them, in the order of the page.
const ROW = '[role="menuitem"], [role="menuitemradio"], [role="menuitemcheckbox"]';

// How near the top or the bottom edge of the menu a drag scrolls it, and by
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

interface ViewsMenuProps {
  readonly state: DockState;
  readonly openFolders: OpenFolders;
  // Whether the views show as a line of tabs, beside which the menu's button
  // is a chevron alone.
  readonly tabs: boolean;
}

function ViewsMenu({ state, openFolders, tabs }: ViewsMenuProps): ReactElement {
  const { core } = useDock();
  const button = useRef<HTMLButtonElement>(null);
  const list = useRef<HTMLDivElement>(null);
  const [managing, setManaging] = useState(false);
  const { opened, close, handlers } = useMenuButton(button, list, {
    onOpen: () => {
      setManaging(false);
      openFolders.open(activeHolders(state));
    },
    first: currentRow,
  });
  const active = state.active === null ? undefined : state.views[state.active];
  const name = active?.name ?? "Views";
  // The popover takes the focus from a row that goes, so that the menu keeps
  // it.
  const keepFocus = () => {
    list.current?.focus({ focusVisible: false });
  };

  const pick = (view: ViewId) => {
    close();
    activate(core, state, view);
  };

  return (
    <>
      <button
        ref={button}
        type="button"
        className={tabs ? "blockr-dock-bar-button blockr-dock-views-chevron" : "blockr-dock-bar-button blockr-dock-views-button"}
        aria-haspopup="menu"
        aria-expanded={opened !== undefined}
        aria-label={tabs ? "Views" : `Views: ${name}`}
        {...(tabs ? { title: "Views" } : cutTip((cut) => (cut ? name : undefined)))}
        {...handlers}
      >
        {tabs ? null : <span className="blockr-dock-bar-label">{name}</span>}
        <Chevron />
      </button>
      <MenuPopover
        list={list}
        label={managing ? "Manage views" : "Views"}
        depth={0}
        role={managing ? "dialog" : "menu"}
        className="blockr-dock-views-menu"
        close={close}
        onKeyDown={
          managing
            ? undefined
            : (event) => {
                moveInTree(event, openFolders);
              }
        }
      >
        {opened === undefined ? null : managing ? (
          <Manage
            state={state}
            openFolders={openFolders}
            list={list}
            onDone={() => {
              keepFocus();
              setManaging(false);
            }}
          />
        ) : (
          <>
            <Tree state={state} root={null} openFolders={openFolders} onPick={pick} />
            {core.lock.board ? null : (
              <>
                <div role="separator" className="blockr-dock-menu-divider" />
                <button
                  type="button"
                  role="menuitem"
                  tabIndex={-1}
                  className="blockr-dock-menu-item blockr-dock-menu-quiet"
                  onClick={() => {
                    keepFocus();
                    setManaging(true);
                  }}
                >
                  <SlidersIcon />
                  <span className="blockr-dock-menu-label">Manage views</span>
                </button>
                <button
                  type="button"
                  role="menuitemcheckbox"
                  aria-checked={tabs}
                  tabIndex={-1}
                  className="blockr-dock-menu-item blockr-dock-menu-quiet"
                  onClick={() => {
                    close();
                    void run(core, SET_OPTION, { id: "view_tabs", value: !tabs });
                  }}
                >
                  <span className="blockr-dock-menu-label">Show views as tabs</span>
                  {tabs ? <CheckIcon /> : null}
                </button>
              </>
            )}
          </>
        )}
      </MenuPopover>
    </>
  );
}

interface TreeProps {
  readonly state: DockState;
  // The folder whose views and folders the menu lists, or null for the top.
  readonly root: FolderId | null;
  readonly openFolders: OpenFolders;
  readonly onPick: (view: ViewId) => void;
}

// The rows of a menu that lists views and folders, with what each open folder
// holds in a group below the folder's row.
function Tree({ state, root, openFolders, onPick }: TreeProps): ReactElement {
  const open = useOpenFolders(openFolders);
  const rows = rowsOf(state, root, open);
  if (rows.length === 0) {
    return (
      <div role="none" className="blockr-dock-menu-status">
        Empty
      </div>
    );
  }
  // Where a folder's row shows, every row leaves room for the folder's
  // chevron, so that the names line up.
  const slot = rows.some((row) => "folder" in row.entry);
  const draw = (folder: FolderId | null, depth: number): ReactElement[] =>
    rows
      .filter((row) => row.folder === folder && row.depth === depth)
      .map((row) => {
        const { entry } = row;
        const name = nameOf(state, entry);
        if ("view" in entry) {
          return (
            <button
              key={keyOf(entry)}
              type="button"
              role="menuitemradio"
              aria-checked={entry.view === state.active}
              tabIndex={-1}
              className="blockr-dock-menu-item blockr-dock-views-row"
              style={depthStyle(depth)}
              onClick={() => {
                onPick(entry.view);
              }}
            >
              {slot ? <span className="blockr-dock-views-slot" /> : null}
              <span className="blockr-dock-menu-label">{name}</span>
            </button>
          );
        }
        return (
          <div key={keyOf(entry)} role="none">
            <button
              type="button"
              role="menuitem"
              aria-expanded={row.open}
              tabIndex={-1}
              data-folder={entry.folder}
              className="blockr-dock-menu-item blockr-dock-views-row"
              style={depthStyle(depth)}
              onClick={() => {
                openFolders.set(entry.folder, !row.open);
              }}
            >
              <span className="blockr-dock-views-slot">
                <Chevron />
              </span>
              <span className="blockr-dock-menu-label">{name}</span>
            </button>
            {row.open ? (
              <div role="group" aria-label={name}>
                {draw(entry.folder, depth + 1)}
              </div>
            ) : null}
          </div>
        );
      });
  return <>{draw(root, 0)}</>;
}

// A drag of a view or a folder by its grip, with where it would land as the
// pointer lets go.
interface Drag {
  readonly entry: Entry;
  readonly drop: Drop | undefined;
}

interface ManageProps {
  readonly state: DockState;
  readonly openFolders: OpenFolders;
  // The popover, which takes the focus from a row that goes.
  readonly list: RefObject<HTMLDivElement | null>;
  readonly onDone: () => void;
}

// The menu in manage mode: every view and folder that the menu shows, each
// with its grip, its name and its cross, and below them the buttons that add
// a view or a folder, and Done.
// A row is renamed, or asks whether to remove it, one at a time.
function Manage({ state, openFolders, list, onDone }: ManageProps): ReactElement {
  const { core } = useDock();
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

  const follow = (next: Drag | undefined) => {
    dragging.current = next;
    setDrag(next);
  };

  const addView = () => {
    void run(core, ADD_VIEW, {}).then((applied) => {
      const added = applied ? dockState(core.store.board())?.active : null;
      if (added !== undefined && added !== null) setRenaming(keyOf({ view: added }));
    });
  };

  const addFolder = () => {
    const before = new Set(Object.keys(state.folders));
    void run(core, ADD_FOLDER, {}).then((applied) => {
      const after = applied ? Object.keys(dockState(core.store.board())?.folders ?? {}) : [];
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
      scrollNearEdge(list.current, event.clientY);
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
    <>
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
                  list.current?.focus({ focusVisible: false });
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
      <div role="separator" className="blockr-dock-menu-divider" />
      <div className="blockr-dock-views-foot">
        <button type="button" className="blockr-dock-views-add" onClick={addView}>
          <PlusIcon />
          New view
        </button>
        <button type="button" className="blockr-dock-views-add" onClick={addFolder}>
          <PlusIcon />
          New folder
        </button>
        <button type="button" className="blockr-dock-views-done" onClick={onDone}>
          Done
        </button>
      </div>
    </>
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
  const button = useRef<HTMLButtonElement>(null);
  const list = useRef<HTMLDivElement>(null);
  const { opened, close, handlers } = useMenuButton(button, list, {
    onOpen: () => {
      openFolders.open(activeHolders(state));
    },
    first: currentRow,
  });
  const name = nameOf(state, { folder });
  return (
    <>
      <button
        ref={button}
        type="button"
        className="blockr-dock-tab"
        aria-haspopup="menu"
        aria-expanded={opened !== undefined}
        aria-current={current ? "true" : undefined}
        {...handlers}
      >
        {name}
        <Chevron />
      </button>
      <MenuPopover
        list={list}
        label={name}
        depth={0}
        className="blockr-dock-views-menu"
        close={close}
        onKeyDown={(event) => {
          moveInTree(event, openFolders);
        }}
      >
        {opened === undefined ? null : (
          <Tree
            state={state}
            root={folder}
            openFolders={openFolders}
            onPick={(view) => {
              close();
              activate(core, state, view);
            }}
          />
        )}
      </MenuPopover>
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

// The row that takes the focus as the keyboard opens a menu of views and
// folders: the active view's, or the first row where the menu does not list
// it.
function currentRow(list: HTMLElement): HTMLElement | undefined {
  return list.querySelector<HTMLElement>('[role="menuitemradio"][aria-checked="true"]') ?? list.querySelector<HTMLElement>(ROW) ?? undefined;
}

// Moves the focus through the rows of a menu of views and folders as the keys
// of a menu move it, and opens and closes a folder in place as the keys of a
// tree do:
// - ArrowDown and ArrowUp move to the next and the previous row, and Home and
//   End to the first and the last.
// - ArrowRight opens a closed folder, and moves into an open one.
// - ArrowLeft closes an open folder, and moves from a row inside a folder to
//   the folder's row.
function moveInTree(event: KeyboardEvent<HTMLElement>, openFolders: OpenFolders): void {
  if (moveFocus([...event.currentTarget.querySelectorAll<HTMLElement>(ROW)], event)) return;
  const row = event.target instanceof HTMLElement ? event.target : undefined;
  const folder = row?.dataset.folder;
  const expanded = row?.getAttribute("aria-expanded") === "true";
  let next: Element | null | undefined;
  if (event.key === "ArrowRight") {
    if (row === undefined || folder === undefined) return;
    if (expanded) next = row.nextElementSibling?.querySelector(ROW);
    else openFolders.set(folder, true);
  } else if (event.key === "ArrowLeft") {
    if (folder !== undefined && expanded) openFolders.set(folder, false);
    else next = row?.closest('[role="group"]')?.previousElementSibling;
  } else return;
  event.preventDefault();
  event.stopPropagation();
  if (next instanceof HTMLElement) next.focus();
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

// Scrolls the menu as a drag nears its top or its bottom edge, so that a drag
// reaches the rows out of view.
function scrollNearEdge(menu: HTMLElement | null, y: number): void {
  if (menu === null) return;
  const { top, bottom } = menu.getBoundingClientRect();
  if (y < top + EDGE) menu.scrollTop -= STEP;
  else if (y > bottom - EDGE) menu.scrollTop += STEP;
}

// Where each row of the menu in manage mode sits on the screen, in their
// order.
function spansOf(rowList: HTMLElement): Span[] {
  return [...rowList.querySelectorAll<HTMLElement>(":scope > [data-row]")].map((row) => {
    const { top, bottom } = row.getBoundingClientRect();
    return { top, bottom };
  });
}

// Runs a command, where core tells the user why the store refused it, through
// its notification board_edit_failed, and gives whether the store applied it.
async function run(core: CoreApi, id: string, args: Readonly<Record<string, Json>>): Promise<boolean> {
  return await core.commands.run(id, args).then(
    () => true,
    () => false,
  );
}

// Makes a view the active one, unless it is already.
function activate(core: CoreApi, state: DockState, view: ViewId): void {
  if (view !== state.active) void run(core, ACTIVATE_VIEW, { view });
}

function rename(core: CoreApi, entry: Entry, name: string): void {
  if ("view" in entry) void run(core, RENAME_VIEW, { view: entry.view, name });
  else void run(core, RENAME_FOLDER, { folder: entry.folder, name });
}

function remove(core: CoreApi, entry: Entry): void {
  if ("view" in entry) void run(core, REMOVE_VIEW, { view: entry.view });
  else void run(core, REMOVE_FOLDER, { folder: entry.folder });
}

// Whether the dock removes a view or a folder: it refuses the last view.
function removable(core: CoreApi, entry: Entry): boolean {
  return "view" in entry
    ? core.commands.preflight(REMOVE_VIEW, { view: entry.view })
    : core.commands.preflight(REMOVE_FOLDER, { folder: entry.folder });
}

// Moves a view or a folder where a drop lands, and opens the folder that
// takes it, so that the menu shows it there.
function move(core: CoreApi, openFolders: OpenFolders, entry: Entry, drop: Drop): void {
  const args: Record<string, Json> =
    "view" in entry ? { view: entry.view, folder: drop.folder } : { folder: entry.folder, into: drop.folder };
  if (drop.before !== undefined) args.before = drop.before;
  const id = "view" in entry ? MOVE_VIEW : MOVE_FOLDER;
  if (!core.commands.preflight(id, args)) return;
  if (drop.folder !== null) openFolders.open([drop.folder]);
  void run(core, id, args);
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

// How deep a row sits, which indents it.
function depthStyle(depth: number): CSSProperties {
  return { "--depth": depth } as CSSProperties;
}
