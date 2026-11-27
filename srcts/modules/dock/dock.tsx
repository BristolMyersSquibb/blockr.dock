// The dock's React app, which draws dockview in the page's element and hands
// dockview's api to ./drawing.ts as dockview is ready.
// Dockview takes its light-spaced theme, as today's dock does, under the
// stylesheet's bridge from blockr.ui's tokens to dockview's variables, which
// follows the board option dark_mode (./index.css).
//
// The dock places each plugin whose class has a `ui` in a panel of its own
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.4#a-plugins-panel):
// - The frame of a plugin's panel fills the panel's body.
//   When dockview draws the panel, the dock mounts the plugin's UI in the
//   frame with mountPlugin().
// - The dock gives dockview the renderer "always", as today's dock does.
//   Under it, dockview keeps the content of every panel on the page.
//   So a panel behind another tab, or on a collapsed rail, stays mounted, and
//   its UI keeps what it holds, such as a scroll position.
//   Dockview hides such a panel rather than squeeze it.
// - Dockview drops a panel when the user closes its tab, when the active view
//   changes to one that does not show it, or when another board opens.
//   The dock then runs the cleanup that mountPlugin() returned.
// The frame of a block's panel holds the block's card (./card).
// A click on a block's tab gives the block focus in CoreApi.focus, as a click
// into its card does, while a click on a plugin's tab leaves the focus where
// it is.
// The tab of a plugin's panel shows a dot while a warning or an error about
// the plugin holds, in blockr.ui's danger colour for an error and its amber
// for a warning, whichever panel its tab group or rail shows, a collapsed
// rail's included
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.4#places-besides-the-panel).
// Its tooltip holds their messages, the errors first, so that the user can
// read one that the plugin does not show, such as why its UI cannot draw.
// The tooltip shows as the pointer rests on the tab, or as the keyboard's
// focus moves onto it, and a screen reader reads the messages as the tab's
// description.
// The tab's element is dockview's, which it draws around the dock's tab, so
// the dock gives that element the tooltip and the description.
//
// The header of each tab group holds a "+", which opens the panel picker
// (./panel-picker.tsx).
// A rail's header holds none.
// An active view that shows no panel says "Start by adding a panel", with a
// button that opens the picker too.
// Under the board lock, no command adds or removes a panel.
// So the dock draws no "+", no close button on a tab and no picker there, and
// an empty view says "This view is empty".
//
// While the views menu on the bar draws its line of tabs, under the bar's row,
// the dock's root starts below the line (./views-menu.tsx).
// While the options sidebar is pinned, the dock's root makes room for it at
// the window's right edge (./options-sidebar.tsx, ./index.css). Dockview lays
// the view out again, as for any change of the root's size.

import type { Board, CoreApi, PluginClassId } from "@blockr/core";
import { useBoard, useNotified } from "@blockr/core/react";
import { Button } from "@blockr/ui";
import { DockviewDefaultTab, DockviewReact, themeLightSpaced } from "dockview-react";
import type {
  DockviewReadyEvent,
  IDockviewHeaderActionsProps,
  IDockviewPanelHeaderProps,
  IDockviewPanelProps,
  IDockviewReactProps,
} from "dockview-react";
import { useCallback, useEffect, useMemo, useRef, useState, useSyncExternalStore } from "react";
import type { ReactElement } from "react";
import { activeView } from "../../commands/index.ts";
import { PANEL_COMPONENT, panelFromId } from "../../layout/index.ts";
import type { PanelParams } from "../../layout/index.ts";
import { gridPanels, idText } from "../../state/index.ts";
import type { IdRecord } from "../../state/index.ts";
import { bindTip, BlockCard, createTooltips, useTooltip } from "./card/index.ts";
import { DockContext, giveFocus, panelClass, useDock } from "./context.ts";
import { drawDock } from "./drawing.ts";
import { LockIcon } from "./icons.tsx";
import { Mounted } from "./mounted.tsx";
import { tabNotifications } from "./notifications.ts";
import type { Narrow } from "./narrow.ts";
import { PanelPicker, usePickerButton } from "./panel-picker.tsx";
import type { Settings } from "./settings.ts";
import type { Sidebar } from "./sidebar.ts";
import { useViewTabs } from "./views-menu.tsx";

// The frame of a panel, which names the block or the plugin whose panel it
// is, and holds a block's card.
function PanelFrame({ params }: IDockviewPanelProps<PanelParams>): ReactElement {
  const { panel } = params;
  return "block" in panel ? (
    <div className="blockr-dock-panel" data-block={panel.block}>
      <BlockCard id={panel.block} />
    </div>
  ) : (
    <PluginFrame panel={panel} />
  );
}

// The frame of a plugin's panel, which holds the plugin's UI while dockview
// draws the panel (./mounted.tsx).
// It names the plugin's class in `data-plugin`, as core's messages name an id.
// Core makes one Id for each pair, so the frame mounts the plugin once.
function PluginFrame({ panel }: { readonly panel: { readonly plugin: IdRecord } }): ReactElement {
  const { core } = useDock();
  const plugin = panelClass(core, panel);
  const mount = useCallback((el: HTMLElement) => core.mountPlugin(el, plugin), [core, plugin]);
  return <Mounted mount={mount} className="blockr-dock-panel" data={{ "data-plugin": idText(plugin) }} />;
}

const components: IDockviewReactProps["components"] = { [PANEL_COMPONENT]: PanelFrame };

// A panel's tab, which has no close button on a locked board.
function Tab(props: IDockviewPanelHeaderProps<PanelParams>): ReactElement {
  const { core } = useDock();
  const { panel } = props.params;
  if ("plugin" in panel) return <PluginTab {...props} plugin={panelClass(core, panel)} />;
  return (
    <DockviewDefaultTab
      {...props}
      hideClose={core.lock.board}
      onPointerDown={() => {
        giveFocus(core, panel.block);
      }}
    />
  );
}

// The tab of a plugin's panel, with the dot while a warning or an error about
// the plugin holds, and their messages in its tooltip and description.
// Its frame lays out as dockview's tab alone (./index.css), and the dock finds
// from it the tab's element that dockview draws.
function PluginTab({ plugin, ...props }: IDockviewPanelHeaderProps<PanelParams> & { readonly plugin: PluginClassId }): ReactElement {
  const { core } = useDock();
  const notified = useNotified(core, { plugin });
  const told = useMemo(() => tabNotifications(notified), [notified]);
  const messages = told.map((notification) => notification.message);
  // A message of several lines takes a line of the tooltip for each.
  const lines = messages.flatMap((message) => message.split("\n")).map((text) => ({ text }));
  const { props: tip, tooltip } = useTooltip(lines.length === 0 ? null : lines);
  const frame = useRef<HTMLDivElement>(null);
  const description = messages.join("\n");

  useEffect(() => {
    const tab = frame.current?.closest<HTMLElement>('[role="tab"]');
    if (tab === null || tab === undefined) return undefined;
    const unbind = bindTip(tab, tip);
    if (description === "") tab.removeAttribute("aria-description");
    else tab.setAttribute("aria-description", description);
    return () => {
      unbind();
      tab.removeAttribute("aria-description");
    };
  }, [tip, description]);

  return (
    <div ref={frame} className="blockr-dock-tab-frame" data-notified={told[0]?.severity}>
      <DockviewDefaultTab {...props} hideClose={core.lock.board} />
      {tooltip}
    </div>
  );
}

// The "+" in the header of a tab group, which opens the panel picker.
// A pick goes near the panel that the group shows, as a new tab in the group.
function AddPanelButton({ api, group }: IDockviewHeaderActionsProps): ReactElement | null {
  const button = usePickerButton(() => {
    const shown = group.activePanel;
    return shown === undefined ? undefined : panelFromId(shown.id);
  });
  if (api.location.type === "edge") return null;
  return (
    <div className="blockr-dock-add-slot">
      <Button variant="quiet" className="blockr-dock-add" aria-label="Add panel" {...button}>
        <svg viewBox="0 0 16 16" width="16" height="16" aria-hidden="true" focusable="false">
          <path d="M8 3v10M3 8h10" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" />
        </svg>
      </Button>
    </div>
  );
}

// Whether the active view shows no panel that the dock draws.
function showsNothing(core: Pick<CoreApi, "id">, board: Board, drawn: ReadonlyMap<PluginClassId, string>): boolean {
  const view = activeView(board);
  if (view === undefined) return false;
  return !gridPanels(view.grid).some((panel) =>
    "block" in panel ? Object.hasOwn(board.blocks, panel.block) : drawn.has(panelClass(core, panel)),
  );
}

// What the dock shows over an empty view.
function EmptyView(): ReactElement | null {
  const { core, drawn } = useDock();
  const board = useBoard(core);
  if (!showsNothing(core, board, drawn)) return null;
  if (!core.lock.board) return <EmptyPrompt />;
  return (
    <div className="blockr-dock-empty">
      <LockIcon size={32} />
      <p>This view is empty</p>
    </div>
  );
}

// The prompt of an empty view, whose button opens the panel picker, whose
// pick goes to the default place.
function EmptyPrompt(): ReactElement {
  const button = usePickerButton(() => undefined);
  return (
    <div className="blockr-dock-empty">
      <svg viewBox="0 0 16 16" width="32" height="32" aria-hidden="true" focusable="false">
        <circle cx="8" cy="8" r="6.5" fill="none" stroke="currentColor" />
        <path d="M8 5v6M5 8h6" fill="none" stroke="currentColor" strokeLinecap="round" />
      </svg>
      <p>Start by adding a panel</p>
      <Button variant="quiet" className="blockr-dock-empty-add" {...button}>
        Add panel
      </Button>
    </div>
  );
}

export interface DockProps {
  readonly core: CoreApi;
  readonly plugin: PluginClassId;
  readonly settings: Settings;
  readonly narrow: Narrow;
  readonly drawn: ReadonlyMap<PluginClassId, string>;
  // Whether the bar holds the views menu, which draws its line of tabs under
  // the bar while the board option view_tabs is on.
  readonly viewsMenu: boolean;
  readonly sidebar: Sidebar;
}

export function Dock({ core, plugin, settings, narrow, drawn, viewsMenu, sidebar }: DockProps): ReactElement {
  const root = useRef<HTMLDivElement>(null);
  const stop = useRef<(() => void) | undefined>(undefined);
  const [tooltips] = useState(createTooltips);
  const shared = useMemo(() => ({ core, plugin, drawn, tooltips }), [core, plugin, drawn, tooltips]);
  const locked = core.lock.board;
  const tabs = useViewTabs(core) && viewsMenu;
  const { pinned } = useSyncExternalStore(sidebar.subscribe, sidebar.state);

  const onReady = useCallback(
    (event: DockviewReadyEvent) => {
      const el = root.current;
      if (el === null) return;
      stop.current?.();
      stop.current = drawDock({ core, plugin, api: event.api, el, settings, narrow, drawn });
    },
    [core, plugin, settings, narrow, drawn],
  );

  useEffect(
    () => () => {
      stop.current?.();
      stop.current = undefined;
    },
    [],
  );

  const view = (
    <>
      <DockviewReact
        components={components}
        onReady={onReady}
        theme={themeLightSpaced}
        defaultRenderer="always"
        defaultTabComponent={Tab}
        {...(locked ? {} : { leftHeaderActionsComponent: AddPanelButton })}
      />
      <EmptyView />
    </>
  );

  return (
    <DockContext value={shared}>
      <div className="blockr-dock" ref={root} data-tabs={tabs ? "" : undefined} data-pushed={pinned ? "" : undefined}>
        {locked ? view : <PanelPicker>{view}</PanelPicker>}
      </div>
    </DockContext>
  );
}
