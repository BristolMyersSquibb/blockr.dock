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
//
// The header of each tab group holds a "+", which opens the panel picker
// (./picker.tsx).
// A rail's header holds none.
// An active view that shows no panel says "Start by adding a panel", with a
// button that opens the picker too.
// Under the board lock, no command adds or removes a panel.
// So the dock draws no "+", no close button on a tab and no picker there, and
// an empty view says "This view is empty".
//
// While the views menu on the bar draws its line of tabs, under the bar's row,
// the dock's root starts below the line (./views-menu.tsx).

import type { Board, CoreApi, PluginClassId } from "@blockr/core";
import { DockviewDefaultTab, DockviewReact, themeLightSpaced } from "dockview-react";
import type {
  DockviewReadyEvent,
  IDockviewHeaderActionsProps,
  IDockviewPanelHeaderProps,
  IDockviewPanelProps,
  IDockviewReactProps,
} from "dockview-react";
import { useCallback, useEffect, useLayoutEffect, useMemo, useRef, useState } from "react";
import type { ReactElement } from "react";
import { activeView } from "../../commands/index.ts";
import { PANEL_COMPONENT, panelFromId } from "../../layout/index.ts";
import type { PanelParams } from "../../layout/index.ts";
import { gridPanels } from "../../state/index.ts";
import { BlockCard, createTooltips } from "./card/index.ts";
import { DockContext, giveFocus, useBoard, useDock } from "./context.ts";
import { drawDock } from "./drawing.ts";
import { LockIcon } from "./icons.tsx";
import type { Narrow } from "./narrow.ts";
import { PanelPicker, usePickerButton } from "./picker.tsx";
import type { Settings } from "./settings.ts";
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
    <PluginFrame plugin={panel.plugin} />
  );
}

// The frame of a plugin's panel, which holds the plugin's UI while dockview
// draws the panel.
// A plugin's UI may draw in a React root of its own, as core's Edit board
// plugin does.
// Were the dock to end the mount as soon as React drops the frame, this would
// happen:
// 1. React drops the frame while it commits the dock's root, and runs the
//    cleanup of the frame's effect.
// 2. The cleanup that mountPlugin() returned unmounts the plugin's root.
//    React puts that off until the commit is done.
// 3. That cleanup then empties the frame.
// 4. Once the commit is done, React unmounts the plugin's root, and fails to
//    find what it drew in the frame.
// So the dock ends the mount once the commit is done.
function PluginFrame({ plugin }: { readonly plugin: PluginClassId }): ReactElement {
  const { core } = useDock();
  const ref = useRef<HTMLDivElement>(null);
  useLayoutEffect(() => {
    const el = ref.current;
    if (el === null) return undefined;
    const end = core.mountPlugin(el, plugin);
    return () => {
      queueMicrotask(end);
    };
  }, [core, plugin]);
  return <div ref={ref} className="blockr-dock-panel" data-plugin={plugin} />;
}

const components: IDockviewReactProps["components"] = { [PANEL_COMPONENT]: PanelFrame };

// A panel's tab, which has no close button on a locked board.
function Tab(props: IDockviewPanelHeaderProps<PanelParams>): ReactElement {
  const { core } = useDock();
  const { panel } = props.params;
  const onPointerDown =
    "block" in panel
      ? () => {
          giveFocus(core, panel.block);
        }
      : undefined;
  return <DockviewDefaultTab {...props} hideClose={core.lock.board} onPointerDown={onPointerDown} />;
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
      <button type="button" className="blockr-dock-add" aria-label="Add panel" {...button}>
        <svg viewBox="0 0 16 16" width="16" height="16" aria-hidden="true" focusable="false">
          <path d="M8 3v10M3 8h10" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" />
        </svg>
      </button>
    </div>
  );
}

// Whether the active view shows no panel that the dock draws.
function showsNothing(board: Board, drawn: ReadonlyMap<PluginClassId, string>): boolean {
  const view = activeView(board);
  if (view === undefined) return false;
  return !gridPanels(view.grid).some((panel) => ("block" in panel ? Object.hasOwn(board.blocks, panel.block) : drawn.has(panel.plugin)));
}

// What the dock shows over an empty view.
function EmptyView(): ReactElement | null {
  const { core, drawn } = useDock();
  const board = useBoard(core);
  if (!showsNothing(board, drawn)) return null;
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
      <button type="button" className="blockr-dock-empty-add" {...button}>
        Add panel
      </button>
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
}

export function Dock({ core, plugin, settings, narrow, drawn, viewsMenu }: DockProps): ReactElement {
  const root = useRef<HTMLDivElement>(null);
  const stop = useRef<(() => void) | undefined>(undefined);
  const [tooltips] = useState(createTooltips);
  const shared = useMemo(() => ({ core, plugin, drawn, tooltips }), [core, plugin, drawn, tooltips]);
  const locked = core.lock.board;
  const tabs = useViewTabs(core) && viewsMenu;

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
      <div className="blockr-dock" ref={root} data-tabs={tabs ? "" : undefined}>
        {locked ? view : <PanelPicker>{view}</PanelPicker>}
      </div>
    </DockContext>
  );
}
