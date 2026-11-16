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
// The frame of a block's panel stays empty, until the dock draws the block's
// card in it.
//
// The header of each tab group holds a "+", which opens the panel picker
// (./picker.tsx).
// A rail's header holds none.
// An active view that shows no panel says "Start by adding a panel", with a
// button that opens the picker too.
// Under the board lock, no command adds or removes a panel.
// So the dock draws no "+", no close button on a tab and no picker there, and
// an empty view says "This view is empty".

import type { Board, CoreApi, PluginClassId } from "@blockr/core";
import { DockviewDefaultTab, DockviewReact, themeLightSpaced } from "dockview-react";
import type {
  DockviewReadyEvent,
  IDockviewHeaderActionsProps,
  IDockviewPanelHeaderProps,
  IDockviewPanelProps,
  IDockviewReactProps,
} from "dockview-react";
import { useCallback, useEffect, useLayoutEffect, useMemo, useRef } from "react";
import type { ReactElement } from "react";
import { activeView } from "../../commands/index.ts";
import { PANEL_COMPONENT, panelFromId } from "../../layout/index.ts";
import type { PanelParams } from "../../layout/index.ts";
import { gridPanels } from "../../state/index.ts";
import { DockContext, useBoard, useDock } from "./context.ts";
import { drawDock } from "./drawing.ts";
import type { Narrow } from "./narrow.ts";
import { PanelPicker, usePickerButton } from "./picker.tsx";
import type { Settings } from "./settings.ts";

// The frame of a panel, which names the block or the plugin whose panel it
// is.
function PanelFrame({ params }: IDockviewPanelProps<PanelParams>): ReactElement {
  const { panel } = params;
  return "block" in panel ? <div className="blockr-dock-panel" data-block={panel.block} /> : <PluginFrame plugin={panel.plugin} />;
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

// A tab without a close button, which a locked board shows.
function LockedTab(props: IDockviewPanelHeaderProps): ReactElement {
  return <DockviewDefaultTab {...props} hideClose />;
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
      <svg viewBox="0 0 16 16" width="32" height="32" aria-hidden="true" focusable="false" fill="currentColor">
        <path d="M8 1a3 3 0 0 0-3 3v3H4.5A1.5 1.5 0 0 0 3 8.5v5A1.5 1.5 0 0 0 4.5 15h7a1.5 1.5 0 0 0 1.5-1.5v-5A1.5 1.5 0 0 0 11.5 7H11V4a3 3 0 0 0-3-3Zm-1.75 6V4a1.75 1.75 0 1 1 3.5 0v3h-3.5Z" />
      </svg>
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
}

export function Dock({ core, plugin, settings, narrow, drawn }: DockProps): ReactElement {
  const root = useRef<HTMLDivElement>(null);
  const stop = useRef<(() => void) | undefined>(undefined);
  const shared = useMemo(() => ({ core, drawn }), [core, drawn]);
  const locked = core.lock.board;

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
        {...(locked ? { defaultTabComponent: LockedTab } : { leftHeaderActionsComponent: AddPanelButton })}
      />
      <EmptyView />
    </>
  );

  return (
    <DockContext value={shared}>
      <div className="blockr-dock" ref={root}>
        {locked ? view : <PanelPicker>{view}</PanelPicker>}
      </div>
    </DockContext>
  );
}
