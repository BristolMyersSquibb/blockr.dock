// The dock's React app, which draws dockview in the page's element and hands
// dockview's api to ./drawing.ts as dockview is ready.
// Dockview takes its light-spaced theme, as today's dock does, under the
// stylesheet's bridge from blockr.ui's tokens to dockview's variables, which
// follows the board option dark_mode (./index.css).
// Each panel holds an empty frame, until the dock draws a block's card and
// mounts a plugin's UI in it.
// Under the board lock, no tab has a close button, since no command adds or
// removes a panel there.

import type { CoreApi, PluginClassId } from "@blockr/core";
import { DockviewDefaultTab, DockviewReact, themeLightSpaced } from "dockview-react";
import type { DockviewReadyEvent, IDockviewPanelHeaderProps, IDockviewPanelProps, IDockviewReactProps } from "dockview-react";
import { useCallback, useEffect, useRef } from "react";
import type { ReactElement } from "react";
import { PANEL_COMPONENT } from "../../layout/index.ts";
import type { PanelParams } from "../../layout/index.ts";
import { drawDock } from "./drawing.ts";
import type { Narrow } from "./narrow.ts";
import type { Settings } from "./settings.ts";

// The frame of a panel, which names the block or the plugin whose panel it
// is.
function PanelFrame({ params }: IDockviewPanelProps<PanelParams>): ReactElement {
  const { panel } = params;
  return "block" in panel ? (
    <div className="blockr-dock-panel" data-block={panel.block} />
  ) : (
    <div className="blockr-dock-panel" data-plugin={panel.plugin} />
  );
}

const components: IDockviewReactProps["components"] = { [PANEL_COMPONENT]: PanelFrame };

// A tab without a close button, which a locked board shows.
function LockedTab(props: IDockviewPanelHeaderProps): ReactElement {
  return <DockviewDefaultTab {...props} hideClose />;
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

  return (
    <div className="blockr-dock" ref={root}>
      <DockviewReact
        components={components}
        onReady={onReady}
        theme={themeLightSpaced}
        {...(core.lock.board ? { defaultTabComponent: LockedTab } : {})}
      />
    </div>
  );
}
