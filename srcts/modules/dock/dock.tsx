// The dock's React app, which draws dockview in the page's element and hands
// dockview's api to ./drawing.ts as dockview is ready.
// Dockview takes its light-spaced theme, as today's dock does, under the
// stylesheet's bridge from blockr.ui's tokens to dockview's variables, which
// follows the board option dark_mode (./index.css).
// A block's panel holds the block's card (./card), and a plugin's panel an
// empty frame, until the dock mounts the plugin's UI in it.
// The cards draw their menus and tooltips in a layer at the end of the dock's
// root, outside dockview's panels, which clip what overflows them.
// A click on a block's tab gives the block focus in CoreApi.focus, as a click
// into its card does, while a click on a plugin's tab leaves the focus where
// it is.
// Under the board lock, no tab has a close button, since no command adds or
// removes a panel there.

import type { CoreApi, PluginClassId } from "@blockr/core";
import { DockviewDefaultTab, DockviewReact, themeLightSpaced } from "dockview-react";
import type { DockviewReadyEvent, IDockviewPanelHeaderProps, IDockviewPanelProps, IDockviewReactProps } from "dockview-react";
import { useCallback, useEffect, useMemo, useRef, useState } from "react";
import type { ReactElement } from "react";
import { PANEL_COMPONENT } from "../../layout/index.ts";
import type { PanelParams } from "../../layout/index.ts";
import { BlockCard, CardScope, createTooltips, giveFocus, useScope } from "./card/index.ts";
import { drawDock } from "./drawing.ts";
import type { Narrow } from "./narrow.ts";
import type { Settings } from "./settings.ts";

// The frame of a panel, which names the block or the plugin whose panel it
// is, and holds a block's card.
function PanelFrame({ params }: IDockviewPanelProps<PanelParams>): ReactElement {
  const { panel } = params;
  return "block" in panel ? (
    <div className="blockr-dock-panel" data-block={panel.block}>
      <BlockCard id={panel.block} />
    </div>
  ) : (
    <div className="blockr-dock-panel" data-plugin={panel.plugin} />
  );
}

const components: IDockviewReactProps["components"] = { [PANEL_COMPONENT]: PanelFrame };

// A panel's tab, which has no close button on a locked board.
function Tab(props: IDockviewPanelHeaderProps<PanelParams>): ReactElement {
  const { core } = useScope();
  const { panel } = props.params;
  const onPointerDown =
    "block" in panel
      ? () => {
          giveFocus(core, panel.block);
        }
      : undefined;
  return <DockviewDefaultTab {...props} hideClose={core.lock.board} onPointerDown={onPointerDown} />;
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
  const [layer, setLayer] = useState<HTMLDivElement | null>(null);
  const [tooltips] = useState(createTooltips);
  const scope = useMemo(() => ({ core, plugin, layer, tooltips }), [core, plugin, layer, tooltips]);

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
    <CardScope value={scope}>
      <div className="blockr-dock" ref={root}>
        <DockviewReact components={components} defaultTabComponent={Tab} onReady={onReady} theme={themeLightSpaced} />
        <div className="blockr-dock-layer" ref={setLayer} />
      </div>
    </CardScope>
  );
}
