// The module of the dock plugin's class, blockr.dock-dock_plugin, which
// blockr.dock lists in R with the class's id. Its default export is the class
// without its id, whose `frontend` core calls with the page's element
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.12#the-frontend).
// The frontend draws the page as a React app on the one copy of React that
// core puts on the page, with the layout drawn by dockview, which the module's
// bundle holds.
// The page is an empty dockview, which holds no panel. It takes dockview's
// light-spaced theme, or its abyss-spaced theme where the board option
// dark_mode is dark, which is why the class requires dark_mode.

import { DockviewReact, themeAbyssSpaced, themeLightSpaced } from "dockview-react";
import type { IDockviewReactProps } from "dockview-react";
import { useCallback, useSyncExternalStore } from "react";
import { flushSync } from "react-dom";
import { createRoot } from "react-dom/client";
import type { CoreApi, FrontendModule, PluginClass } from "@blockr/core";

export const api = 1;

// The components that draw the content of panels, by name, of which dockview
// needs one for each panel it draws.
const panels: IDockviewReactProps["components"] = {};

// Whether the board option dark_mode is dark. A change to the board, such as
// one that sets the board option, can switch it.
function useDark(core: CoreApi): boolean {
  const subscribe = useCallback((changed: () => void) => core.store.subscribe(changed), [core]);
  return useSyncExternalStore(subscribe, () => core.options.value("dark_mode") === "dark");
}

function Dock({ core }: { core: CoreApi }) {
  const theme = useDark(core) ? themeAbyssSpaced : themeLightSpaced;
  return (
    <div className="blockr-dock">
      <DockviewReact components={panels} onReady={() => undefined} theme={theme} />
    </div>
  );
}

const frontend = {
  render({ el, core }) {
    const root = createRoot(el);
    // React draws the page before render() returns.
    flushSync(() => {
      root.render(<Dock core={core} />);
    });
    return () => {
      root.unmount();
    };
  },
} satisfies FrontendModule;

export default {
  options: ["dark_mode"],
  frontend,
} satisfies Omit<PluginClass, "id">;
