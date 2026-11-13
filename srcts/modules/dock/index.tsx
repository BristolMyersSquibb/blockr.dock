// The module of the dock plugin's class, blockr.dock-dock_plugin, which
// blockr.dock lists in R with the class's id. Its default export is the class
// without its id, whose `frontend` core calls with the page's element
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.12#the-frontend).
// The frontend draws the page as a React app on the one copy of React that
// core puts on the page, with the layout drawn by dockview, which the module's
// bundle holds.
// The page is an empty dockview, which holds no panel. It takes dockview's
// light-spaced theme, or its abyss-spaced theme where the board option
// dark_mode is dark.
// The class requires five board options: the dock's own, compact, view_tabs
// and sync_rails, which ../../options defines, and core's show_conditions and
// dark_mode. So core gives each board that lacks a value for one of them its
// default as the board opens, before the dock draws it.
// The class keeps the dock's state, which ../../state defines.
// It gives core the schema of the state, which the build generates from the
// state's types, and the default of the state, which holds no view.
// It also gives core the `update` and the `migrate` of ../../state.
// R gives the class version 2, while today's files hold version 1.

import { DockviewReact, themeAbyssSpaced, themeLightSpaced } from "dockview-react";
import type { IDockviewReactProps } from "dockview-react";
import { useCallback, useSyncExternalStore } from "react";
import { flushSync } from "react-dom";
import { createRoot } from "react-dom/client";
import type { CoreApi, FrontendModule, JsonSchema, PluginClass } from "@blockr/core";
// The schema of the class's state is the file that the build writes for R,
// beside the bundle.
// eslint-disable-next-line no-restricted-imports
import schema from "../../../inst/js/dock.schema.json" with { type: "json" };
import { defaultState, migrate, update } from "../../state/index.ts";

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
  options: ["compact", "view_tabs", "sync_rails", "show_conditions", "dark_mode"],
  // TypeScript gives the objects in the JSON's arrays properties that may be
  // undefined, which no Json has, so it does not take the schema for a Json,
  // though it is one.
  state: { schema: schema as unknown as JsonSchema, default: defaultState },
  update,
  migrate,
  frontend,
} satisfies Omit<PluginClass, "id">;
