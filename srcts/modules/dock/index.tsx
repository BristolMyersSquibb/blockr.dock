// The module of the dock plugin's class, blockr.dock-dock_plugin, which
// blockr.dock lists in R with the class's id. Its default export is the class
// without its id, whose `frontend` core calls with the page's element
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.12#the-frontend).
// The frontend draws the page as a React app on the one copy of React that
// core puts on the page, with the layout drawn by dockview, which the module's
// bundle holds.
// The class requires five board options: the dock's own, compact, view_tabs
// and sync_rails, which ../../options defines, and core's show_conditions and
// dark_mode. So core gives each board that lacks a value for one of them its
// default as the board opens, before the dock draws it.
// The class keeps the dock's state, which ../../state defines.
// It gives core the schema of the state, which the build generates from the
// state's types, and the default of the state, which holds no view.
// It also gives core the `update` of ../../state, and the dock's commands,
// which ../../commands defines and core registers with the class.
//
// The frontend's render() takes these steps, which core awaits before it
// starts the scheduler
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#starting-the-page):
// 1. It reads the blockr options that the dock reads as it draws
//    (./settings.ts).
// 2. It takes the step on starting, as one delta of the dock plugin's class
//    that undo does not take back (./start.ts).
// 3. It demands the blocks whose cards are on screen, so that the scheduler
//    evaluates the blocks of the opening view first (./demand.ts).
// 4. It draws the page, and the active view in it, with dockview
//    (./dock.tsx, ./drawing.ts).
// The cleanup that render() returns undoes all of them but the step on
// starting, which is the board's.

import { flushSync } from "react-dom";
import { createRoot } from "react-dom/client";
import type { FrontendModule, JsonSchema, PluginClass } from "@blockr/core";
// The schema of the class's state is the file that the build writes for R,
// beside the bundle.
// eslint-disable-next-line no-restricted-imports
import schema from "../../../inst/js/dock.schema.json" with { type: "json" };
import { dockCommands } from "../../commands/index.ts";
import { defaultState, update } from "../../state/index.ts";
import { demandShown } from "./demand.ts";
import { Dock } from "./dock.tsx";
import { trackNarrow } from "./narrow.ts";
import { readSettings } from "./settings.ts";
import { drawnPlugins, startOnBoard, urlView } from "./start.ts";

export const api = 1;

const frontend = {
  async render({ el, core, plugin, signal }) {
    const settings = await readSettings(core);
    if (signal.aborted) return undefined;

    const win = el.ownerDocument.defaultView ?? window;
    const drawn = drawnPlugins(core);
    startOnBoard(core, plugin, [...drawn.keys()], urlView(win.location));

    const narrow = trackNarrow(win, settings.breakpoint);
    const release = demandShown(core, drawn, narrow);

    const root = createRoot(el);
    // React draws the page before render() returns.
    flushSync(() => {
      root.render(<Dock core={core} plugin={plugin} settings={settings} narrow={narrow} drawn={drawn} />);
    });
    return () => {
      root.unmount();
      release();
      narrow.stop();
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
  commands: dockCommands,
  frontend,
} satisfies Omit<PluginClass, "id">;
