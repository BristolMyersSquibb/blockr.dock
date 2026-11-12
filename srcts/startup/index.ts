// The dock's startup module, which core initializes as each board opens,
// before the store opens the board
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.7#startup-modules).
// It registers the class of the dock plugin, whose module blockr.dock-dock
// draws the page as the app's frontend
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.12#the-frontend).
// The class keeps no state, so a board holds nothing for the dock.

import type { CoreApi, PluginClass } from "@blockr/core";

export const api = 1;

// The class of the plugin that `new_dock_plugin()` makes in R, by its id.
const dockPlugin: PluginClass = {
  id: "blockr.dock-dock_plugin",
  frontend: "blockr.dock-dock",
};

export default {
  initialize({ core }: { core: CoreApi; signal: AbortSignal }): void {
    core.plugins.register(dockPlugin);
  },
};
