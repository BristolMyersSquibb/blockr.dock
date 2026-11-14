// The dock's commands, which the dock plugin's class gives core to register
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.1#commands).
// They are the only way that the dock changes its state, so that undo, saving
// and other plugins see each change, and no change of arrangement reaches R.
// Each reads the dock's state on the board and returns one delta of patches
// inside it, which the store checks against the state's schema, and each is
// one undo step but blockr.dock-activate_view, blockr.dock-activate_panel and
// blockr.dock-collapse_rail, which change only what the user looks at.
// Their descriptions and the schemas of their args describe them to menus and
// to the assistant's tools.
// The store applies a patch inside a plugin's state on a locked board and
// leaves its lock to the plugin's commands, so the dock's commands lock what
// the dock alone keeps
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.1#on-a-locked-board).

import type { Command, Json } from "@blockr/core";
import { panelCommands } from "./panels.ts";
import { sectionCommands } from "./sections.ts";
import { viewCommands } from "./views.ts";

export { DOCK, gridSizeTolerance, layoutLocks, stateOf } from "./common.ts";
export type { LayoutLocks } from "./common.ts";

export const dockCommands: readonly Command<Json, unknown>[] = [...viewCommands, ...panelCommands, ...sectionCommands];
