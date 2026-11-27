// What the dock's components share through React's context: CoreApi, the id
// of the dock plugin's class, and the app's plugins whose panels the dock
// draws, each fixed for one opening of a board, and the tooltips of the cards
// (./card).
// Dockview draws the content of each panel and the header of each tab group
// in a portal of the dock's React tree, so they read the context of the dock's
// root as well.
// The module also holds how the dock runs one of its commands, or a batch of
// them, as an edit, how it gives a block focus, and how it makes the id of the
// plugin whose panel its state names.

import type { BlockId, CommandCall, CommandId, CoreApi, Json, PluginClassId } from "@blockr/core";
import { createContext, useContext } from "react";
import type { IdRecord } from "../../state/index.ts";

export interface DockContextValue {
  readonly core: CoreApi;
  // The id of the dock plugin's class, which raises the error of a failed copy
  // of a block's id (./card/header.tsx).
  readonly plugin: PluginClassId;
  // The app's plugins whose class has a `ui`, each with its name, in the order
  // of the app's plugins.
  readonly drawn: ReadonlyMap<PluginClassId, string>;
  readonly tooltips: Tooltips;
}

// The tooltips of the dock, of which one shows at a time, each by the
// function that hides it (./card/tooltip.tsx).
export interface Tooltips {
  // A tooltip shows, which hides the one on screen.
  showing(hide: () => void): void;
  // A tooltip left the screen.
  left(): void;
  // Whether a tooltip left the screen less than 400ms ago, so that the next
  // shows at once.
  warm(): boolean;
}

export const DockContext = createContext<DockContextValue | null>(null);

export function useDock(): DockContextValue {
  const value = useContext(DockContext);
  if (value === null) throw new Error("A component of the dock is drawn outside the dock.");
  return value;
}

// Commands of the board, each by its id, as core.id() makes it, with its
// args, which run in order as one step.
export type Batch = readonly CommandCall[];

// Runs one of the board's commands as an edit of the dock, or the commands of
// a batch.
// Where the board refuses the edit, core tells the user why, as an error of
// the code board_edit_failed.
export function runEdit(core: CoreApi, id: CommandId, args: Json): void;
export function runEdit(core: CoreApi, batch: Batch): void;
export function runEdit(core: CoreApi, edit: CommandId | Batch, args?: Json): void {
  const running = isBatch(edit) ? core.commands.run(edit) : core.commands.run(edit, args);
  running.catch(() => {
    // Core tells the user why the board refused the edit.
  });
}

function isBatch(edit: CommandId | Batch): edit is Batch {
  return Array.isArray(edit);
}

// The id of the class of the plugin whose panel the dock's state names, as
// core makes it of the record that the state holds.
// So === finds it among the ids that core gives, as `drawn` holds them, and
// core finds the plugin by it.
export function panelClass(core: Pick<CoreApi, "id">, panel: { readonly plugin: IdRecord }): PluginClassId {
  return core.id(panel.plugin.package, panel.plugin.id);
}

// Gives a block focus in CoreApi.focus, unless it has it.
export function giveFocus(core: CoreApi, block: BlockId): void {
  if (core.focus.get() !== block) core.focus.set(block);
}
