// The module `assistant` of blockr.dock, which tells blockr.assistant's model
// what the dock's state holds now.
// The package lists it in R, in the `modules` of its `._BLOCKR_MODULES_`, so
// that its id is blockr.dock-assistant.
// As each call of the model goes out, the assistant loads the module and calls
// describe() of its default export for each plugin of blockr.dock that the app
// runs.
// It passes the id of the plugin's class and the plugin's state, or null where
// the board holds none.
// For the dock's class, describe() returns these lines, the most useful first,
// since the assistant may cut them at its cap:
// 1. A line that names the active view, by its name and id, where a view is
//    active.
// 2. A line for each view, in the order of the navigation, with its name, its
//    id and the panels that its grid places: blocks by their ids, and plugins
//    by the ids of their classes.
//    A view that places no panel reads as empty.
// It returns no lines for another class.
// It returns none where the board holds no state for the dock either, since
// the default state holds no view.
// It throws where the state does not have the shape of the dock's state.
// The store checks the state against its schema after every change, so only a
// defect leaves such a state.
// The assistant then raises the error and leaves the lines out.
// The dock needs no dependency on blockr.assistant for the module, as for its
// skill `layout` in inst/assistant/skills.

import type { Json, PluginClassId } from "@blockr/core";
import { gridPanels, isDockState, viewOrder } from "../../state/index.ts";
import type { DockState, View, ViewId } from "../../state/index.ts";

// The id of the dock plugin's class.
// The module keeps its own copy of the DOCK of the dock's commands.
// Importing that one would bundle all of the commands' code into the module.
// The tests pass the commands' DOCK to describe(), so they fail where the two
// differ.
const DOCK = "blockr.dock-dock_plugin";

// The lines about the state of a plugin of the class given.
function describe(plugin: PluginClassId, state: Json): string[] {
  if (plugin !== DOCK || state === null) return [];
  if (!isDockState(state)) throw new Error("The dock's state does not have the shape that its schema gives.");
  const views = orderedViews(state);
  const active = views.find(([id]) => id === state.active);
  return [
    ...(active === undefined ? [] : [`The active view is ${active[1].name} (id: ${active[0]}).`]),
    ...views.map(([id, view]) => `- ${view.name} (id: ${id}): ${placed(view)}`),
  ];
}

// Each view of the state, with its id, in the order of the navigation.
function orderedViews(state: DockState): [ViewId, View][] {
  return viewOrder(state).flatMap((id) => {
    const view = state.views[id];
    return view === undefined ? [] : [[id, view]];
  });
}

// The panels that the view's grid places, in reading order, each by the id of
// its block or of its plugin's class, or "empty" where it places none.
function placed(view: View): string {
  const panels = gridPanels(view.grid).map((panel) => ("block" in panel ? panel.block : panel.plugin));
  return panels.length === 0 ? "empty" : panels.join(", ");
}

export default { describe };
