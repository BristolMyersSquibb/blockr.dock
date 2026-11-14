// The commands of the dock plugin's class, which ../../commands defines, run
// through blockr.core's command registry and store on core's CoreApi for
// tests, as menus and the assistant's tools run them.

import type { Json, JsonBoard } from "@blockr/core";
import { HELLO, stubCore } from "@blockr/core/stubs";
import { describe, expect, onTestFinished, test } from "vitest";
import { compare, gridPanels, inFolder, viewOrder } from "../../state/index.ts";
import type { DockState, Grid, Panel, Rail, TabGroup, View } from "../../state/index.ts";
import * as compactOption from "../compact-option/index.ts";
import * as syncRailsOption from "../sync-rails-option/index.ts";
import * as viewTabsOption from "../view-tabs-option/index.ts";
import * as dock from "./index.tsx";

const DOCK = "blockr.dock-dock_plugin";

const a = { block: "a" };
const b = { block: "b" };
const c = { block: "c" };
const dag = { plugin: "blockr.dag-dag_plugin" };
const md = { plugin: "blockr.md-md_plugin" };

function group(...panels: Panel[]): TabGroup {
  const [first] = panels;
  if (first === undefined) throw new Error("A tab group holds a panel.");
  return { panels, active: first };
}

function rail(...panels: Panel[]): Rail {
  return { ...group(...panels), collapsed: false, size: 260, collapsed_size: 35 };
}

const empty: Grid = { orientation: "horizontal", children: [], sizes: [] };

// The view of a state, which the test knows it holds.
function viewIn(dockState: DockState, id: string): View {
  const view = dockState.views[id];
  if (view === undefined) throw new Error(`The state holds no view "${id}".`);
  return view;
}

// The view Main shows a tab group of a and b, which shows b, beside c, with
// the DAG on a rail at its left edge. The folder Safety follows it at the top
// and holds the view Inner and the empty folder Nested, and the view Side,
// which shows c, comes last. The card of b shows its output alone.
const main: Grid = {
  orientation: "horizontal",
  children: [{ panels: [a, b], active: b }, group(c)],
  sizes: [0.5, 0.5],
  rails: { left: rail(dag) },
};

const state: DockState = {
  views: {
    main: { name: "Main", folder: null, index: "a0", grid: main },
    inner: {
      name: "Inner",
      folder: "f",
      index: "a0",
      grid: { orientation: "horizontal", children: [group(b)], sizes: [1], rails: { left: rail(dag) } },
    },
    side: { name: "Side", folder: null, index: "a2", grid: { orientation: "horizontal", children: [group(c)], sizes: [1] } },
  },
  folders: {
    f: { name: "Safety", folder: null, index: "a1" },
    g: { name: "Nested", folder: "f", index: "a1" },
  },
  active: "main",
  visible: { b: ["outputs"] },
};

// R's reply to hello in an app that runs the dock in place of core's basic
// frontend, with the dock's board options, on a board with three blocks and
// the dock's state given, under the board lock where `locked` says so.
function helloWith(dockState: DockState, locked: boolean) {
  const board: JsonBoard = {
    format: 1,
    packages: { "blockr.core": "0.2.0", "blockr.dock": "0.2.1" },
    board: {
      id: "curious_otter",
      versions: { [DOCK]: 1 },
      name: "Curious otter",
      blocks: {
        a: { registry_id: "blockr.core-dataset_block", version: 1, state: { dataset: "iris", package: "datasets" }, name: "Data", locked: null },
        b: { registry_id: "blockr.core-head_block", version: 1, state: { n: 3, direction: "head" }, name: "Head", locked: null },
        c: { registry_id: "blockr.core-head_block", version: 1, state: { n: 5, direction: "tail" }, name: "Tail", locked: null },
      },
      links: { ab: { from: "a", to: "b", input: "data" }, ac: { from: "a", to: "c", input: "data" } },
      stacks: {},
      options: {},
      plugins: { [DOCK]: dockState },
    },
  };
  return {
    ...HELLO,
    board,
    lock: { board: locked, load: false },
    packages: { ...HELLO.packages, "blockr.dock": "0.2.1" },
    plugins: {
      ...Object.fromEntries(Object.entries(HELLO.plugins).filter(([id]) => id !== "blockr.core-basic_frontend_plugin")),
      [DOCK]: { name: "Dock", version: 1, module: "blockr.dock/js/dock.js" },
    },
    options: {
      ...HELLO.options,
      compact: { package: "blockr.dock", module: "blockr.dock/compact-option.js", version: 1 },
      view_tabs: { package: "blockr.dock", module: "blockr.dock/view-tabs-option.js", version: 1 },
      sync_rails: { package: "blockr.dock", module: "blockr.dock/sync-rails-option.js", version: 1 },
    },
  };
}

const modules = { [DOCK]: dock, compact: compactOption, view_tabs: viewTabsOption, sync_rails: syncRailsOption };

// Starts core on a board that holds the dock's state given, the one above
// unless given, with the values of blockr options that R holds, none unless
// given.
async function start(
  options: { readonly dockState?: DockState; readonly locked?: boolean; readonly blockrOptions?: Readonly<Record<string, Json>> } = {},
) {
  const { core, settled, stop } = await stubCore({
    hello: helloWith(options.dockState ?? state, options.locked ?? false),
    modules,
    blockrOptions: options.blockrOptions ?? {},
  });
  onTestFinished(stop);
  await settled();
  return {
    core,
    // The dock's state on the board.
    dock: () => core.store.board().plugins[DOCK] as DockState,
    run: (name: string, args: Json = {}) => core.commands.run(`blockr.dock-${name}`, args),
    enabled: (name: string, args: Json = {}) => core.commands.enabled(`blockr.dock-${name}`, args),
  };
}

const IDS = [
  "add_view",
  "remove_view",
  "rename_view",
  "move_view",
  "activate_view",
  "add_folder",
  "remove_folder",
  "rename_folder",
  "move_folder",
  "add_panel",
  "remove_panel",
  "move_panel",
  "resize_panel",
  "activate_panel",
  "show_panel",
  "set_grid",
  "collapse_rail",
  "set_sections",
].map((name) => `blockr.dock-${name}`);

describe("the dock's commands", () => {
  test("are the 18 of the class, each with a label, a description and args, and three take no undo step", async () => {
    const { core } = await start();
    const registered = core.commands.registered();

    expect(Object.keys(registered).filter((id) => id.startsWith("blockr.dock-")).sort()).toEqual([...IDS].sort());
    for (const id of IDS) {
      expect(registered[id]?.label).not.toBe("");
      expect(registered[id]?.description).not.toBe("");
      expect(registered[id]?.args).toMatchObject({ type: "object", additionalProperties: false });
    }
    expect(IDS.filter((id) => registered[id]?.undo === false)).toEqual([
      "blockr.dock-activate_view",
      "blockr.dock-activate_panel",
      "blockr.dock-collapse_rail",
    ]);
  });

  test("carry the definitions of the state's schema that a panel and a grid refer to", async () => {
    const { core } = await start();
    const args = core.commands.registered()["blockr.dock-set_grid"]?.args;
    expect(Object.keys((args?.$defs ?? {}) as object)).toEqual(["BlockId", "Grid", "Node", "Panel", "PluginClassId", "Rail", "Split", "TabGroup"]);
    expect(core.commands.registered()["blockr.dock-remove_view"]?.args).not.toHaveProperty("$defs");
  });
});

describe("blockr.dock-add_view", () => {
  test('adds an empty view named "View N" at the end of the top, under a new id, and makes it active, which one undo takes back', async () => {
    const { core, dock, run } = await start();

    await run("add_view");
    const id = dock().active ?? "";
    expect(id).toMatch(/^[a-z]+_[a-z]+$/);
    expect(dock().views[id]).toEqual({ name: "View 4", folder: null, index: "a3", grid: empty });
    expect(viewOrder(dock()).at(-1)).toBe(id);

    core.store.undo();
    expect(dock()).toEqual(state);
  });

  test("places the view in a folder, before the view or folder that `before` names, with its name and grid", async () => {
    const { dock, run } = await start();
    const grid: Grid = { orientation: "vertical", children: [group(a)], sizes: [1] };

    await run("add_view", { name: "Labs", folder: "f", before: { folder: "g" }, grid });
    const id = dock().active ?? "";
    expect(dock().views[id]).toMatchObject({ name: "Labs", folder: "f", grid });
    expect(inFolder(dock(), "f")).toEqual([{ view: "inner" }, { view: id }, { folder: "g" }]);
  });

  test('names the view with the first "View N" after the views there are that no view has', async () => {
    const { dock, run } = await start({ dockState: { ...state, views: { ...state.views, side: { ...viewIn(state, "side"), name: "View 4" } } } });
    await run("add_view");
    expect(dock().views[dock().active ?? ""]?.name).toBe("View 5");
  });

  test("refuses a name of spaces alone, a folder that the state lacks, and a `before` in another folder", async () => {
    const { dock, run } = await start();
    await expect(run("add_view", { name: "  " })).rejects.toThrow("/name");
    await expect(run("add_view", { folder: "nope" })).rejects.toThrow("not enabled");
    await expect(run("add_view", { folder: "f", before: { view: "side" } })).rejects.toThrow("not enabled");
    expect(dock()).toEqual(state);
  });

  test("refuses a grid that fails its checks, or places a block that the board lacks", async () => {
    const { dock, run } = await start();
    const sizes: Grid = { orientation: "horizontal", children: [group(a), group(b)], sizes: [0.5, 0.6] };
    await expect(run("add_view", { grid: sizes })).rejects.toThrow('The shares of the split at "/" do not sum to 1.');
    const lacking: Grid = { orientation: "horizontal", children: [group({ block: "z" })], sizes: [1] };
    await expect(run("add_view", { grid: lacking })).rejects.toThrow('the block "z", which the board does not hold');
    expect(dock()).toEqual(state);
  });
});

describe("blockr.dock-remove_view", () => {
  test("removes a view and makes the first view of the navigation active where it was, which one undo takes back", async () => {
    const { core, dock, run } = await start();

    await run("remove_view", { view: "main" });
    expect(Object.keys(dock().views).sort()).toEqual(["inner", "side"]);
    expect(dock().active).toBe("inner");

    core.store.undo();
    expect(dock()).toEqual(state);
  });

  test("leaves the active view where it removes another", async () => {
    const { dock, run } = await start();
    await run("remove_view", { view: "side" });
    expect(dock().active).toBe("main");
  });

  test("refuses to remove the last view", async () => {
    const one: DockState = { ...state, views: { main: viewIn(state, "main") } };
    const { dock, enabled, run } = await start({ dockState: one });
    expect(enabled("remove_view", { view: "main" })).toBe(false);
    await expect(run("remove_view", { view: "main" })).rejects.toThrow("not enabled");
    expect(dock()).toEqual(one);
  });
});

describe("blockr.dock-rename_view", () => {
  test("renames a view, to the name of another view too, and refuses a name of spaces alone", async () => {
    const { dock, run } = await start();
    await run("rename_view", { view: "side", name: "Main" });
    expect(dock().views.side?.name).toBe("Main");
    await expect(run("rename_view", { view: "side", name: "\t" })).rejects.toThrow("/name");
    await expect(run("rename_view", { view: "nope", name: "New" })).rejects.toThrow("not enabled");
  });
});

describe("blockr.dock-move_view", () => {
  test("moves a view into a folder, before the view that `before` names, changing only its folder and index", async () => {
    const { core, dock, run } = await start();

    await run("move_view", { view: "side", folder: "f", before: { view: "inner" } });
    const moved = viewIn(dock(), "side");
    expect(moved).toEqual({ ...viewIn(state, "side"), folder: "f", index: moved.index });
    expect(compare(moved.index, "a0")).toBe(-1);
    expect({ ...dock(), views: { ...dock().views, side: state.views.side } }).toEqual(state);
    expect(viewOrder(dock())).toEqual(["main", "side", "inner"]);

    core.store.undo();
    expect(dock()).toEqual(state);
  });

  test("moves a view to the end of the top", async () => {
    const { dock, run } = await start();
    await run("move_view", { view: "inner", folder: null });
    expect(inFolder(dock(), null)).toEqual([{ view: "main" }, { folder: "f" }, { view: "side" }, { view: "inner" }]);
  });

  test("changes nothing where the view sits where it moves to", async () => {
    const { core, dock, run } = await start();
    await run("move_view", { view: "side", folder: null });
    await run("move_view", { view: "main", folder: null, before: { folder: "f" } });
    expect(dock()).toEqual(state);
    expect(core.store.canUndo()).toBe(false);
  });

  test("refuses a `before` that is the view itself, or that its folder does not hold", async () => {
    const { run } = await start();
    await expect(run("move_view", { view: "side", folder: null, before: { view: "side" } })).rejects.toThrow("not enabled");
    await expect(run("move_view", { view: "side", folder: null, before: { view: "inner" } })).rejects.toThrow("not enabled");
    await expect(run("move_view", { view: "side", folder: "nope" })).rejects.toThrow("not enabled");
  });
});

describe("blockr.dock-activate_view", () => {
  test("makes a view active without an undo step, so that undo takes back the edit before it", async () => {
    const { core, dock, run } = await start();

    await run("rename_view", { view: "main", name: "Overview" });
    await run("activate_view", { view: "side" });
    expect(dock().active).toBe("side");

    core.store.undo();
    expect(dock().views.main?.name).toBe("Main");
    expect(dock().active).toBe("side");
    expect(core.store.canUndo()).toBe(false);
  });

  test("refuses a view that the state lacks", async () => {
    const { run } = await start();
    await expect(run("activate_view", { view: "nope" })).rejects.toThrow("not enabled");
  });
});

describe("blockr.dock-add_folder", () => {
  test('adds a folder named "Folder N" at the end of the top, under a new id, which one undo takes back', async () => {
    const { core, dock, run } = await start();

    await run("add_folder");
    const [id] = Object.keys(dock().folders).filter((key) => !Object.hasOwn(state.folders, key));
    expect(dock().folders[id ?? ""]).toEqual({ name: "Folder 3", folder: null, index: "a3" });
    expect(dock().active).toBe("main");

    core.store.undo();
    expect(dock()).toEqual(state);
  });

  test("places the folder in a folder, before the view that `before` names, with its name", async () => {
    const { dock, run } = await start();
    await run("add_folder", { name: "Labs", folder: "f", before: { view: "inner" } });
    const [id = ""] = Object.keys(dock().folders).filter((key) => !Object.hasOwn(state.folders, key));
    expect(dock().folders[id]).toMatchObject({ name: "Labs", folder: "f" });
    expect(inFolder(dock(), "f")).toEqual([{ folder: id }, { view: "inner" }, { folder: "g" }]);
  });
});

describe("blockr.dock-remove_folder", () => {
  test("moves what the folder holds up into its place, in its order, which one undo takes back", async () => {
    const { core, dock, run } = await start();

    await run("remove_folder", { folder: "f" });
    expect(Object.keys(dock().folders)).toEqual(["g"]);
    expect(inFolder(dock(), null)).toEqual([{ view: "main" }, { view: "inner" }, { folder: "g" }, { view: "side" }]);
    expect(Object.keys(dock().views).sort()).toEqual(["inner", "main", "side"]);

    core.store.undo();
    expect(dock()).toEqual(state);
  });
});

describe("blockr.dock-rename_folder", () => {
  test("renames a folder, and refuses a name of spaces alone", async () => {
    const { dock, run } = await start();
    await run("rename_folder", { folder: "g", name: "Labs" });
    expect(dock().folders.g?.name).toBe("Labs");
    await expect(run("rename_folder", { folder: "g", name: "" })).rejects.toThrow("/name");
  });
});

describe("blockr.dock-move_folder", () => {
  test("moves a folder to the top, before the view that `before` names, with what it holds", async () => {
    const { dock, run } = await start();
    await run("move_folder", { folder: "f", into: null, before: { view: "main" } });
    expect(dock().folders.f?.folder).toBeNull();
    expect(viewOrder(dock())).toEqual(["inner", "main", "side"]);
  });

  test("refuses to move a folder into itself or into a folder inside it", async () => {
    const { dock, run } = await start();
    await expect(run("move_folder", { folder: "g", into: "g" })).rejects.toThrow("not enabled");
    await expect(run("move_folder", { folder: "f", into: "g" })).rejects.toThrow("not enabled");
    expect(dock()).toEqual(state);
  });
});

describe("the commands for views and folders on a locked board", () => {
  test("add, rename, move or remove no view or folder, while a reader still switches views", async () => {
    const { dock, enabled, run } = await start({ locked: true });

    for (const [name, args] of [
      ["add_view", {}],
      ["remove_view", { view: "side" }],
      ["rename_view", { view: "side", name: "New" }],
      ["move_view", { view: "side", folder: "f" }],
      ["add_folder", {}],
      ["remove_folder", { folder: "f" }],
      ["rename_folder", { folder: "f", name: "New" }],
      ["move_folder", { folder: "g", into: null }],
    ] as const) {
      expect(enabled(name, args)).toBe(false);
    }

    await run("activate_view", { view: "side" });
    expect(dock()).toEqual({ ...state, active: "side" });
  });
});

describe("blockr.dock-add_panel", () => {
  test("places a panel at the default place where nothing names a place, which one undo takes back", async () => {
    const { core, dock, run } = await start();

    await run("add_panel", { view: "inner", panel: c });
    expect(dock().views.inner?.grid.children).toEqual([{ panels: [b, c], active: c }]);

    core.store.undo();
    expect(dock()).toEqual(state);
  });

  test("places a panel beside the tab group of the panel that `near` names, or on a rail", async () => {
    const { dock, run } = await start();

    await run("add_panel", { view: "main", panel: md, near: c, side: "below" });
    expect(dock().views.main?.grid.children[1]).toEqual({ children: [group(c), group(md)], sizes: [0.5, 0.5] });

    await run("add_panel", { view: "side", panel: dag, rail: "right" });
    expect(dock().views.side?.grid.rails).toEqual({ right: rail(dag) });
  });

  test("evens out the split that takes a new tab group, as dockview's addPanel() does", async () => {
    const { dock, run } = await start();
    await run("add_panel", { view: "main", panel: md, near: c, side: "right" });
    expect(dock().views.main?.grid).toEqual({ ...main, children: [...main.children, group(md)], sizes: [1 / 3, 1 / 3, 1 / 3] });
  });

  test("refuses a panel that the view shows, a block that the board lacks, and a `near` that the view does not show", async () => {
    const { dock, enabled, run } = await start();
    expect(enabled("add_panel", { view: "main", panel: a })).toBe(false);
    expect(enabled("add_panel", { view: "main", panel: { block: "z" } })).toBe(false);
    await expect(run("add_panel", { view: "inner", panel: a, near: c })).rejects.toThrow(
      'The view does not show the panel of the block "c", which near names.',
    );
    await expect(run("add_panel", { view: "main", panel: md, side: "beside" })).rejects.toThrow("/side");
    expect(dock()).toEqual(state);
  });
});

describe("blockr.dock-remove_panel", () => {
  test("takes a panel out of a view, with the tab group or rail that it empties, which one undo takes back", async () => {
    const { core, dock, run } = await start();

    await run("remove_panel", { view: "main", panel: c });
    await run("remove_panel", { view: "main", panel: dag });
    expect(dock().views.main?.grid).toEqual({ orientation: "horizontal", children: [{ panels: [a, b], active: b }], sizes: [1] });
    expect(dock().views.inner).toEqual(state.views.inner);

    core.store.undo();
    core.store.undo();
    expect(dock()).toEqual(state);
  });

  test("refuses a panel that the view does not show", async () => {
    const { run } = await start();
    await expect(run("remove_panel", { view: "side", panel: a })).rejects.toThrow("not enabled");
  });
});

describe("blockr.dock-move_panel", () => {
  test("moves a panel into the tab group of the panel that `near` names, or onto another rail", async () => {
    const { dock, run } = await start();

    await run("move_panel", { view: "main", panel: c, near: a });
    await run("move_panel", { view: "main", panel: dag, rail: "right" });
    expect(dock().views.main?.grid).toEqual({
      orientation: "horizontal",
      children: [{ panels: [a, b, c], active: c }],
      sizes: [1],
      rails: { right: rail(dag) },
    });
  });

  test("halves the tab group that a panel moves beside, as dockview's moveTo() and a drag do", async () => {
    const { dock, run } = await start();
    await run("move_panel", { view: "main", panel: a, near: c, side: "right" });
    expect(dock().views.main?.grid).toEqual({ ...main, children: [group(b), group(c), group(a)], sizes: [0.5, 0.25, 0.25] });
  });

  test("changes nothing where a panel alone on a rail moves to that rail, whose widths stay", async () => {
    const wide: DockState = {
      ...state,
      views: { ...state.views, main: { ...viewIn(state, "main"), grid: { ...main, rails: { left: { ...rail(dag), size: 400 } } } } },
    };
    const { core, dock, run } = await start({ dockState: wide });
    await run("move_panel", { view: "main", panel: dag, rail: "left" });
    expect(dock()).toEqual(wide);
    expect(core.store.canUndo()).toBe(false);
  });

  test("refuses a panel that the view does not show, and a `near` that is the panel itself", async () => {
    const { enabled } = await start();
    expect(enabled("move_panel", { view: "side", panel: a })).toBe(false);
    expect(enabled("move_panel", { view: "main", panel: a, near: a })).toBe(false);
  });
});

describe("blockr.dock-resize_panel", () => {
  test("sets the share of the tab group that holds a panel, or the width of its rail", async () => {
    const { dock, run } = await start();

    await run("resize_panel", { view: "main", panel: c, size: 0.25 });
    await run("resize_panel", { view: "main", panel: dag, size: 300 });
    expect(dock().views.main?.grid).toMatchObject({ sizes: [0.75, 0.25], rails: { left: { size: 300 } } });
  });

  test("refuses a share that is not below 1, and a tab group that fills its split alone", async () => {
    const { dock, run } = await start();
    await expect(run("resize_panel", { view: "main", panel: c, size: 1.5 })).rejects.toThrow("below 1");
    await expect(run("resize_panel", { view: "side", panel: c, size: 0.5 })).rejects.toThrow("fills its split alone");
    await expect(run("resize_panel", { view: "main", panel: c, size: 0 })).rejects.toThrow("/size");
    expect(dock()).toEqual(state);
  });
});

describe("blockr.dock-activate_panel", () => {
  test("has the tab group that holds a panel show it, without an undo step", async () => {
    const { core, dock, run } = await start();
    await run("activate_panel", { view: "main", panel: a });
    expect(dock().views.main?.grid.children[0]).toEqual({ panels: [a, b], active: a });
    expect(core.store.canUndo()).toBe(false);
  });
});

describe("blockr.dock-show_panel", () => {
  test("has the active view show a panel that it holds, and places one that it lacks at the default place", async () => {
    const { dock, run } = await start();

    await run("show_panel", { panel: a });
    expect(dock().views.main?.grid.children[0]).toEqual({ panels: [a, b], active: a });

    await run("show_panel", { panel: md });
    expect(dock().views.main?.grid.children[1]).toEqual({ panels: [c, md], active: md });
  });

  test("on a locked board, only has the active view show a panel that it holds", async () => {
    const { dock, enabled, run } = await start({ locked: true });
    expect(enabled("show_panel", { panel: md })).toBe(false);
    await run("show_panel", { panel: a });
    expect(dock().views.main?.grid.children[0]).toEqual({ panels: [a, b], active: a });
  });
});

describe("blockr.dock-set_grid", () => {
  test("sets a view's whole grid, which one undo takes back", async () => {
    const { core, dock, run } = await start();
    const grid: Grid = { orientation: "vertical", children: [group(c), group(a)], sizes: [0.3, 0.7] };

    await run("set_grid", { view: "side", grid });
    expect(dock().views.side?.grid).toEqual(grid);

    core.store.undo();
    expect(dock()).toEqual(state);
  });

  test("refuses a grid that fails its checks, or places a block that the board lacks", async () => {
    const { dock, run } = await start();
    const twice: Grid = { orientation: "horizontal", children: [group(a), group(a)], sizes: [0.5, 0.5] };
    await expect(run("set_grid", { view: "side", grid: twice })).rejects.toThrow('places the panel of the block "a" more than once');
    const lacking: Grid = { orientation: "horizontal", children: [group({ block: "z" })], sizes: [1] };
    await expect(run("set_grid", { view: "side", grid: lacking })).rejects.toThrow("which the board does not hold");
    await expect(run("set_grid", { view: "side", grid: { ...lacking, orientation: "diagonal" } })).rejects.toThrow("/grid");
    expect(dock()).toEqual(state);
  });

  test("keeps the shares of a split that the grid moves by no more than the blockr option dock_grid_size_tol", async () => {
    const { dock, run } = await start();
    await run("set_grid", { view: "main", grid: { ...main, sizes: [0.502, 0.498] } });
    expect(dock()).toEqual(state);

    const jittered: Grid = { ...main, children: [{ panels: [a, b], active: a }, group(c)], sizes: [0.503, 0.497] };
    await run("set_grid", { view: "main", grid: jittered });
    expect(dock().views.main?.grid).toEqual({ ...jittered, sizes: [0.5, 0.5] });
    const dragged: Grid = { ...jittered, sizes: [0.51, 0.49] };
    await run("set_grid", { view: "main", grid: dragged });
    expect(dock().views.main?.grid).toEqual(dragged);
  });

  test("reads dock_grid_size_tol as a number of 0 or more, and from the string that an environment variable sets", async () => {
    const jittered: Grid = { ...main, children: [{ panels: [a, b], active: a }, group(c)], sizes: [0.503, 0.497] };

    const loose = await start({ blockrOptions: { dock_grid_size_tol: "0.02" } });
    await loose.run("set_grid", { view: "main", grid: { ...jittered, sizes: [0.51, 0.49] } });
    expect(loose.dock().views.main?.grid.sizes).toEqual([0.5, 0.5]);

    const exact = await start({ blockrOptions: { dock_grid_size_tol: 0 } });
    await exact.run("set_grid", { view: "main", grid: jittered });
    expect(exact.dock().views.main?.grid.sizes).toEqual([0.503, 0.497]);

    for (const value of ["small", -0.01, null]) {
      const { dock, run } = await start({ blockrOptions: { dock_grid_size_tol: value } });
      await expect(run("set_grid", { view: "main", grid: jittered })).rejects.toThrow(
        `The blockr option dock_grid_size_tol is ${JSON.stringify(value)}, where it takes a number of 0 or more.`,
      );
      expect(dock()).toEqual(state);
    }
  });
});

describe("blockr.dock-collapse_rail", () => {
  test("collapses the rail on an edge of every view that has one while sync_rails is on, without an undo step", async () => {
    const { core, dock, run } = await start();

    await run("collapse_rail", { view: "main", edge: "left", collapsed: true });
    expect(dock().views.main?.grid.rails?.left?.collapsed).toBe(true);
    expect(dock().views.inner?.grid.rails?.left?.collapsed).toBe(true);
    expect(dock().views.side).toEqual(state.views.side);
    expect(core.store.canUndo()).toBe(false);

    await run("collapse_rail", { view: "inner", edge: "left", collapsed: false });
    expect(dock()).toEqual(state);
  });

  test("collapses the rail of the view alone while sync_rails is off", async () => {
    const { core, dock, run } = await start();
    await core.commands.run("blockr.core-set_option", { id: "sync_rails", value: false });

    await run("collapse_rail", { view: "main", edge: "left", collapsed: true });
    expect(dock().views.main?.grid.rails?.left?.collapsed).toBe(true);
    expect(dock().views.inner?.grid.rails?.left?.collapsed).toBe(false);
  });

  test("refuses a view without a rail on the edge", async () => {
    const { run } = await start();
    await expect(run("collapse_rail", { view: "side", edge: "left", collapsed: true })).rejects.toThrow("not enabled");
  });
});

describe("the commands for panels and rails on a locked board", () => {
  test("add and remove no panel, while a reader still has a tab group show another panel and collapses a rail", async () => {
    const { dock, enabled, run } = await start({ locked: true });

    expect(enabled("add_panel", { view: "main", panel: md })).toBe(false);
    expect(enabled("remove_panel", { view: "main", panel: c })).toBe(false);

    await run("activate_panel", { view: "main", panel: a });
    await run("collapse_rail", { view: "main", edge: "left", collapsed: true });
    expect(dock().views.main?.grid).toMatchObject({ children: [{ active: a }, group(c)], rails: { left: { collapsed: true } } });
  });

  test("move or resize no panel unless lock_rearrange or lock_resize leaves that open", async () => {
    const locked = await start({ locked: true });
    await expect(locked.run("move_panel", { view: "main", panel: c, near: a })).rejects.toThrow("lock_rearrange");
    await expect(locked.run("resize_panel", { view: "main", panel: c, size: 0.25 })).rejects.toThrow("lock_resize");
    expect(locked.dock()).toEqual(state);

    const open = await start({ locked: true, blockrOptions: { lock_rearrange: "FALSE", lock_resize: false } });
    await open.run("move_panel", { view: "main", panel: c, near: a });
    await open.run("resize_panel", { view: "inner", panel: dag, size: 300 });
    expect(gridPanels(open.dock().views.main?.grid ?? empty)).toEqual([a, b, c, dag]);
    expect(open.dock().views.inner?.grid.rails?.left?.size).toBe(300);
  });

  test("set no grid that holds other panels, under either blockr option", async () => {
    const { run } = await start({ locked: true, blockrOptions: { lock_rearrange: false, lock_resize: false } });
    const fewer: Grid = { ...main, children: [{ panels: [a, b], active: b }], sizes: [1] };
    await expect(run("set_grid", { view: "main", grid: fewer })).rejects.toThrow("keeps the panels that each view shows");
  });

  test("set a grid that changes only the panels shown and the rails' collapse, under both blockr options", async () => {
    const { dock, run } = await start({ locked: true });
    const grid: Grid = { ...main, children: [{ panels: [a, b], active: a }, group(c)], rails: { left: { ...rail(dag), collapsed: true } } };
    await run("set_grid", { view: "main", grid });
    expect(dock().views.main?.grid).toEqual(grid);
  });

  test("set a grid that rearranges or resizes only where lock_rearrange or lock_resize leaves that open", async () => {
    const rearranged: Grid = { ...main, children: [group(c), { panels: [a, b], active: b }] };
    const resized: Grid = { ...main, sizes: [0.4, 0.6] };
    // Moving c below a and b gives the top split another shape, whose shares
    // are not compared.
    const reshaped: Grid = { ...main, children: [{ children: [{ panels: [a, b], active: b }, group(c)], sizes: [0.7, 0.3] }], sizes: [1] };

    const locked = await start({ locked: true });
    await expect(locked.run("set_grid", { view: "main", grid: rearranged })).rejects.toThrow("lock_rearrange");
    await expect(locked.run("set_grid", { view: "main", grid: resized })).rejects.toThrow("lock_resize");

    const rearrange = await start({ locked: true, blockrOptions: { lock_rearrange: "F" } });
    await rearrange.run("set_grid", { view: "main", grid: rearranged });
    await rearrange.run("set_grid", { view: "main", grid: reshaped });
    expect(rearrange.dock().views.main?.grid).toEqual(reshaped);
    await expect(rearrange.run("set_grid", { view: "main", grid: { ...reshaped, children: [{ children: [{ panels: [a, b], active: b }, group(c)], sizes: [0.5, 0.5] }] } })).rejects.toThrow("lock_resize");

    const resize = await start({ locked: true, blockrOptions: { lock_resize: "false" } });
    await resize.run("set_grid", { view: "main", grid: resized });
    expect(resize.dock().views.main?.grid).toEqual(resized);
  });

  test("set a grid that moves a tab under lock_resize, keeping the shares that it moves by no more than dock_grid_size_tol", async () => {
    const { dock, run } = await start({ locked: true, blockrOptions: { lock_rearrange: false } });
    // Moving b into the tab group of c keeps the splits and tab groups, whose
    // shares are compared.
    const moved: Grid = { ...main, children: [group(a), { panels: [b, c], active: b }], sizes: [0.497, 0.503] };
    await run("set_grid", { view: "main", grid: moved });
    expect(dock().views.main?.grid).toEqual({ ...moved, sizes: [0.5, 0.5] });
    await expect(run("set_grid", { view: "main", grid: { ...moved, sizes: [0.49, 0.51] } })).rejects.toThrow("lock_resize");
  });

  test("refuse to move a panel where a blockr option of the lock holds no logical", async () => {
    const { run } = await start({ locked: true, blockrOptions: { lock_rearrange: "maybe" } });
    await expect(run("move_panel", { view: "main", panel: c, near: a })).rejects.toThrow(
      'The blockr option lock_rearrange is "maybe", where it takes TRUE or FALSE.',
    );
  });
});

describe("blockr.dock-set_sections", () => {
  test("sets the sections of a block's card, and removes its entry where both show, which one undo takes back", async () => {
    const { core, dock, run } = await start();

    await run("set_sections", { block: "a", sections: ["inputs"] });
    await run("set_sections", { block: "b", sections: ["outputs", "inputs"] });
    await run("set_sections", { block: "c", sections: [] });
    expect(dock().visible).toEqual({ a: ["inputs"], c: [] });

    core.store.undo();
    core.store.undo();
    core.store.undo();
    expect(dock()).toEqual(state);
  });

  test("refuses a section twice, a block that the board lacks, and a locked board", async () => {
    const { enabled, run } = await start();
    await expect(run("set_sections", { block: "a", sections: ["inputs", "inputs"] })).rejects.toThrow("/sections");
    expect(enabled("set_sections", { block: "z", sections: [] })).toBe(false);

    const locked = await start({ locked: true });
    expect(locked.enabled("set_sections", { block: "a", sections: [] })).toBe(false);
  });
});
