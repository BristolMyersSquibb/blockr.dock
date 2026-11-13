// The `update` of the dock plugin's class, called as the store calls it, with
// a delta and the board before the delta.

import type { Block, Board, Delta, Patch } from "@blockr/core";
import { describe, expect, test } from "vitest";
import { inFolder } from "./navigation.ts";
import { patched, pointer } from "./patch.ts";
import { defaultState, isDockState } from "./state.ts";
import type { DockState, Folder, FolderId, Grid, Panel, TabGroup, View } from "./types.ts";
import { update } from "./update.ts";

const DOCK = "blockr.dock-dock_plugin";

const a = { block: "a" };
const b = { block: "b" };
const c = { block: "c" };
const d = { block: "d" };
const dag = { plugin: "blockr.dag-dag_plugin" };

const head: Block = { registry_id: "blockr.core-head_block", state: {}, name: "Head", locked: null };

function group(...panels: Panel[]): TabGroup {
  const [first] = panels;
  if (first === undefined) throw new Error("A tab group holds a panel.");
  return { panels, active: first };
}

function grid(children: Grid["children"], sizes: number[], rails?: Grid["rails"]): Grid {
  return { orientation: "horizontal", children, sizes, ...(rails === undefined ? {} : { rails }) };
}

function view(index: string, of: Grid = grid([], []), folder: FolderId | null = null): View {
  return { name: "View", folder, index, grid: of };
}

function folder(index: string, parent: FolderId | null = null): Folder {
  return { name: "Folder", folder: parent, index };
}

function boardWith(state: DockState): Board {
  return { id: "board", name: "Board", blocks: {}, links: {}, stacks: {}, options: {}, plugins: { [DOCK]: state } };
}

function at(...segments: string[]): string {
  return pointer(["plugins", DOCK, ...segments]);
}

// The state after a delta and the patches that the `update` returned for it.
function after(state: DockState, delta: Delta, patches: readonly Patch[]): DockState {
  const next = patched(state, [...(delta.patches ?? []), ...patches], DOCK);
  if (!isDockState(next)) throw new Error("The patches leave no state.");
  return next;
}

// What the `update` returns for the delta, after checking that it returns no
// patch when the store calls it again with the delta that holds them.
function updated(delta: Delta, board: Board): Patch[] {
  const patches = update(delta, board, DOCK);
  expect(update({ ...delta, patches: [...(delta.patches ?? []), ...patches] }, board, DOCK)).toEqual([]);
  return patches;
}

const state: DockState = {
  views: {
    v1: view("a0", grid([{ panels: [a, b], active: b }, group(c)], [0.5, 0.5])),
    v2: view("a1", grid([group(a)], [1], { left: { ...group(dag), collapsed: false, size: 260, collapsed_size: 35 } })),
    v3: view("a2", grid([group(c)], [1])),
  },
  folders: {},
  active: "v1",
  visible: { a: ["outputs"], c: ["inputs"] },
};

describe("the update, for a block that the delta removes", () => {
  test("takes the block's panel out of every grid that places it and drops its entry in visible", () => {
    const patches = updated({ blocks: { rm: ["a"] } }, boardWith(state));
    expect(patches).toEqual([
      { op: "replace", path: at("views", "v1", "grid"), value: grid([group(b), group(c)], [0.5, 0.5]) },
      { op: "replace", path: at("views", "v2", "grid"), value: grid([], [], state.views.v2?.grid.rails) },
      { op: "remove", path: at("visible", "a") },
    ]);
  });

  test("changes nothing for a block that no grid places and that has no entry in visible", () => {
    expect(updated({ blocks: { rm: ["z"] } }, boardWith(state))).toEqual([]);
  });
});

describe("the update, for a block that the delta adds", () => {
  test("places the block's panel in the active view at the default place", () => {
    const patches = updated({ blocks: { add: { d: head } } }, boardWith(state));
    expect(patches).toEqual([
      { op: "replace", path: at("views", "v1", "grid"), value: grid([{ panels: [a, b], active: b }, { panels: [c, d], active: d }], [0.5, 0.5]) },
    ]);
  });

  test("leaves a block's panel where the delta's own patches place it", () => {
    const placed = grid([group(c), group(d)], [0.5, 0.5]);
    const delta: Delta = { blocks: { add: { d: head } }, patches: [{ op: "replace", path: at("views", "v3", "grid"), value: placed }] };
    expect(updated(delta, boardWith(state))).toEqual([]);
  });

  test("places nothing while the state holds no view", () => {
    expect(updated({ blocks: { add: { d: head } } }, boardWith(defaultState))).toEqual([]);
  });

  test("places the panels of the blocks that it adds while it takes out those that it removes", () => {
    const patches = updated({ blocks: { add: { d: head }, rm: ["c"] } }, boardWith(state));
    expect(after(state, {}, patches).views).toMatchObject({
      v1: { grid: grid([{ panels: [a, b, d], active: d }], [1]) },
      v3: { grid: grid([], []) },
    });
  });
});

describe("the update, for what the delta's patches leave naming nothing", () => {
  test("makes the first view of the navigation active where the patches remove the active view", () => {
    const delta: Delta = { patches: [{ op: "remove", path: at("views", "v1") }] };
    expect(updated(delta, boardWith(state))).toEqual([{ op: "replace", path: at("active"), value: "v2" }]);
  });

  test("sets `active` to null where the patches remove every view", () => {
    const delta: Delta = { patches: [{ op: "replace", path: at("views"), value: {} }] };
    expect(updated(delta, boardWith(state))).toEqual([{ op: "replace", path: at("active"), value: null }]);
  });

  test("places an added block in the view that it makes active", () => {
    const delta: Delta = { blocks: { add: { d: head } }, patches: [{ op: "replace", path: at("active"), value: "gone" }] };
    const patches = updated(delta, boardWith(state));
    expect(after(state, delta, patches)).toMatchObject({
      active: "v1",
      views: { v1: { grid: grid([{ panels: [a, b], active: b }, { panels: [c, d], active: d }], [0.5, 0.5]) } },
    });
  });

  // The top holds Overview, the folder Safety and Listing. Safety holds
  // Adverse events, the folder Nested and Aside, and Nested holds Labs.
  const foldered: DockState = {
    ...defaultState,
    views: {
      overview: view("a0"),
      ae: view("a0", undefined, "safety"),
      labs: view("a0", undefined, "nested"),
      aside: view("a2", undefined, "safety"),
      listing: view("a2"),
    },
    folders: { safety: folder("a1"), nested: folder("a1", "safety") },
    active: "overview",
  };

  test("moves what a removed folder held up into its place, in its order, with new keys", () => {
    const delta: Delta = { patches: [{ op: "remove", path: at("folders", "safety") }] };
    const patches = updated(delta, boardWith(foldered));
    expect(patches.map(({ path }) => path)).toEqual([
      at("views", "ae", "folder"),
      at("views", "ae", "index"),
      at("folders", "nested", "folder"),
      at("folders", "nested", "index"),
      at("views", "aside", "folder"),
      at("views", "aside", "index"),
    ]);
    const next = after(foldered, delta, patches);
    expect(inFolder(next, null)).toEqual([
      { view: "overview" },
      { view: "ae" },
      { folder: "nested" },
      { view: "aside" },
      { view: "listing" },
    ]);
    expect(inFolder(next, "nested")).toEqual([{ view: "labs" }]);
  });

  test("moves what nested removed folders held up into the place of the outer one", () => {
    const delta: Delta = {
      patches: [
        { op: "remove", path: at("folders", "safety") },
        { op: "remove", path: at("folders", "nested") },
      ],
    };
    const next = after(foldered, delta, updated(delta, boardWith(foldered)));
    expect(inFolder(next, null)).toEqual([
      { view: "overview" },
      { view: "ae" },
      { view: "labs" },
      { view: "aside" },
      { view: "listing" },
    ]);
  });

  test("keeps the order of two removed folders that stood side by side", () => {
    const sides: DockState = {
      ...defaultState,
      views: {
        x: view("a0"),
        p: view("a0", undefined, "f1"),
        q: view("a1", undefined, "f1"),
        r: view("a0", undefined, "f2"),
        y: view("a3"),
      },
      folders: { f1: folder("a1"), f2: folder("a2") },
      active: "x",
    };
    const delta: Delta = { patches: [{ op: "replace", path: at("folders"), value: {} }] };
    const next = after(sides, delta, updated(delta, boardWith(sides)));
    expect(inFolder(next, null)).toEqual([{ view: "x" }, { view: "p" }, { view: "q" }, { view: "r" }, { view: "y" }]);
  });

  test("moves a view whose folder the board before lacks too to the end of the top", () => {
    const delta: Delta = { patches: [{ op: "replace", path: at("views", "overview", "folder"), value: "made_up" }] };
    const next = after(foldered, delta, updated(delta, boardWith(foldered)));
    expect(inFolder(next, null)).toEqual([{ folder: "safety" }, { view: "listing" }, { view: "overview" }]);
  });
});

describe("the update, for a delta that leaves the dock's state alone", () => {
  test("changes nothing, even where the state names a view that it lacks", () => {
    const dangling: DockState = { ...state, active: "gone" };
    const delta: Delta = { blocks: { mod: { a: { name: "Renamed" } } } };
    expect(update(delta, boardWith(dangling), DOCK)).toEqual([]);
  });
});

describe("the update, for a delta that it cannot read", () => {
  test("returns no patch where the delta's patches leave a state of another shape", () => {
    const delta: Delta = { blocks: { rm: ["a"] }, patches: [{ op: "replace", path: at("views", "v1", "grid"), value: {} }] };
    expect(update(delta, boardWith(state), DOCK)).toEqual([]);
  });

  test("returns no patch where one of the delta's patches cannot apply", () => {
    const delta: Delta = { blocks: { rm: ["a"] }, patches: [{ op: "remove", path: at("views", "gone") }] };
    expect(update(delta, boardWith(state), DOCK)).toEqual([]);
  });

  test("reads past the patches of other plugins", () => {
    const delta: Delta = { blocks: { rm: ["c"] }, patches: [{ op: "replace", path: "/plugins/blockr.dag-dag_plugin/positions", value: {} }] };
    expect(updated(delta, boardWith(state)).map(({ path }) => path)).toEqual([
      at("views", "v1", "grid"),
      at("views", "v3", "grid"),
      at("visible", "c"),
    ]);
  });
});
