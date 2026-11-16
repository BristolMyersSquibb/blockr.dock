// When the logo on the bar turns, on a scheduler whose statuses the test sets.

import type { Block, BlockId, BlockStatus, Board, CoreApi, Status } from "@blockr/core";
import { afterEach, beforeEach, describe, expect, test, vi } from "vitest";
import type { DockState } from "../../state/index.ts";
import { trackBusy } from "./busy.ts";
import type { Narrow } from "./narrow.ts";

const DOCK = "blockr.dock-dock_plugin";

const head: Block = { registry_id: "blockr.core-head_block", state: {}, name: "Head", locked: null };

// The active view shows the block "a", and "b" only on a collapsed rail, which
// the narrow column shows.
const state: DockState = {
  views: {
    main: {
      name: "Main",
      folder: null,
      index: "a0",
      grid: {
        orientation: "horizontal",
        children: [{ panels: [{ block: "a" }], active: { block: "a" } }],
        sizes: [1],
        rails: { left: { panels: [{ block: "b" }], active: { block: "b" }, collapsed: true, size: 260 } },
      },
    },
  },
  folders: {},
  active: "main",
  visible: {},
};

const board: Board = {
  id: "board",
  name: "Board",
  blocks: { a: { ...head, name: "A" }, b: { ...head, name: "B" }, c: { ...head, name: "C" } },
  links: {},
  stacks: {},
  options: {},
  plugins: { [DOCK]: state },
};

// A store and a scheduler whose statuses the test sets, each of which says
// what changed as the test changes it.
function fakeCore() {
  const statuses = new Map<BlockId, Status>();
  const heard = new Set<(changed: readonly BlockId[]) => void>();
  const core = {
    store: { board: () => board, subscribe: () => () => undefined },
    scheduler: {
      status: (id: BlockId) => {
        const status = statuses.get(id);
        return status === undefined ? undefined : ({ status } as BlockStatus);
      },
      subscribe: (fn: (changed: readonly BlockId[]) => void) => {
        heard.add(fn);
        return () => {
          heard.delete(fn);
        };
      },
    },
  } as unknown as Pick<CoreApi, "store" | "scheduler">;
  const set = (id: BlockId, status: Status) => {
    statuses.set(id, status);
    for (const fn of [...heard]) fn([id]);
  };
  return { core, set, heard };
}

function fakeNarrow(narrow: boolean): Narrow & { readonly cross: () => void } {
  let now = narrow;
  const listeners = new Set<() => void>();
  return {
    get: () => now,
    subscribe(fn) {
      listeners.add(fn);
      return () => {
        listeners.delete(fn);
      };
    },
    stop: () => undefined,
    cross() {
      now = !now;
      for (const fn of [...listeners]) fn();
    },
  };
}

beforeEach(() => {
  vi.useFakeTimers();
});

afterEach(() => {
  vi.useRealTimers();
});

describe("trackBusy()", () => {
  test("turns once a demanded block has read unevaluated or stale for the delay, and stops as soon as none does", () => {
    const { core, set } = fakeCore();
    set("a", "unevaluated");
    const busy = trackBusy(core, new Map(), fakeNarrow(false), 200);
    const heard = vi.fn();
    busy.subscribe(heard);

    expect(busy.get()).toBe(false);
    vi.advanceTimersByTime(199);
    expect(busy.get()).toBe(false);
    vi.advanceTimersByTime(1);
    expect(busy.get()).toBe(true);
    expect(heard).toHaveBeenCalledTimes(1);

    set("a", "stale");
    expect(busy.get()).toBe(true);
    set("a", "ready");
    expect(busy.get()).toBe(false);
    expect(heard).toHaveBeenCalledTimes(2);
    busy.stop();
  });

  test("never turns for an evaluation that ends before the delay is up", () => {
    const { core, set } = fakeCore();
    const busy = trackBusy(core, new Map(), fakeNarrow(false), 200);
    set("a", "stale");
    vi.advanceTimersByTime(150);
    set("a", "ready");
    vi.advanceTimersByTime(1000);
    expect(busy.get()).toBe(false);
    busy.stop();
  });

  test("leaves out the blocks that the dock does not demand, but follows what it demands as the window crosses the breakpoint", () => {
    const { core, set } = fakeCore();
    set("a", "ready");
    set("c", "stale");
    set("b", "unevaluated");
    const narrow = fakeNarrow(false);
    const busy = trackBusy(core, new Map(), narrow, 0);
    vi.advanceTimersByTime(10);
    // The block "c" has no panel in the view, and "b" sits on a collapsed
    // rail.
    expect(busy.get()).toBe(false);

    narrow.cross();
    vi.advanceTimersByTime(10);
    expect(busy.get()).toBe(true);
    busy.stop();
  });

  test("reads no status once it stops", () => {
    const { core, set, heard } = fakeCore();
    const busy = trackBusy(core, new Map(), fakeNarrow(false), 200);
    set("a", "unevaluated");
    busy.stop();
    expect(heard.size).toBe(0);
    vi.advanceTimersByTime(1000);
    expect(busy.get()).toBe(false);
  });
});
