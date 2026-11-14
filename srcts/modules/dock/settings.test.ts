// The blockr options that the dock's frontend reads as it starts, and the
// narrow layout that one of them turns on.

import type { Json } from "@blockr/core";
import { describe, expect, test } from "vitest";
import { trackNarrow } from "./narrow.ts";
import { readSettings } from "./settings.ts";

// Resizes happy-dom's window, which tells its listeners as a browser does.
function setWindowWidth(width: number): void {
  (window as unknown as { readonly happyDOM: { setInnerWidth(width: number): void } }).happyDOM.setInnerWidth(width);
}

// What the frontend reads of CoreApi, with the values of blockr options that
// R holds and whether the app sets the board lock.
function coreWith(options: Readonly<Record<string, Json>> = {}, board = false) {
  return {
    lock: { board, load: false },
    blockrOption: (name: string) => Promise.resolve(Object.hasOwn(options, name) ? options[name] : undefined),
  };
}

describe("readSettings()", () => {
  test("takes the defaults where R holds none of the blockr options", async () => {
    expect(await readSettings(coreWith())).toEqual({
      tolerance: 0.005,
      lockResize: false,
      lockRearrange: false,
      breakpoint: undefined,
      fraction: 0.8,
    });
  });

  test("locks the sizes and the arrangement under the board lock, unless a deployment leaves them to readers", async () => {
    expect(await readSettings(coreWith({}, true))).toMatchObject({ lockResize: true, lockRearrange: true });
    expect(await readSettings(coreWith({ lock_resize: false, lock_rearrange: "FALSE" }, true))).toMatchObject({
      lockResize: false,
      lockRearrange: false,
    });
    expect(await readSettings(coreWith({ lock_resize: true }, false))).toMatchObject({ lockResize: false });
  });

  test("reads narrow_breakpoint as a number of pixels, and R's Inf as every width", async () => {
    expect(await readSettings(coreWith({ narrow_breakpoint: 600 }))).toMatchObject({ breakpoint: 600 });
    expect(await readSettings(coreWith({ narrow_breakpoint: "768" }))).toMatchObject({ breakpoint: 768 });
    expect(await readSettings(coreWith({ narrow_breakpoint: null }))).toMatchObject({ breakpoint: Infinity });
    expect(await readSettings(coreWith({ narrow_breakpoint: "Inf" }))).toMatchObject({ breakpoint: Infinity });
  });

  test("fails on a narrow_breakpoint below 50 pixels or of another kind, naming the value", async () => {
    for (const value of [49, "wide", "", true, [600]]) {
      await expect(readSettings(coreWith({ narrow_breakpoint: value }))).rejects.toThrow(
        `The blockr option narrow_breakpoint is ${JSON.stringify(value)}`,
      );
    }
  });

  test("reads narrow_group_fraction as a share above 0 and at most 1", async () => {
    expect(await readSettings(coreWith({ narrow_group_fraction: 0.5 }))).toMatchObject({ fraction: 0.5 });
    expect(await readSettings(coreWith({ narrow_group_fraction: "1" }))).toMatchObject({ fraction: 1 });
    for (const value of [0, 1.5, "half", null]) {
      await expect(readSettings(coreWith({ narrow_group_fraction: value }))).rejects.toThrow("The blockr option narrow_group_fraction");
    }
  });

  test("fails on a dock_grid_size_tol below 0", async () => {
    await expect(readSettings(coreWith({ dock_grid_size_tol: -1 }))).rejects.toThrow("The blockr option dock_grid_size_tol");
  });
});

describe("trackNarrow()", () => {
  test("follows the window as it crosses the breakpoint", () => {
    setWindowWidth(1024);
    const narrow = trackNarrow(window, 600);
    const seen: boolean[] = [];
    narrow.subscribe(() => seen.push(narrow.get()));
    expect(narrow.get()).toBe(false);

    setWindowWidth(500);
    setWindowWidth(400);
    setWindowWidth(1024);
    expect(seen).toEqual([true, false]);

    narrow.stop();
    setWindowWidth(500);
    expect(seen).toEqual([true, false]);
    setWindowWidth(1024);
  });

  test("is narrow at every width for Infinity, and at none without a breakpoint", () => {
    expect(trackNarrow(window, Infinity).get()).toBe(true);
    expect(trackNarrow(window, undefined).get()).toBe(false);
  });
});
