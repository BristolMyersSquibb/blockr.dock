// The dock's frontend on blockr.core's CoreApi for tests, which starts core
// as the page of an app that runs the dock would, with R played from the
// fixtures of core's wire protocol, so that the tests need neither R nor a
// browser (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#testing).

import { HELLO, stubCore } from "@blockr/core/stubs";
import { describe, expect, test } from "vitest";
import * as startup from "../../startup/index.ts";
import * as dock from "./index.tsx";

const DOCK = "blockr.dock-dock_plugin";

// R's reply to hello in an app that runs the dock in place of core's basic
// frontend, beside core's other plugins.
const hello = {
  ...HELLO,
  plugins: {
    ...Object.fromEntries(
      Object.entries(HELLO.plugins).filter(([id]) => id !== "blockr.core-basic_frontend_plugin"),
    ),
    [DOCK]: { name: "Dock", version: 1 },
  },
};

// Starts core with the dock's startup module and its frontend, which draws
// the page in `el` where a test gives one.
function startDock(el?: HTMLElement) {
  return stubCore({
    hello,
    startup: { "blockr.dock": startup },
    modules: { "blockr.dock-dock": dock },
    el,
  });
}

describe("the dock's frontend", () => {
  test("is the frontend of the dock plugin's class, which the startup module registers", async () => {
    const { core, stop } = await startDock();

    expect(core.plugins.pluginClass(DOCK)).toEqual({ id: DOCK, frontend: "blockr.dock-dock" });
    expect(core.plugins.appPlugins()).toHaveProperty([DOCK], { name: "Dock" });
    // The class keeps no state, so the board holds nothing for the dock.
    expect(core.store.board().plugins).not.toHaveProperty([DOCK]);
    stop();
  });

  test("draws the page as an empty dockview, in the page's element", async () => {
    const el = document.createElement("div");
    const { settled, stop } = await startDock(el);
    await settled();

    const shell = el.querySelector(".blockr-dock .dv-shell");
    expect(shell?.classList).toContain("dockview-theme-light-spaced");
    expect(el.querySelectorAll(".dv-groupview")).toHaveLength(0);
    stop();
  });

  test("removes what it drew in its cleanup", async () => {
    const s = await startDock();
    const el = document.createElement("div");
    const cleanup = dock.default.render({
      el,
      core: s.core,
      plugin: DOCK,
      signal: new AbortController().signal,
      args: {},
    });
    expect(el.querySelector(".blockr-dock")).not.toBeNull();

    cleanup();
    expect(el.childElementCount).toBe(0);
    s.stop();
  });

  test("draws the page again as another board opens", async () => {
    const el = document.createElement("div");
    const s = await startDock(el);
    await s.settled();
    const before = el.querySelector(".blockr-dock");

    await s.core.store.open(JSON.stringify(HELLO.board));
    await s.settled();

    const after = el.querySelector(".blockr-dock");
    expect(after).not.toBeNull();
    expect(after).not.toBe(before);
    expect(before?.isConnected).toBe(false);
    s.stop();
  });
});
