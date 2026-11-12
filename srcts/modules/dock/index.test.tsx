// The dock's frontend on blockr.core's CoreApi for tests, which starts core
// as the page of an app that runs the dock would, with R played from the
// fixtures of core's wire protocol, so that the tests need neither R nor a
// browser (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#testing).

import { HELLO, stubCore } from "@blockr/core/stubs";
import { describe, expect, test } from "vitest";
import * as dock from "./index.tsx";

const DOCK = "blockr.dock-dock_plugin";

// R's reply to hello in an app that runs the dock in place of core's basic
// frontend, beside core's other plugins, with where R serves the module of
// the dock plugin's class.
const hello = {
  ...HELLO,
  plugins: {
    ...Object.fromEntries(
      Object.entries(HELLO.plugins).filter(([id]) => id !== "blockr.core-basic_frontend_plugin"),
    ),
    [DOCK]: { name: "Dock", version: 1, module: "blockr.dock/js/dock.js" },
  },
};

// Starts core with the module of the dock plugin's class, whose frontend
// draws the page in `el` where a test gives one.
function startDock(el?: HTMLElement) {
  return stubCore({ hello, modules: { [DOCK]: dock }, el });
}

describe("the dock's frontend", () => {
  test("is the frontend of the dock plugin's class, which requires dark_mode", async () => {
    const { core, stop } = await startDock();

    const cls = core.plugins.pluginClass(DOCK);
    expect(cls.frontend).toBe(dock.default.frontend);
    expect(cls.options).toEqual(["dark_mode"]);
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

  test("takes dockview's dark theme while dark_mode is dark", async () => {
    const el = document.createElement("div");
    const { core, settled, stop } = await startDock(el);
    await settled();
    const shell = () => el.querySelector(".blockr-dock .dv-shell");

    await core.commands.run("blockr.core-set_option", { id: "dark_mode", value: "dark" });
    await settled();
    expect(shell()?.classList).toContain("dockview-theme-abyss-spaced");
    expect(shell()?.classList).not.toContain("dockview-theme-light-spaced");

    await core.commands.run("blockr.core-set_option", { id: "dark_mode", value: "light" });
    await settled();
    expect(shell()?.classList).toContain("dockview-theme-light-spaced");
    stop();
  });

  test("removes what it drew in its cleanup", async () => {
    const s = await startDock();
    const el = document.createElement("div");
    const cleanup = dock.default.frontend.render({
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
