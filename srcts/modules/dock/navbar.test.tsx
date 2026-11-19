// The bar across the top of the dock, on blockr.core's CoreApi for tests, from
// the navbar items that R's reply to hello carries, as the method of R's
// frontend_args() for the dock plugin's class returns them
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.5#testing).
// A storage plugin class whose controls hold every kind, and the modules on
// the bar, come from a package that only the tests know, blockr.test.
// The tests give each module to core's test stub by the id that R's reply
// names it under, since core runs only the plugins that the reply names.

import type { CoreApi, Json, LogLine, PluginClass, StorageControl } from "@blockr/core";
import { HELLO, stubCore } from "@blockr/core/stubs";
import { beforeAll, describe, expect, onTestFinished, test } from "vitest";
import * as compactOption from "../compact-option/index.ts";
import * as syncRailsOption from "../sync-rails-option/index.ts";
import * as viewTabsOption from "../view-tabs-option/index.ts";
import * as dock from "./index.tsx";

const DOCK = "blockr.dock-dock_plugin";
const BASIC = "blockr.core-basic_frontend_plugin";
const FILE = "blockr.core-file_storage_plugin";
const STORAGE = "blockr.test-storage_plugin";

interface BarOptions {
  // The navbar items as R writes them, none for the default bar.
  readonly navbar?: readonly Json[];
  // The app's storage plugin: core's, the test's, or none.
  readonly storage?: "file" | "test" | "none";
  // The test storage plugin's class.
  readonly storageClass?: Omit<PluginClass, "id">;
  readonly lock?: { readonly board?: boolean; readonly load?: boolean };
  // Modules that packages offer, by id.
  readonly modules?: Readonly<Record<string, unknown>>;
  readonly blockrOptions?: Readonly<Record<string, Json>>;
}

// Starts core with the dock as the app's frontend, which draws the page in an
// element of the document, and stops it as the test finishes.
async function startBar(options: BarOptions = {}) {
  const storage = options.storage ?? "file";
  const plugins = Object.fromEntries(
    Object.entries(HELLO.plugins).filter(([id]) => id !== BASIC && (id !== FILE || storage === "file")),
  );
  const frontend: Record<string, Json> = options.navbar === undefined ? {} : { navbar: [...options.navbar] };
  const hello = {
    ...HELLO,
    packages: { ...HELLO.packages, "blockr.dock": "0.2.1", "blockr.test": "1.0.0" },
    plugins: {
      ...plugins,
      [DOCK]: {
        name: "Dock",
        version: 1,
        options: ["compact", "view_tabs", "sync_rails", "show_conditions", "dark_mode"],
        module: "blockr.dock/js/dock.js",
      },
      ...(storage === "test" ? { [STORAGE]: { name: "Test storage", version: 1, module: "blockr.test/storage.js" } } : {}),
    },
    options: {
      ...HELLO.options,
      compact: { package: "blockr.dock", module: "blockr.dock/compact-option.js", version: 1 },
      view_tabs: { package: "blockr.dock", module: "blockr.dock/view-tabs-option.js", version: 1 },
      sync_rails: { package: "blockr.dock", module: "blockr.dock/sync-rails-option.js", version: 1 },
    },
    lock: { board: options.lock?.board ?? false, load: options.lock?.load ?? false },
    frontend,
  };
  const modules = {
    [DOCK]: dock,
    compact: compactOption,
    view_tabs: viewTabsOption,
    sync_rails: syncRailsOption,
    ...(options.storageClass === undefined ? {} : { [STORAGE]: { api: 1, default: options.storageClass } }),
    ...options.modules,
  };
  const el = document.createElement("div");
  document.body.append(el);
  const s = await stubCore({ hello, modules, el, blockrOptions: options.blockrOptions ?? {} });
  onTestFinished(() => {
    s.stop();
    el.remove();
  });
  await s.settled();
  await drawn();
  return { ...s, el };
}

function dockBar(el: HTMLElement): HTMLElement {
  const bar = el.querySelector<HTMLElement>(".blockr-dock-bar");
  if (bar === null) throw new Error("The dock draws no bar.");
  return bar;
}

// What each child of the bar is, by its class.
function barItems(el: HTMLElement): string[] {
  const named: readonly (readonly [string, string])[] = [
    ["blockr-dock-logo", "logo"],
    ["blockr-dock-bar-name", "name"],
    ["blockr-dock-bar-storage", "storage"],
    ["blockr-dock-bar-steps", "undo_redo"],
    ["blockr-dock-bar-spacer", "spacer"],
    ["blockr-dock-bar-views", "views"],
    ["blockr-dock-bar-pill", "read_only"],
    ["blockr-dock-bar-options", "options"],
    ["blockr-dock-bar-module", "module"],
    ["blockr-dock-bar-warning", "warning"],
    ["blockr-dock-bar-status", "status"],
  ];
  return [...dockBar(el).children].flatMap((child) => {
    if (child.getAttribute("popover") !== null) return [];
    const name = named.find(([cls]) => child.classList.contains(cls))?.[1];
    if (name !== undefined) return [name];
    return [`${child.tagName.toLowerCase()}:${child.textContent}`];
  });
}

function button(root: ParentNode, name: string): HTMLButtonElement {
  const found = [...root.querySelectorAll<HTMLButtonElement>("button")].find(
    (b) => b.getAttribute("aria-label") === name || b.textContent === name,
  );
  if (found === undefined) throw new Error(`No button "${name}".`);
  return found;
}

// The rows of the open menu whose label is given, as their text.
function menuRows(el: HTMLElement, label: string): string[] {
  const menu = el.querySelector<HTMLElement>(`[role="menu"][aria-label="${label}"]`);
  if (menu === null) throw new Error(`No menu "${label}".`);
  return [...menu.querySelectorAll<HTMLElement>(':scope > [role="menuitem"], :scope > [role="none"]')].map((row) =>
    row.getAttribute("role") === "none" && row.querySelector('[role="menuitem"]') !== null
      ? `${row.querySelector('[role="menuitem"]')?.textContent ?? ""} >`
      : row.textContent,
  );
}

// A storage plugin class whose controls hold every kind, a menu inside a menu
// among them, which records what its buttons run and calls back through its
// subscribe() as the test changes its status.
function everyKind() {
  const runs: string[] = [];
  let saved = "Not saved";
  const listeners = new Set<() => void>();
  const run = (id: string) => () => {
    runs.push(id);
    return Promise.resolve();
  };
  const controls = (): StorageControl[] => [
    {
      kind: "menu",
      id: "workflow",
      label: "Workflow one",
      items: [
        { kind: "button", id: "new", label: "New", run: run("new") },
        { kind: "status", id: "owner", text: "Owned by you" },
        { kind: "menu", id: "recent", label: "Recent", items: [{ kind: "button", id: "open-a", label: "Open A", shortcut: "Mod+O", run: run("open-a") }] },
      ],
    },
    { kind: "status", id: "saved", text: saved, tooltip: "When the board was saved" },
    { kind: "button", id: "save", label: "Save", shortcut: "Mod+S", run: run("save") },
    { kind: "button", id: "share", label: "Share", tooltip: "Save first", shortcut: "Mod+E", disabled: true, run: run("share") },
    { kind: "status", id: "avatar", text: "NJ" },
  ];
  const storageClass: Omit<PluginClass, "id"> = {
    storage: {
      controls,
      subscribe(fn) {
        listeners.add(fn);
        return () => {
          listeners.delete(fn);
        };
      },
    },
  };
  const setSaved = (text: string) => {
    saved = text;
    for (const fn of [...listeners]) fn();
  };
  return { runs, storageClass, setSaved, listeners };
}

describe("the bar", () => {
  test("draws the dock's own parts of the default bar in their order, where R sends no navbar", async () => {
    const { el } = await startBar();

    expect(barItems(el)).toEqual(["logo", "name", "storage", "undo_redo", "spacer", "views", "options"]);
    // Core's storage plugin gives Save and Open.
    const storage = el.querySelector(".blockr-dock-bar-storage");
    expect([...(storage?.querySelectorAll("button") ?? [])].map((b) => b.textContent)).toEqual(["Save", "Open"]);
    expect(el.querySelector(".blockr-dock-logo")?.classList).not.toContain("is-computing");
  });

  test("draws the navbar items that R sends, in their order: the dock's own parts, links and menus", async () => {
    const navbar: Json[] = [
      { kind: "part", id: "logo" },
      { kind: "link", id: "docs", label: "Docs", href: "https://example.org/docs" },
      {
        kind: "menu",
        id: "help",
        label: "Help",
        items: [
          { kind: "link", id: "issues", label: "Report an issue", href: "https://example.org/issues" },
          { kind: "menu", id: "more", label: "More", items: [{ kind: "link", id: "faq", label: "FAQ", href: "https://example.org/faq" }] },
        ],
      },
      { kind: "part", id: "options" },
    ];
    const { el } = await startBar({ navbar });

    expect(barItems(el)).toEqual(["logo", "a:Docs", "button:Help", "options"]);
    const link = dockBar(el).querySelector("a");
    expect(link?.getAttribute("href")).toBe("https://example.org/docs");
    expect(link?.getAttribute("target")).toBe("_blank");
    expect(link?.getAttribute("rel")).toBe("noopener noreferrer");

    const help = button(dockBar(el), "Help");
    expect(help.getAttribute("aria-haspopup")).toBe("menu");
    click(help);
    await drawn();
    expect(help.getAttribute("aria-expanded")).toBe("true");
    expect(menuRows(el, "Help")).toEqual(["Report an issue", "More >"]);
    expect(el.querySelector('[role="menu"][aria-label="Help"] a')?.getAttribute("target")).toBe("_blank");

    click(button(el, "More"));
    await drawn();
    expect(menuRows(el, "More")).toEqual(["FAQ"]);
  });

  test("fails to draw, naming the item, where R sends a navbar item that the dock cannot draw", async () => {
    const { el } = await startBar({ navbar: [{ kind: "part", id: "clock" }] });

    expect(el.textContent).toContain('The dock has no part "clock" to draw on the navbar.');
    expect(el.querySelector(".blockr-dock")).toBeNull();
  });
});

describe("the storage plugin's controls on the bar", () => {
  test("draw each kind in the bar's look, in the plugin's order", async () => {
    const { storageClass } = everyKind();
    const { el } = await startBar({ storage: "test", storageClass });

    const storage = el.querySelector<HTMLElement>(".blockr-dock-bar-storage");
    if (storage === null) throw new Error("No storage controls.");
    expect([...storage.children].filter((c) => c.getAttribute("popover") === null).map((c) => c.textContent)).toEqual([
      "Workflow one",
      "Not saved",
      "Save",
      "Share",
      "NJ",
    ]);

    // A button is a quiet button, greyed out while it is disabled, with its
    // tooltip where it has one.
    const share = button(storage, "Share");
    expect(share.classList).toContain("blockr-dock-bar-button");
    expect(share.disabled).toBe(true);
    expect(share.title).toBe("Save first");
    expect(button(storage, "Save").getAttribute("aria-keyshortcuts")).toBe("Control+S Meta+S");

    // A status is text in the muted colour, with its tooltip.
    const saved = storage.querySelector<HTMLElement>(".blockr-dock-bar-status");
    expect(saved?.textContent).toBe("Not saved");
    expect(saved?.title).toBe("When the board was saved");

    // A menu opens a menu that holds its items, and a menu among those opens
    // beside it.
    const menu = button(storage, "Workflow one");
    expect(menu.querySelector(".blockr-dock-chevron")).not.toBeNull();
    click(menu);
    await drawn();
    expect(menuRows(el, "Workflow one")).toEqual(["New", "Owned by you", "Recent >"]);
    click(button(el, "Recent"));
    await drawn();
    expect(menuRows(el, "Recent")).toEqual(["Open A"]);
    expect(el.querySelector('[role="menu"][aria-label="Recent"]')?.getAttribute("data-depth")).toBe("1");
  });

  test("run a button as the user picks it, and a pick closes the menus it opened from", async () => {
    const { runs, storageClass } = everyKind();
    const { el } = await startBar({ storage: "test", storageClass });
    const storage = el.querySelector<HTMLElement>(".blockr-dock-bar-storage");
    if (storage === null) throw new Error("No storage controls.");

    click(button(storage, "Save"));
    click(button(storage, "Share"));
    expect(runs).toEqual(["save"]);

    click(button(storage, "Workflow one"));
    await drawn();
    click(button(el, "Recent"));
    await drawn();
    click(button(el, "Open A"));
    await drawn();
    expect(runs).toEqual(["save", "open-a"]);
    expect(el.querySelector('[role="menu"][aria-label="Workflow one"] [role="menuitem"]')).toBeNull();
    expect(button(storage, "Workflow one").getAttribute("aria-expanded")).toBe("false");
  });

  test("keep the focus on a menu while the arrows move its row past a status, and open a menu inside it by ArrowRight", async () => {
    const { storageClass } = everyKind();
    const { el } = await startBar({ storage: "test", storageClass });
    const storage = el.querySelector<HTMLElement>(".blockr-dock-bar-storage");
    if (storage === null) throw new Error("No storage controls.");
    // The row that a menu names as its active descendant.
    const active = (menu: Element | null) => document.getElementById(menu?.getAttribute("aria-activedescendant") ?? "")?.textContent;
    const keyOn = (target: Element | null, key: string) => {
      target?.dispatchEvent(new KeyboardEvent("keydown", { key, bubbles: true, cancelable: true }));
    };

    const opener = button(storage, "Workflow one");
    opener.focus();
    keyOn(opener, "ArrowDown");
    await drawn();
    const menu = el.querySelector('[role="menu"][aria-label="Workflow one"]');
    expect(document.activeElement).toBe(menu);
    expect(active(menu)).toBe("New");

    // The arrows pass over the status "Owned by you".
    keyOn(menu, "ArrowDown");
    await drawn();
    expect(active(menu)).toBe("Recent");
    keyOn(menu, "ArrowRight");
    await drawn();
    const recent = el.querySelector('[role="menu"][aria-label="Recent"]');
    expect(document.activeElement).toBe(recent);
    expect(active(recent)).toBe("Open A");

    keyOn(recent, "ArrowLeft");
    await drawn();
    expect(el.querySelector('[role="menu"][aria-label="Recent"]')).toBeNull();
    expect(document.activeElement).toBe(menu);
    keyOn(menu, "Escape");
    await drawn();
    expect(el.querySelector('[role="menu"][aria-label="Workflow one"]')).toBeNull();
    expect(document.activeElement).toBe(opener);
  });

  test("place a control apart where a storage item names it, while the storage item draws the rest", async () => {
    const { storageClass } = everyKind();
    const navbar: Json[] = [
      { kind: "part", id: "storage" },
      { kind: "part", id: "spacer" },
      { kind: "part", id: "options" },
      { kind: "storage", id: "avatar" },
      { kind: "storage", id: "nothing" },
    ];
    const { el } = await startBar({ storage: "test", storageClass, navbar });

    expect(barItems(el)).toEqual(["storage", "spacer", "options", "status"]);
    expect(dockBar(el).lastElementChild?.textContent).toBe("NJ");
    expect(el.querySelector(".blockr-dock-bar-storage")?.textContent).not.toContain("NJ");
  });

  test("draw again as the storage plugin's subscribe() calls back", async () => {
    const { storageClass, setSaved } = everyKind();
    const { el } = await startBar({ storage: "test", storageClass });

    setSaved("Saved just now");
    await drawn();
    expect(el.querySelector(".blockr-dock-bar-status")?.textContent).toBe("Saved just now");
  });

  test("bind the shortcut of every button, those in menus too, but not of a disabled one, nor in a text field", async () => {
    const { runs, storageClass } = everyKind();
    const { el } = await startBar({ storage: "test", storageClass });

    pressOnPage({ key: "s", ctrlKey: true });
    pressOnPage({ key: "o", metaKey: true });
    pressOnPage({ key: "e", ctrlKey: true });
    pressOnPage({ key: "s", ctrlKey: true, repeat: true });
    expect(runs).toEqual(["save", "open-a"]);

    const field = document.createElement("input");
    el.append(field);
    field.dispatchEvent(new KeyboardEvent("keydown", { key: "s", ctrlKey: true, bubbles: true }));
    expect(runs).toEqual(["save", "open-a"]);
  });

  test("bind no shortcut of a control that the bar does not draw", async () => {
    const { runs, storageClass } = everyKind();
    const { el } = await startBar({ storage: "test", storageClass, navbar: [{ kind: "storage", id: "save" }] });

    expect(barItems(el)).toEqual(["button:Save"]);
    pressOnPage({ key: "o", ctrlKey: true });
    pressOnPage({ key: "s", ctrlKey: true });
    expect(runs).toEqual(["save"]);
  });

  test("log a run() that rejects, while the storage plugin tells the user why through a notification of its own", async () => {
    let core: CoreApi | undefined;
    const storageClass: Omit<PluginClass, "id"> = {
      storage: {
        controls: (args) => {
          core = args.core;
          return [
            {
              kind: "button",
              id: "save",
              label: "Save",
              run: () => {
                args.core.notifications.notify(args.plugin, { severity: "error", code: "save_failed", message: "Connect is down." });
                return Promise.reject(new Error("HTTP 503"));
              },
            },
          ];
        },
      },
    };
    const s = await startBar({ storage: "test", storageClass });

    click(button(dockBar(s.el), "Save"));
    await drawn();
    expect(core).toBe(s.core);
    expect(errors(s.logged, "blockr.dock")).toContain("HTTP 503");
    const toasts = [...document.querySelectorAll(".blockr-dock-toast")].map((t) => t.textContent);
    expect(toasts.filter((t) => t.includes("Connect is down."))).toHaveLength(1);
    expect(toasts.some((t) => t.includes("HTTP 503"))).toBe(false);
  });

  test("leave a warning in their place where controls() throws, which names the plugin's package", async () => {
    const storageClass: Omit<PluginClass, "id"> = {
      storage: {
        controls: () => {
          throw new Error("No token.");
        },
      },
    };
    const s = await startBar({ storage: "test", storageClass });

    const warning = s.el.querySelector<HTMLElement>(".blockr-dock-bar-warning");
    expect(warning?.title).toContain("blockr.test");
    expect(warning?.title).toContain("No token.");
    expect(barItems(s.el)).toEqual(["logo", "name", "warning", "undo_redo", "spacer", "views", "options"]);
    expect(errors(s.logged, "blockr.dock").filter((m) => m.includes("No token."))).toHaveLength(1);
  });

  test("draw the first of two controls of one list that share an id, and log the second", async () => {
    const run = () => Promise.resolve();
    const storageClass: Omit<PluginClass, "id"> = {
      storage: {
        controls: () => [
          { kind: "button", id: "save", label: "Save", run },
          { kind: "button", id: "save", label: "Save again", run },
        ],
      },
    };
    const s = await startBar({ storage: "test", storageClass });

    expect(s.el.querySelector(".blockr-dock-bar-storage")?.textContent).toBe("Save");
    expect(errors(s.logged, "blockr.dock").filter((m) => m.includes('"save"'))).toHaveLength(1);
  });

  test("give way first on a narrow bar, with a label cut short in full as the tooltip, where the control has none", async () => {
    const { storageClass } = everyKind();
    const { el } = await startBar({ storage: "test", storageClass });

    const shrinking = [...dockBar(el).children].filter((c) => c.classList.contains("blockr-dock-bar-shrinks"));
    expect(shrinking.map((c) => c.className.split(" ")[0])).toEqual(["blockr-dock-bar-name", "blockr-dock-bar-storage"]);

    const storage = el.querySelector<HTMLElement>(".blockr-dock-bar-storage");
    if (storage === null) throw new Error("No storage controls.");
    const menu = button(storage, "Workflow one");
    const avatar = [...storage.querySelectorAll<HTMLElement>(".blockr-dock-bar-status")].find((s) => s.textContent === "NJ");
    const saved = storage.querySelector<HTMLElement>(".blockr-dock-bar-status");
    for (const control of [menu, avatar, saved]) {
      if (control === undefined || control === null) throw new Error("No control.");
      cutShort(control);
      control.dispatchEvent(new PointerEvent("pointerover", { bubbles: true }));
    }
    expect(menu.title).toBe("Workflow one");
    expect(avatar?.title).toBe("NJ");
    // A status with a tooltip of its own keeps it.
    expect(saved?.title).toBe("When the board was saved");
  });
});

describe("a module on the bar", () => {
  test("draws in the item's element with CoreApi, and goes as the bar goes, through its signal and its cleanup", async () => {
    const rendered: { el: HTMLElement; core: CoreApi; signal: AbortSignal }[] = [];
    let cleaned = 0;
    const deck = {
      api: 1,
      default: {
        render(args: { el: HTMLElement; core: CoreApi; signal: AbortSignal }) {
          rendered.push(args);
          args.el.textContent = "Deck";
          return () => {
            cleaned += 1;
          };
        },
      },
    };
    const navbar: Json[] = [
      { kind: "part", id: "logo" },
      { kind: "module", id: "deck", module: "blockr.test-deck_button" },
    ];
    const s = await startBar({ navbar, modules: { "blockr.test-deck_button": deck } });
    await drawn();

    expect(barItems(s.el)).toEqual(["logo", "module"]);
    const slot = s.el.querySelector(".blockr-dock-bar-module");
    expect(slot?.textContent).toBe("Deck");
    expect(rendered).toHaveLength(1);
    expect(rendered[0]?.el).toBe(slot);
    expect(rendered[0]?.core).toBe(s.core);
    expect(rendered[0]?.signal.aborted).toBe(false);

    s.stop();
    expect(rendered[0]?.signal.aborted).toBe(true);
    expect(cleaned).toBe(1);
  });

  test("leaves a warning that names the module where it fails to load, declares another major or throws, and the rest of the bar works", async () => {
    const navbar: Json[] = [
      { kind: "part", id: "logo" },
      { kind: "module", id: "missing", module: "blockr.test-missing" },
      { kind: "module", id: "newer", module: "blockr.test-newer" },
      { kind: "module", id: "broken", module: "blockr.test-broken" },
      { kind: "part", id: "options" },
    ];
    const modules = {
      "blockr.test-newer": { api: 2, default: { render: () => undefined } },
      "blockr.test-broken": {
        api: 1,
        default: {
          render: () => {
            throw new Error("No canvas.");
          },
        },
      },
    };
    const s = await startBar({ navbar, modules });
    await drawn();

    expect(barItems(s.el)).toEqual(["logo", "warning", "warning", "warning", "options"]);
    const titles = [...s.el.querySelectorAll<HTMLElement>(".blockr-dock-bar-warning")].map((w) => w.title);
    expect(titles[0]).toContain("blockr.test-missing");
    expect(titles[1]).toContain("blockr.test-newer");
    expect(titles[1]).toContain("major 2");
    expect(titles[2]).toContain("blockr.test-broken");
    expect(titles[2]).toContain("No canvas.");
    expect(errors(s.logged, "blockr.dock")).toHaveLength(3);

    click(button(dockBar(s.el), "Options"));
    await drawn();
    expect(button(dockBar(s.el), "Options").getAttribute("aria-expanded")).toBe("true");
  });
});

describe("the dock's own parts", () => {
  test("rename the board on a click on its name, through core's command", async () => {
    const { el, core } = await startBar();
    const before = core.store.board().name;

    const name = dockBar(el).querySelector<HTMLButtonElement>("button.blockr-dock-bar-name");
    expect(name?.textContent).toBe(before);
    click(name);
    await drawn();
    const field = dockBar(el).querySelector<HTMLInputElement>("input.blockr-dock-bar-field");
    if (field === null) throw new Error("No field.");
    expect(document.activeElement).toBe(field);
    type(field, "  Quarterly  ");
    field.dispatchEvent(new KeyboardEvent("keydown", { key: "Enter", bubbles: true }));
    await drawn();

    expect(core.store.board().name).toBe("Quarterly");
    expect(dockBar(el).querySelector("button.blockr-dock-bar-name")?.textContent).toBe("Quarterly");
    expect(core.store.canUndo()).toBe(true);
  });

  test("give up a rename on Escape, and keep the name where the field is empty", async () => {
    const { el, core } = await startBar();
    const before = core.store.board().name;

    for (const [text, key] of [
      ["Other", "Escape"],
      ["   ", "Enter"],
    ] as const) {
      click(dockBar(el).querySelector("button.blockr-dock-bar-name"));
      await drawn();
      const field = dockBar(el).querySelector<HTMLInputElement>("input.blockr-dock-bar-field");
      if (field === null) throw new Error("No field.");
      type(field, text);
      field.dispatchEvent(new KeyboardEvent("keydown", { key, bubbles: true }));
      await drawn();
    }
    expect(core.store.board().name).toBe(before);
    expect(core.store.canUndo()).toBe(false);
  });

  test("follow the store's history with Undo and Redo, which run on their shortcuts but not in a text field", async () => {
    const { el, core } = await startBar();
    const before = core.store.board().name;
    const undo = button(dockBar(el), "Undo");
    const redo = button(dockBar(el), "Redo");
    // Both show their labels, as the storage plugin's buttons beside them do.
    expect([undo.textContent, redo.textContent]).toEqual(["Undo", "Redo"]);
    expect(undo.disabled).toBe(true);
    expect(redo.disabled).toBe(true);
    expect(undo.title).toMatch(/^Undo \((Ctrl\+Z|⌘Z)\)$/);

    await core.commands.run("blockr.core-rename_board", { name: "Renamed" });
    await drawn();
    expect(undo.disabled).toBe(false);

    click(undo);
    await drawn();
    expect(core.store.board().name).toBe(before);
    expect(redo.disabled).toBe(false);

    pressOnPage({ key: "z", ctrlKey: true, shiftKey: true });
    expect(core.store.board().name).toBe("Renamed");
    pressOnPage({ key: "z", metaKey: true });
    expect(core.store.board().name).toBe(before);
    pressOnPage({ key: "y", ctrlKey: true });
    expect(core.store.board().name).toBe("Renamed");

    const field = document.createElement("input");
    el.append(field);
    field.dispatchEvent(new KeyboardEvent("keydown", { key: "z", ctrlKey: true, bubbles: true }));
    expect(core.store.board().name).toBe("Renamed");
  });

  test("show under the board lock the name alone, the read-only pill, and Undo and Redo", async () => {
    const { el } = await startBar({ lock: { board: true } });

    expect(barItems(el)).toEqual(["logo", "name", "storage", "undo_redo", "spacer", "views", "read_only", "options"]);
    const name = dockBar(el).querySelector<HTMLElement>(".blockr-dock-bar-name");
    expect(name?.tagName).toBe("SPAN");
    click(name);
    await drawn();
    expect(dockBar(el).querySelector("input")).toBeNull();

    const pill = dockBar(el).querySelector<HTMLElement>(".blockr-dock-bar-pill");
    expect(pill?.textContent).toBe("Read-only");
    expect(pill?.title).toBe("Editing is disabled by this deployment.");
    // Core's storage plugin keeps Save under the board lock.
    expect(el.querySelector(".blockr-dock-bar-storage")?.textContent).toBe("SaveOpen");
  });

  test("draw no storage controls in an app without a storage plugin, and core's leave Open out under the load lock", async () => {
    const none = await startBar({ storage: "none" });
    expect(barItems(none.el)).toEqual(["logo", "name", "undo_redo", "spacer", "views", "options"]);
    none.stop();

    const loaded = await startBar({ lock: { load: true } });
    expect(loaded.el.querySelector(".blockr-dock-bar-storage")?.textContent).toBe("Save");
  });

  test("open and close the options sidebar from the options button", async () => {
    const { el } = await startBar();
    const options = button(dockBar(el), "Options");
    expect(options.getAttribute("aria-expanded")).toBe("false");

    click(options);
    await drawn();
    expect(options.getAttribute("aria-expanded")).toBe("true");
    click(options);
    await drawn();
    expect(options.getAttribute("aria-expanded")).toBe("false");
  });

  test("take the board's name in full as its tooltip where the bar cuts it short", async () => {
    const { el, core } = await startBar();
    const name = dockBar(el).querySelector<HTMLElement>("button.blockr-dock-bar-name");
    if (name === null) throw new Error("No name.");

    name.dispatchEvent(new PointerEvent("pointerover", { bubbles: true }));
    expect(name.title).toBe("Click to rename");
    cutShort(name);
    name.dispatchEvent(new PointerEvent("pointerout", { bubbles: true }));
    name.dispatchEvent(new PointerEvent("pointerover", { bubbles: true }));
    expect(name.title).toBe(`${core.store.board().name}\nClick to rename`);
  });
});

// The tests run on happy-dom, which has no Popover API, with which the bar's
// menus show in the top layer and hide again.
// So the tests give every element a showPopover() and a hidePopover() that do
// nothing, and read what the menus draw.
beforeAll(() => {
  for (const method of ["showPopover", "hidePopover"]) {
    if (!(method in HTMLElement.prototype)) {
      Object.defineProperty(HTMLElement.prototype, method, { configurable: true, value: () => undefined });
    }
  }
});

// Clicks the element as a pointer does, which goes down on it first.
function click(target: Element | null | undefined): void {
  if (target === null || target === undefined) throw new Error("Nothing to click.");
  target.dispatchEvent(new PointerEvent("pointerdown", { bubbles: true }));
  target.dispatchEvent(new MouseEvent("click", { bubbles: true, detail: 1 }));
}

// Presses keys on the page, outside every field.
function pressOnPage(init: KeyboardEventInit): void {
  document.body.dispatchEvent(new KeyboardEvent("keydown", { ...init, bubbles: true, cancelable: true }));
}

// Types the text into the field, as React hears it.
function type(field: HTMLInputElement, text: string): void {
  Object.getOwnPropertyDescriptor(HTMLInputElement.prototype, "value")?.set?.call(field, text);
  field.dispatchEvent(new Event("input", { bubbles: true }));
}

// Has the element's label measure wider than the room that it has, as where
// the bar cuts it short.
function cutShort(control: HTMLElement): void {
  const label = control.querySelector<HTMLElement>(".blockr-dock-bar-label") ?? control;
  Object.defineProperty(label, "scrollWidth", { configurable: true, value: 120 });
  Object.defineProperty(label, "clientWidth", { configurable: true, value: 40 });
}

// The messages of the errors that the package logged.
function errors(logged: readonly LogLine[], pkg: string): string[] {
  return logged.filter((line) => line.level === "error" && line.package === pkg).map((line) => line.message);
}

// Waits for React to draw what an event changed.
async function drawn(): Promise<void> {
  await new Promise((resolve) => setTimeout(resolve, 0));
}
