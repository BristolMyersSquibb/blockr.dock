// The dock's options sidebar, on blockr.core's CoreApi for tests, with the
// dock as the app's frontend
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.4#settings).
// Besides core's plugins and the dock, the app runs a plugin class of a
// package that only the tests know, blockr.test, which requires a board option
// of that package, zoom. The package defines a second board option, grain,
// which nothing requires, and whose default fails.

import type { CoreApi, Json, JsonBoard, Option, OptionModule, PluginClass } from "@blockr/core";
import { HELLO, stubCore } from "@blockr/core/stubs";
import { describe, expect, onTestFinished, test, vi } from "vitest";
import * as compactOption from "../compact-option/index.ts";
import * as syncRailsOption from "../sync-rails-option/index.ts";
import * as viewTabsOption from "../view-tabs-option/index.ts";
import * as dock from "./index.tsx";

const DOCK = "blockr.dock-dock_plugin";
const BASIC = "blockr.core-basic_frontend_plugin";
const VIEWER = "blockr.test-viewer_plugin";

// What the UIs of blockr.test's board options wait for before they draw,
// which a test may hold back.
let drawing: Promise<void> = Promise.resolve();

// A UI that shows the board option's value in a number field, and submits the
// number that the user types as the field changes. Escape in a field whose
// number differs from the value reverts the field, and takes the key, so that
// nothing else acts on it.
// After the field, the UI holds a button that it hides, as a UI may hide a
// control until it applies.
function numberUi(): OptionModule<number> {
  return {
    async render({ el, host }) {
      await drawing;
      const field = el.ownerDocument.createElement("input");
      field.type = "number";
      const show = () => {
        field.value = String(host.value());
      };
      show();
      const off = host.on("value", show);
      field.addEventListener("change", () => {
        void host.submit(Number(field.value));
      });
      field.addEventListener("keydown", (event) => {
        if (event.key !== "Escape" || field.value === String(host.value())) return;
        event.preventDefault();
        show();
      });
      const reset = el.ownerDocument.createElement("button");
      reset.type = "button";
      reset.textContent = "Reset";
      reset.style.display = "none";
      el.append(field, reset);
      return () => {
        off();
        field.remove();
        reset.remove();
      };
    },
  };
}

// The board option of blockr.test that its plugin class requires.
const zoom: Omit<Option, "id"> = {
  label: "Zoom",
  category: "View options",
  schema: { type: "integer", minimum: 1 },
  default: 2,
  ui: numberUi(),
};

// The board option of blockr.test that nothing requires, whose default fails.
const grain: Omit<Option, "id"> = {
  label: "Grain",
  category: "Extra options",
  schema: { type: "integer" },
  default: () => Promise.reject(new Error("Grain has no default.")),
  ui: numberUi(),
};

const viewer: Omit<PluginClass, "id"> = { options: ["zoom"] };

interface Start {
  // Whether the app sets the board lock.
  readonly locked?: boolean;
  // The board options that the board holds as it opens, as a JSON board
  // records them, those of the board of core's reply to hello unless given.
  readonly options?: Readonly<Record<string, { readonly value: Json; readonly version: number }>>;
}

// Starts core with the dock as the app's frontend, which draws the page in an
// element of the document, and stops it as the test finishes.
async function startSidebar(start: Start = {}) {
  const given: JsonBoard = HELLO.board;
  const board = start.options === undefined ? given : { ...given, board: { ...given.board, options: start.options } };
  const hello = {
    ...HELLO,
    packages: { ...HELLO.packages, "blockr.dock": "0.2.1", "blockr.test": "1.0.0" },
    plugins: {
      ...Object.fromEntries(Object.entries(HELLO.plugins).filter(([id]) => id !== BASIC)),
      [DOCK]: { name: "Dock", version: 1, module: "blockr.dock/js/dock.js" },
      [VIEWER]: { name: "Viewer", version: 1, module: "blockr.test/viewer.js" },
    },
    options: {
      ...HELLO.options,
      compact: { package: "blockr.dock", module: "blockr.dock/compact-option.js", version: 1 },
      view_tabs: { package: "blockr.dock", module: "blockr.dock/view-tabs-option.js", version: 1 },
      sync_rails: { package: "blockr.dock", module: "blockr.dock/sync-rails-option.js", version: 1 },
      zoom: { package: "blockr.test", module: "blockr.test/zoom-option.js", version: 1 },
      grain: { package: "blockr.test", module: "blockr.test/grain-option.js", version: 1 },
    },
    lock: { board: start.locked ?? false, load: false },
    board,
  };
  const modules = {
    [DOCK]: dock,
    compact: compactOption,
    view_tabs: viewTabsOption,
    sync_rails: syncRailsOption,
    [VIEWER]: { api: 1, default: viewer },
    zoom: { api: 1, default: zoom },
    grain: { api: 1, default: grain },
  };
  const el = document.createElement("div");
  document.body.append(el);
  const s = await stubCore({ hello, modules, el });
  onTestFinished(() => {
    s.stop();
    el.remove();
  });
  await s.settled();
  await drawn();
  return { ...s, el };
}

function find(root: ParentNode, selector: string): HTMLElement {
  const found = root.querySelector<HTMLElement>(selector);
  if (found === null) throw new Error(`Found nothing for ${selector}.`);
  return found;
}

const sidebarIn = (el: HTMLElement): HTMLElement => find(el, ".blockr-dock-sidebar");
const optionsButton = (el: HTMLElement): HTMLElement => find(el, ".blockr-dock-bar-options");
const isOpen = (el: HTMLElement): boolean => sidebarIn(el).hasAttribute("data-open");
const title = (el: HTMLElement): string | null => find(sidebarIn(el), ".blockr-dock-sidebar-title").textContent;

// The categories of the list, in order.
const rows = (el: HTMLElement): (string | null)[] =>
  [...sidebarIn(el).querySelectorAll<HTMLElement>(".blockr-dock-sidebar-row")].map((row) => row.textContent);

function row(el: HTMLElement, category: string): HTMLElement {
  const found = [...sidebarIn(el).querySelectorAll<HTMLElement>(".blockr-dock-sidebar-row")].find((r) => r.textContent === category);
  if (found === undefined) throw new Error(`The list has no row "${category}".`);
  return found;
}

// The ids of the board options whose UIs the page holds, in order.
const pageOptions = (el: HTMLElement): (string | undefined)[] =>
  [...sidebarIn(el).querySelectorAll<HTMLElement>(".blockr-dock-sidebar-option")].map((frame) => frame.dataset.option);

// The fields that the UIs on the page drew, in order.
const fields = (el: HTMLElement): HTMLElement[] => [
  ...sidebarIn(el).querySelectorAll<HTMLElement>(".blockr-dock-sidebar-option :is(input, select)"),
];

function button(root: ParentNode, name: string): HTMLElement {
  const found = [...root.querySelectorAll<HTMLElement>("button")].find((b) => b.getAttribute("aria-label") === name);
  if (found === undefined) throw new Error(`No button "${name}".`);
  return found;
}

// Clicks the element as a pointer does, which goes down on it first.
function click(target: Element): void {
  target.dispatchEvent(new PointerEvent("pointerdown", { bubbles: true }));
  target.dispatchEvent(new MouseEvent("click", { bubbles: true, detail: 1 }));
}

// Presses a key on the element that has the focus, and says whether something
// took it.
function press(key: string, init: KeyboardEventInit = {}): boolean {
  const target = document.activeElement ?? document.body;
  const event = new KeyboardEvent("keydown", { ...init, key, bubbles: true, cancelable: true });
  target.dispatchEvent(event);
  return event.defaultPrevented;
}

// Types the number into the field, as React hears it, and has the field
// change.
function type(field: HTMLElement, text: string): void {
  if (!(field instanceof HTMLInputElement)) throw new Error("The field is no input.");
  field.value = text;
  field.dispatchEvent(new Event("input", { bubbles: true }));
  field.dispatchEvent(new Event("change", { bubbles: true }));
}

// Opens the sidebar from the options button, which has the focus first, as a
// click on it gives it in a browser.
async function open(el: HTMLElement): Promise<void> {
  optionsButton(el).focus();
  click(optionsButton(el));
  await drawn();
}

// Opens the page of the category, and waits for core to draw the UI of each
// of its board options.
async function openPage(el: HTMLElement, category: string, count: number): Promise<void> {
  click(row(el, category));
  await vi.waitFor(() => {
    expect(fields(el)).toHaveLength(count);
  });
}

// Holds back the UIs of blockr.test's board options until the returned
// function runs, or the test finishes.
function holdBack(): () => void {
  let release = (): void => undefined;
  drawing = new Promise((resolve) => {
    release = resolve;
  });
  onTestFinished(() => {
    release();
    drawing = Promise.resolve();
  });
  return () => {
    release();
  };
}

// Counts the mounts of board options' UIs that started and ended.
function countMounts(core: CoreApi): { readonly started: string[]; readonly ended: string[] } {
  const started: string[] = [];
  const ended: string[] = [];
  const mount = core.mountOption.bind(core);
  vi.spyOn(core, "mountOption").mockImplementation((el, option) => {
    started.push(option);
    const end = mount(el, option);
    return () => {
      ended.push(option);
      end();
    };
  });
  return { started, ended };
}

describe("the options sidebar", () => {
  test("is closed and inert as the dock starts, and opens from the options button on the list of categories", async () => {
    const { el } = await startSidebar();
    const sidebar = sidebarIn(el);
    expect(isOpen(el)).toBe(false);
    expect(sidebar.hasAttribute("inert")).toBe(true);
    expect(optionsButton(el).getAttribute("aria-controls")).toBe(sidebar.id);

    await open(el);
    expect(isOpen(el)).toBe(true);
    expect(sidebar.hasAttribute("inert")).toBe(false);
    expect(optionsButton(el).getAttribute("aria-expanded")).toBe("true");
    expect(sidebar.getAttribute("role")).toBe("dialog");
    expect(sidebar.getAttribute("aria-labelledby")).toBe(find(sidebar, "h2").id);
    expect(title(el)).toBe("Board options");
    expect(document.activeElement).toBe(row(el, "Board options"));
  });

  test("lists the categories of the board options that the board holds in alphabetical order, those that a plugin class of another package requires among them", async () => {
    const { el } = await startSidebar();
    await open(el);
    expect(rows(el)).toEqual(["Board options", "Table options", "Theme options", "View options"]);
  });

  test("shows a category's page, titled with it, where core mounts the UI of each of its board options, in the alphabetical order of their labels", async () => {
    const { el, core } = await startSidebar();
    const mounts = countMounts(core);
    await open(el);

    await openPage(el, "Theme options", 3);
    expect(title(el)).toBe("Theme options");
    expect(pageOptions(el)).toEqual(["compact", "dark_mode", "view_tabs"]);
    expect(mounts.started).toEqual(["compact", "dark_mode", "view_tabs"]);

    const compact = fields(el)[0];
    expect(compact).toBeInstanceOf(HTMLInputElement);
    compact?.click();
    await vi.waitFor(() => {
      expect(core.store.board().options.compact).toBe(true);
    });
  });

  test("gives the back arrow the focus as a page opens, and the page's first field once core has drawn it", async () => {
    const { el } = await startSidebar();
    const release = holdBack();
    await open(el);
    click(row(el, "View options"));
    await drawn();
    expect(fields(el)).toEqual([]);
    expect(document.activeElement).toBe(button(sidebarIn(el), "Back"));

    release();
    await vi.waitFor(() => {
      expect(document.activeElement).toBe(fields(el)[0]);
    });
  });

  test("leaves the focus where the user moved it before the page's first field draws", async () => {
    const { el } = await startSidebar();
    const release = holdBack();
    await open(el);
    click(row(el, "View options"));
    await drawn();
    const pin = button(sidebarIn(el), "Pin sidebar");
    pin.focus();

    release();
    await vi.waitFor(() => {
      expect(fields(el)).toHaveLength(1);
    });
    expect(document.activeElement).toBe(pin);
  });

  test("returns to the list on the back arrow and on Escape, where the page's row takes the focus, and ends the page's mounts", async () => {
    const { el, core } = await startSidebar();
    const mounts = countMounts(core);
    await open(el);

    await openPage(el, "Theme options", 3);
    click(button(sidebarIn(el), "Back"));
    await drawn();
    expect(title(el)).toBe("Board options");
    expect(pageOptions(el)).toEqual([]);
    expect(mounts.ended).toEqual(["compact", "dark_mode", "view_tabs"]);
    expect(document.activeElement).toBe(row(el, "Theme options"));

    await openPage(el, "View options", 1);
    expect(press("Escape")).toBe(true);
    await drawn();
    expect(isOpen(el)).toBe(true);
    expect(rows(el)).toEqual(["Board options", "Table options", "Theme options", "View options"]);
    expect(mounts.ended).toEqual(["compact", "dark_mode", "view_tabs", "zoom"]);
    expect(document.activeElement).toBe(row(el, "View options"));
  });

  test("leaves Escape to a UI on the page that takes it, as a field that reverts what the user typed", async () => {
    const { el, core } = await startSidebar();
    await open(el);
    await openPage(el, "View options", 1);
    const [field] = fields(el);
    if (!(field instanceof HTMLInputElement)) throw new Error("No field.");

    field.focus();
    field.value = "7";
    expect(press("Escape")).toBe(true);
    await drawn();
    expect(field.value).toBe("2");
    expect(title(el)).toBe("View options");

    type(field, "3");
    await vi.waitFor(() => {
      expect(core.store.board().options.zoom).toBe(3);
    });
    press("Escape");
    await drawn();
    expect(title(el)).toBe("Board options");
  });

  test("closes on Escape on the list, on its close button and on a press outside it, and gives the focus back to the options button", async () => {
    const { el } = await startSidebar();

    await open(el);
    expect(press("Escape")).toBe(true);
    await drawn();
    expect(isOpen(el)).toBe(false);
    expect(sidebarIn(el).hasAttribute("inert")).toBe(true);
    expect(optionsButton(el).getAttribute("aria-expanded")).toBe("false");
    expect(document.activeElement).toBe(optionsButton(el));

    await open(el);
    await openPage(el, "Theme options", 3);
    click(button(sidebarIn(el), "Close sidebar"));
    await drawn();
    expect(isOpen(el)).toBe(false);
    expect(document.activeElement).toBe(optionsButton(el));

    await open(el);
    find(el, ".blockr-dock").dispatchEvent(new PointerEvent("pointerdown", { bubbles: true }));
    await drawn();
    expect(isOpen(el)).toBe(false);
  });

  test("opens again on the list, unpinned, once it has closed on a pinned page", async () => {
    const { el } = await startSidebar();
    await open(el);
    click(button(sidebarIn(el), "Pin sidebar"));
    await openPage(el, "Theme options", 3);
    click(button(sidebarIn(el), "Close sidebar"));
    await drawn();

    await open(el);
    expect(title(el)).toBe("Board options");
    expect(button(sidebarIn(el), "Pin sidebar").getAttribute("aria-pressed")).toBe("false");
  });

  test("stays open on a press on the options button, which toggles it, and on a toast, which lies over it", async () => {
    const { el, core } = await startSidebar();
    await open(el);
    optionsButton(el).dispatchEvent(new PointerEvent("pointerdown", { bubbles: true }));
    await drawn();
    expect(isOpen(el)).toBe(true);
    optionsButton(el).dispatchEvent(new MouseEvent("click", { bubbles: true, detail: 1 }));
    await drawn();
    expect(isOpen(el)).toBe(false);

    await open(el);
    core.notifications.notify(VIEWER, { severity: "message", code: "zoomed", message: "Zoomed in." });
    await drawn();
    find(el, ".blockr-dock-toasts [data-severity]").dispatchEvent(new PointerEvent("pointerdown", { bubbles: true }));
    await drawn();
    expect(isOpen(el)).toBe(true);
  });

  test("stays open while pinned, on Escape and on a press outside it, while the bar and the dock make room for it, until its close button closes it", async () => {
    const { el } = await startSidebar();
    const bar = find(el, ".blockr-dock-bar");
    const root = find(el, ".blockr-dock");
    await open(el);
    expect(bar.hasAttribute("data-pushed")).toBe(false);

    const pin = button(sidebarIn(el), "Pin sidebar");
    click(pin);
    await drawn();
    expect(pin.getAttribute("aria-pressed")).toBe("true");
    expect(bar.hasAttribute("data-pushed")).toBe(true);
    expect(root.hasAttribute("data-pushed")).toBe(true);

    row(el, "Board options").focus();
    expect(press("Escape")).toBe(false);
    root.dispatchEvent(new PointerEvent("pointerdown", { bubbles: true }));
    await drawn();
    expect(isOpen(el)).toBe(true);

    await openPage(el, "View options", 1);
    press("Escape");
    await drawn();
    expect(title(el)).toBe("Board options");
    expect(isOpen(el)).toBe(true);

    click(button(sidebarIn(el), "Close sidebar"));
    await drawn();
    expect(isOpen(el)).toBe(false);
    expect(bar.hasAttribute("data-pushed")).toBe(false);
    expect(root.hasAttribute("data-pushed")).toBe(false);
  });

  test("keeps the focus inside it on Tab and Shift+Tab, pinned or not", async () => {
    const { el } = await startSidebar();
    await open(el);
    const sidebar = sidebarIn(el);
    const pin = button(sidebar, "Pin sidebar");
    const last = row(el, "View options");

    last.focus();
    expect(press("Tab")).toBe(true);
    expect(document.activeElement).toBe(pin);
    expect(press("Tab", { shiftKey: true })).toBe(true);
    expect(document.activeElement).toBe(last);

    click(pin);
    await drawn();
    row(el, "Board options").focus();
    expect(press("Tab")).toBe(false);
    pin.focus();
    expect(press("Tab", { shiftKey: true })).toBe(true);
    expect(document.activeElement).toBe(last);
  });

  test("leaves a control that a board option's UI hides out as Tab wraps around", async () => {
    const { el } = await startSidebar();
    await open(el);
    await openPage(el, "View options", 1);
    const [field] = fields(el);
    field?.focus();
    expect(press("Tab")).toBe(true);
    expect(document.activeElement).toBe(button(sidebarIn(el), "Back"));
  });

  test("shows a board option that comes onto the board later", async () => {
    const { el, core } = await startSidebar({ options: { show_conditions: { value: ["error"], version: 1 } } });
    await open(el);
    expect(rows(el)).toEqual(["Board options", "Theme options", "View options"]);

    await core.commands.run("blockr.core-set_option", { id: "n_rows", value: 25 });
    await drawn();
    expect(rows(el)).toEqual(["Board options", "Table options", "Theme options", "View options"]);
  });

  test("returns to the list where the board no longer holds a board option of the page's category", async () => {
    const { el, core } = await startSidebar({ options: { grain: { value: 5, version: 1 } } });
    await open(el);
    expect(rows(el)).toContain("Extra options");
    await openPage(el, "Extra options", 1);

    core.store.apply({ options: { unset: ["grain"] } });
    await vi.waitFor(() => {
      expect(title(el)).toBe("Board options");
    });
    expect(rows(el)).not.toContain("Extra options");
    expect(core.store.board().options.grain).toBeUndefined();
  });

  test("shows the board options on a locked board too, where a change goes onto the board", async () => {
    const { el, core } = await startSidebar({ locked: true });
    await open(el);
    expect(rows(el)).toEqual(["Board options", "Table options", "Theme options", "View options"]);

    await openPage(el, "View options", 1);
    const [field] = fields(el);
    if (field === undefined) throw new Error("No field.");
    type(field, "4");
    await vi.waitFor(() => {
      expect(core.store.board().options.zoom).toBe(4);
    });
  });
});

// Waits for React to draw what an event changed.
async function drawn(): Promise<void> {
  await new Promise((resolve) => setTimeout(resolve, 0));
}
