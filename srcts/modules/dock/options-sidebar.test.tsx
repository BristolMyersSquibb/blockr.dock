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

// The categories of the board options that the board of core's reply to hello
// holds, once core has put on it those that the dock and blockr.test require.
const CATEGORIES = ["Board options", "Table options", "Theme options", "View options"];

// A UI that shows the board option's value in a number field, and submits the
// number that the user types as the field changes. Escape in a field whose
// number differs from the value reverts the field, and takes the key, so that
// nothing else acts on it.
// After the field, the UI holds a button that it hides, as a UI may hide a
// control until it applies.
function numberUi(): OptionModule<number> {
  return {
    render({ el, host }) {
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

// The categories, in order, each with whether it is expanded.
const categoriesIn = (el: HTMLElement): HTMLDetailsElement[] => [...sidebarIn(el).querySelectorAll<HTMLDetailsElement>("details")];
const names = (el: HTMLElement): (string | null)[] => categoriesIn(el).map((details) => find(details, "summary").textContent);
const expanded = (el: HTMLElement): (string | null)[] =>
  categoriesIn(el)
    .filter((details) => details.open)
    .map((details) => find(details, "summary").textContent);

function category(el: HTMLElement, name: string): HTMLDetailsElement {
  const found = categoriesIn(el).find((details) => find(details, "summary").textContent === name);
  if (found === undefined) throw new Error(`The sidebar has no category "${name}".`);
  return found;
}

const summary = (el: HTMLElement, name: string): HTMLElement => find(category(el, name), "summary");

// The ids of the board options whose UIs the category holds, in order.
const optionsIn = (el: HTMLElement, name: string): (string | undefined)[] =>
  [...category(el, name).querySelectorAll<HTMLElement>(".blockr-dock-sidebar-option")].map((frame) => frame.dataset.option);

// The fields that the UIs of the category's board options drew, in order.
const fields = (el: HTMLElement, name: string): HTMLElement[] => [
  ...category(el, name).querySelectorAll<HTMLElement>(".blockr-dock-sidebar-option :is(input, select)"),
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
function type(field: HTMLElement | undefined, text: string): void {
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

// Expands the category by a click on its summary, and waits for core to draw
// the UI of each of its board options.
async function expand(el: HTMLElement, name: string, count: number): Promise<void> {
  click(summary(el, name));
  await vi.waitFor(() => {
    expect(fields(el, name)).toHaveLength(count);
  });
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
  test("is closed and inert as the dock starts, and opens from the options button with every category collapsed, the first of which takes the focus", async () => {
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
    expect(find(sidebar, "h2").textContent).toBe("Board options");
    expect(expanded(el)).toEqual([]);
    expect(sidebar.querySelectorAll(".blockr-dock-sidebar-option")).toHaveLength(0);
    expect(document.activeElement).toBe(summary(el, "Board options"));
  });

  test("lists the categories of the board options that the board holds in alphabetical order, those that a plugin class of another package requires among them", async () => {
    const { el } = await startSidebar();
    await open(el);
    expect(names(el)).toEqual(CATEGORIES);
  });

  test("expands a category in place, where core mounts the UI of each of its board options, in the alphabetical order of their labels", async () => {
    const { el, core } = await startSidebar();
    const mounts = countMounts(core);
    await open(el);

    await expand(el, "Theme options", 3);
    expect(expanded(el)).toEqual(["Theme options"]);
    expect(optionsIn(el, "Theme options")).toEqual(["compact", "dark_mode", "view_tabs"]);
    expect(mounts.started).toEqual(["compact", "dark_mode", "view_tabs"]);

    fields(el, "Theme options")[0]?.click();
    await vi.waitFor(() => {
      expect(core.store.board().options.compact).toBe(true);
    });
  });

  test("expands several categories at once, and ends the mounts of a category as it collapses", async () => {
    const { el, core } = await startSidebar();
    const mounts = countMounts(core);
    await open(el);

    await expand(el, "Theme options", 3);
    await expand(el, "View options", 1);
    expect(expanded(el)).toEqual(["Theme options", "View options"]);

    click(summary(el, "Theme options"));
    await vi.waitFor(() => {
      expect(mounts.ended).toEqual(["compact", "dark_mode", "view_tabs"]);
    });
    expect(expanded(el)).toEqual(["View options"]);
    expect(optionsIn(el, "Theme options")).toEqual([]);
    expect(fields(el, "View options")).toHaveLength(1);
  });

  test("keeps the UIs of a category until it has folded shut, as long as the longest transition of its content", async () => {
    const { el, core } = await startSidebar();
    const mounts = countMounts(core);
    const computed = window.getComputedStyle.bind(window);
    vi.spyOn(window, "getComputedStyle").mockImplementation((element, pseudo) =>
      pseudo === "::details-content"
        ? ({ transitionDuration: "0.03s, 0.08s, 0s", transitionDelay: "0s" } as CSSStyleDeclaration)
        : computed(element, pseudo),
    );
    await open(el);
    await expand(el, "View options", 1);

    click(summary(el, "View options"));
    await new Promise((resolve) => setTimeout(resolve, 40));
    expect(expanded(el)).toEqual([]);
    expect(optionsIn(el, "View options")).toEqual(["zoom"]);
    expect(mounts.ended).toEqual([]);
    await vi.waitFor(() => {
      expect(mounts.ended).toEqual(["zoom"]);
    });
    expect(optionsIn(el, "View options")).toEqual([]);
  });

  test("keeps its categories expanded as it closes and opens again, unpinned", async () => {
    const { el } = await startSidebar();
    await open(el);
    click(button(sidebarIn(el), "Pin sidebar"));
    await expand(el, "View options", 1);
    click(button(sidebarIn(el), "Close sidebar"));
    await drawn();

    await open(el);
    expect(expanded(el)).toEqual(["View options"]);
    expect(fields(el, "View options")).toHaveLength(1);
    expect(button(sidebarIn(el), "Pin sidebar").getAttribute("aria-pressed")).toBe("false");
  });

  test("closes on Escape, on its close button and on a press outside it, and gives the focus back to the options button", async () => {
    const { el } = await startSidebar();

    await open(el);
    expect(press("Escape")).toBe(true);
    await drawn();
    expect(isOpen(el)).toBe(false);
    expect(sidebarIn(el).hasAttribute("inert")).toBe(true);
    expect(optionsButton(el).getAttribute("aria-expanded")).toBe("false");
    expect(document.activeElement).toBe(optionsButton(el));

    await open(el);
    click(button(sidebarIn(el), "Close sidebar"));
    await drawn();
    expect(isOpen(el)).toBe(false);
    expect(document.activeElement).toBe(optionsButton(el));

    await open(el);
    find(el, ".blockr-dock").dispatchEvent(new PointerEvent("pointerdown", { bubbles: true }));
    await drawn();
    expect(isOpen(el)).toBe(false);
  });

  test("leaves Escape to a UI in it that takes the key, as a field that reverts what the user typed", async () => {
    const { el, core } = await startSidebar();
    await open(el);
    await expand(el, "View options", 1);
    const [field] = fields(el, "View options");
    if (!(field instanceof HTMLInputElement)) throw new Error("No field.");

    field.focus();
    field.value = "7";
    expect(press("Escape")).toBe(true);
    await drawn();
    expect(field.value).toBe("2");
    expect(isOpen(el)).toBe(true);

    type(field, "3");
    await vi.waitFor(() => {
      expect(core.store.board().options.zoom).toBe(3);
    });
    press("Escape");
    await drawn();
    expect(isOpen(el)).toBe(false);
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

    summary(el, "Board options").focus();
    expect(press("Escape")).toBe(false);
    root.dispatchEvent(new PointerEvent("pointerdown", { bubbles: true }));
    await drawn();
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
    const pin = button(sidebarIn(el), "Pin sidebar");
    const last = summary(el, "View options");

    last.focus();
    expect(press("Tab")).toBe(true);
    expect(document.activeElement).toBe(pin);
    expect(press("Tab", { shiftKey: true })).toBe(true);
    expect(document.activeElement).toBe(last);

    click(pin);
    await drawn();
    summary(el, "Board options").focus();
    expect(press("Tab")).toBe(false);
    pin.focus();
    expect(press("Tab", { shiftKey: true })).toBe(true);
    expect(document.activeElement).toBe(last);
  });

  test("leaves a control that a board option's UI hides out as Tab wraps around", async () => {
    const { el } = await startSidebar();
    await open(el);
    await expand(el, "View options", 1);
    fields(el, "View options")[0]?.focus();
    expect(press("Tab")).toBe(true);
    expect(document.activeElement).toBe(button(sidebarIn(el), "Pin sidebar"));
  });

  test("shows a board option that comes onto the board later", async () => {
    const { el, core } = await startSidebar({ options: { show_conditions: { value: ["error"], version: 1 } } });
    await open(el);
    expect(names(el)).toEqual(["Board options", "Theme options", "View options"]);

    await core.commands.run("blockr.core-set_option", { id: "n_rows", value: 25 });
    await drawn();
    expect(names(el)).toEqual(CATEGORIES);
  });

  test("drops a category whose board options leave the board, though the user expanded it", async () => {
    const { el, core } = await startSidebar({ options: { grain: { value: 5, version: 1 } } });
    await open(el);
    await expand(el, "Extra options", 1);

    core.store.apply({ options: { unset: ["grain"] } });
    await vi.waitFor(() => {
      expect(names(el)).not.toContain("Extra options");
    });
    expect(core.store.board().options.grain).toBeUndefined();
  });

  test("shows the board options on a locked board too, where a change goes onto the board", async () => {
    const { el, core } = await startSidebar({ locked: true });
    await open(el);
    expect(names(el)).toEqual(CATEGORIES);

    await expand(el, "View options", 1);
    type(fields(el, "View options")[0], "4");
    await vi.waitFor(() => {
      expect(core.store.board().options.zoom).toBe(4);
    });
  });
});

// Waits for React to draw what an event changed.
async function drawn(): Promise<void> {
  await new Promise((resolve) => setTimeout(resolve, 0));
}
