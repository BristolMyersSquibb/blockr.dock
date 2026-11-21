// The dock's options sidebar, on blockr.core's CoreApi for tests, with the
// dock as the app's frontend
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.4#settings).
// Besides core's plugins and the dock, the app runs a plugin class of a
// package that only the tests know, blockr.test, which requires a board option
// of that package, zoom. The package defines two more board options, grain and
// texture, which nothing requires. They share a category that no other board
// option has. The default of grain fails.
// Where a test asks, the class requires two more, layout and shade, whose UIs
// are a menu and a select of core's React SDK.

import type { CoreApi, Json, JsonBoard, Option, OptionHost, OptionModule, PluginClass } from "@blockr/core";
import { Button, ComboBox, ComboBoxItem, Menu, MenuItem, MenuTrigger, reactModule, useValue } from "@blockr/core/react";
import type { ModuleProps } from "@blockr/core/react";
import { HELLO, stubCore } from "@blockr/core/stubs";
import type { ReactElement } from "react";
import { describe, expect, onTestFinished, test, vi } from "vitest";
import * as compactOption from "../compact-option/index.ts";
import * as syncRailsOption from "../sync-rails-option/index.ts";
import * as viewTabsOption from "../view-tabs-option/index.ts";
import * as dock from "./index.tsx";

const DOCK = "blockr.dock-dock_plugin";
const BASIC = "blockr.core-basic_frontend_plugin";
const VIEWER = "blockr.test-viewer_plugin";

// The categories of the board options of the dock and of blockr.test.
// Core puts board options of its own on the board too, for reasons of its own.
// A test that lists categories leaves out those that hold core's alone.
const OWN_CATEGORIES = ["Board options", "Extra options", "Theme options", "View options"];

// A UI that shows the board option's value in a number field, and submits the
// number that the user types as the field changes. Escape in a field whose
// number differs from the value reverts the field, and takes the key, so that
// nothing else acts on it.
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
      el.append(field);
      return () => {
        off();
        field.remove();
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

// The board option of blockr.test that nothing requires, in the category of
// grain.
const texture: Omit<Option, "id"> = {
  label: "Texture",
  category: "Extra options",
  schema: { type: "integer" },
  default: 1,
  ui: numberUi(),
};

// A UI that sets the board option's value from a menu of core's React SDK,
// which React Aria opens in a portal at the end of the page's body, over an
// element that it lays over the whole page.
function menuUi(): OptionModule<string> {
  function MenuUi({ host }: ModuleProps<OptionHost<string>>): ReactElement {
    return (
      <MenuTrigger>
        <Button variant="secondary">Layout</Button>
        <Menu
          aria-label="Layouts"
          onAction={(key) => {
            void host.submit(String(key));
          }}
        >
          <MenuItem id="grid">Grid</MenuItem>
          <MenuItem id="list">List</MenuItem>
        </Menu>
      </MenuTrigger>
    );
  }
  return reactModule(MenuUi);
}

// The board option of blockr.test whose UI is a menu, which its plugin class
// requires where a test asks.
const layout: Omit<Option, "id"> = {
  label: "Layout",
  category: "Overlay options",
  schema: { type: "string", enum: ["grid", "list"] },
  default: "grid",
  ui: menuUi(),
};

// The shades that shade's select offers.
const SHADES = [
  { id: "pale", name: "Pale" },
  { id: "deep", name: "Deep" },
];

// A UI that binds the board option's value to a select of core's React SDK,
// which React Aria opens in a portal at the end of the page's body, while the
// focus stays in its field.
function selectUi(): OptionModule<string> {
  function SelectUi({ host }: ModuleProps<OptionHost<string>>): ReactElement {
    return (
      <ComboBox bound={useValue(host)} label="Shade" defaultItems={SHADES} isRequired>
        {(item) => <ComboBoxItem id={item.id}>{item.name}</ComboBoxItem>}
      </ComboBox>
    );
  }
  return reactModule(SelectUi);
}

// The board option of blockr.test whose UI is a select, which its plugin class
// requires where a test asks, with layout.
const shade: Omit<Option, "id"> = {
  label: "Shade",
  category: "Overlay options",
  schema: { type: "string", enum: ["pale", "deep"] },
  default: "pale",
  ui: selectUi(),
};

// The plugin class of blockr.test, which names no board option itself.
// Its entry in R's reply to hello names zoom, as the method of R's
// plugin_options() for the class would.
const viewer: Omit<PluginClass, "id"> = {};

interface Start {
  // Whether the app sets the board lock.
  readonly locked?: boolean;
  // Whether the plugin class of blockr.test requires layout and shade too.
  readonly overlays?: boolean;
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
      [DOCK]: {
        name: "Dock",
        version: 1,
        options: ["compact", "view_tabs", "sync_rails", "dark_mode"],
        module: "blockr.dock/js/dock.js",
      },
      [VIEWER]: { name: "Viewer", version: 1, options: start.overlays === true ? ["zoom", "layout", "shade"] : ["zoom"], module: "blockr.test/viewer.js" },
    },
    options: {
      ...HELLO.options,
      compact: { package: "blockr.dock", module: "blockr.dock/compact-option.js", version: 1 },
      view_tabs: { package: "blockr.dock", module: "blockr.dock/view-tabs-option.js", version: 1 },
      sync_rails: { package: "blockr.dock", module: "blockr.dock/sync-rails-option.js", version: 1 },
      zoom: { package: "blockr.test", module: "blockr.test/zoom-option.js", version: 1 },
      grain: { package: "blockr.test", module: "blockr.test/grain-option.js", version: 1 },
      texture: { package: "blockr.test", module: "blockr.test/texture-option.js", version: 1 },
      layout: { package: "blockr.test", module: "blockr.test/layout-option.js", version: 1 },
      shade: { package: "blockr.test", module: "blockr.test/shade-option.js", version: 1 },
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
    texture: { api: 1, default: texture },
    layout: { api: 1, default: layout },
    shade: { api: 1, default: shade },
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

// The categories, in order, by their names, and those that are expanded.
const categoriesIn = (el: HTMLElement): HTMLDetailsElement[] => [...sidebarIn(el).querySelectorAll<HTMLDetailsElement>("details")];
const nameOf = (details: HTMLDetailsElement): string | null => find(details, ".blockr-dock-sidebar-summary-name").textContent;
const names = (el: HTMLElement): (string | null)[] => categoriesIn(el).map(nameOf);
const ownNames = (el: HTMLElement): (string | null)[] => names(el).filter((name) => name !== null && OWN_CATEGORIES.includes(name));
const expanded = (el: HTMLElement): (string | null)[] => categoriesIn(el).filter((details) => details.open).map(nameOf);

function category(el: HTMLElement, name: string): HTMLDetailsElement {
  const found = categoriesIn(el).find((details) => nameOf(details) === name);
  if (found === undefined) throw new Error(`The sidebar has no category "${name}".`);
  return found;
}

const summary = (el: HTMLElement, name: string): HTMLElement => find(category(el, name), "summary");
const summaries = (el: HTMLElement): HTMLElement[] => categoriesIn(el).map((details) => find(details, "summary"));

// The ids of the board options whose UIs the category holds, in order.
const optionsIn = (el: HTMLElement, name: string): (string | undefined)[] =>
  [...category(el, name).querySelectorAll<HTMLElement>(".blockr-dock-sidebar-option")].map((frame) => frame.dataset.option);

// The row of the board option given, which holds the option's UI and its
// Reset button, or null where no expanded category shows the option.
const rowOf = (el: HTMLElement, option: string): HTMLElement | null =>
  sidebarIn(el).querySelector<HTMLElement>(`.blockr-dock-sidebar-option[data-option="${option}"]`);

// The field that the UI of the board option given drew, or null for none.
const fieldOf = (el: HTMLElement, option: string): HTMLElement | null =>
  rowOf(el, option)?.querySelector<HTMLElement>(":is(input, select)") ?? null;

// The Reset button of the board option given, or null for none.
const resetOf = (el: HTMLElement, option: string): HTMLElement | null =>
  rowOf(el, option)?.querySelector<HTMLElement>(".blockr-dock-sidebar-reset") ?? null;

// The button whose accessible name, or else whose text, is the one given.
function button(root: ParentNode, name: string): HTMLElement {
  const found = [...root.querySelectorAll<HTMLElement>("button")].find(
    (b) => b.getAttribute("aria-label") === name || (!b.hasAttribute("aria-label") && b.textContent === name),
  );
  if (found === undefined) throw new Error(`No button "${name}".`);
  return found;
}

// The field of shade's select, its combobox.
function shadeField(el: HTMLElement): HTMLInputElement {
  const field = fieldOf(el, "shade");
  if (!(field instanceof HTMLInputElement)) throw new Error("No field of shade.");
  return field;
}

// The list of the select while it is open, which the field names in
// aria-controls.
function listOf(field: HTMLElement): HTMLElement | null {
  const id = field.getAttribute("aria-controls");
  return id === null ? null : document.getElementById(id);
}

function optionOf(list: HTMLElement, label: string): HTMLElement {
  const found = [...list.querySelectorAll<HTMLElement>('[role="option"]')].find((option) => option.textContent.trim() === label);
  if (found === undefined) throw new Error(`The list has no option "${label}".`);
  return found;
}

// The menu of layout's UI while it is open.
const layouts = (): HTMLElement | null => document.querySelector<HTMLElement>('[role="menu"][aria-label="Layouts"]');

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
function type(field: HTMLElement | null, text: string): void {
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
// the UIs of the board options given, of that category.
async function expand(el: HTMLElement, name: string, ...options: readonly string[]): Promise<void> {
  click(summary(el, name));
  await vi.waitFor(() => {
    for (const option of options) expect(fieldOf(el, option)).not.toBeNull();
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
    expect(document.activeElement).toBe(summaries(el)[0]);
  });

  test("lists the categories of the board options that the board holds in alphabetical order, those that a plugin class of another package requires among them", async () => {
    const { el } = await startSidebar({ options: { grain: { value: 5, version: 1 } } });
    await open(el);
    expect(ownNames(el)).toEqual(["Board options", "Extra options", "Theme options", "View options"]);
  });

  test("shows how many board options each category holds, which a screen reader hears as a phrase", async () => {
    // The category of grain and texture holds no other board option.
    const { el, core } = await startSidebar({ options: { grain: { value: 5, version: 1 } } });
    await open(el);
    expect(summary(el, "Extra options").textContent).toBe("Extra options1, 1 board option");

    await core.commands.run("blockr.core-set_option", { id: "texture", value: 2 });
    await drawn();
    expect(summary(el, "Extra options").textContent).toBe("Extra options2, 2 board options");
  });

  test("resets a board option to its default with the Reset button by its UI, which hands the focus to the UI", async () => {
    const { el, core } = await startSidebar({ options: { compact: { value: true, version: 1 } } });
    await open(el);
    await expand(el, "Theme options", "compact", "view_tabs");
    await vi.waitFor(() => {
      expect(resetOf(el, "compact")).not.toBeNull();
    });
    // The board holds the default of view_tabs.
    expect(resetOf(el, "view_tabs")).toBeNull();

    const reset = button(category(el, "Theme options"), "Reset Compact to its default");
    expect(reset.hasAttribute("data-rac")).toBe(true);
    expect(reset.title).toBe("Reset to the default");
    reset.focus();
    click(reset);
    await vi.waitFor(() => {
      expect(core.store.board().options.compact).toBe(false);
    });
    await drawn();
    expect(resetOf(el, "compact")).toBeNull();
    expect(document.activeElement).toBe(fieldOf(el, "compact"));
  });

  test("expands every category from its header, and collapses them all once every one is expanded", async () => {
    const { el } = await startSidebar();
    await open(el);
    const toggle = find(sidebarIn(el), ".blockr-dock-sidebar-expand");
    expect(toggle.hasAttribute("data-rac")).toBe(true);
    expect(toggle.textContent).toBe("Expand all");
    expect(toggle.getAttribute("aria-expanded")).toBe("false");

    await expand(el, "Theme options", "compact");
    expect(toggle.textContent).toBe("Expand all");
    click(toggle);
    await vi.waitFor(() => {
      expect(expanded(el)).toEqual(names(el));
    });
    expect(toggle.textContent).toBe("Collapse all");
    expect(toggle.getAttribute("aria-expanded")).toBe("true");

    click(toggle);
    await vi.waitFor(() => {
      expect(expanded(el)).toEqual([]);
    });
    expect(toggle.textContent).toBe("Expand all");
  });

  test("expands a category in place, where core mounts the UI of each of its board options, in the alphabetical order of their labels", async () => {
    const { el, core } = await startSidebar({ options: { grain: { value: 5, version: 1 }, texture: { value: 2, version: 1 } } });
    const mounts = countMounts(core);
    await open(el);

    await expand(el, "Extra options", "grain", "texture");
    expect(expanded(el)).toEqual(["Extra options"]);
    expect(optionsIn(el, "Extra options")).toEqual(["grain", "texture"]);
    expect(mounts.started).toEqual(["grain", "texture"]);

    type(fieldOf(el, "texture"), "3");
    await vi.waitFor(() => {
      expect(core.store.board().options.texture).toBe(3);
    });
  });

  test("expands several categories at once, and ends the mounts of a category as it collapses", async () => {
    const { el, core } = await startSidebar({ options: { grain: { value: 5, version: 1 }, texture: { value: 2, version: 1 } } });
    const mounts = countMounts(core);
    await open(el);

    await expand(el, "Extra options", "grain", "texture");
    await expand(el, "View options", "zoom");
    expect(expanded(el)).toEqual(["Extra options", "View options"]);

    click(summary(el, "Extra options"));
    await vi.waitFor(() => {
      expect(mounts.ended).toEqual(["grain", "texture"]);
    });
    expect(expanded(el)).toEqual(["View options"]);
    expect(optionsIn(el, "Extra options")).toEqual([]);
    expect(fieldOf(el, "zoom")).not.toBeNull();
  });

  test("keeps the UIs of a category until it has folded shut, as long as the longest transition of its content", async () => {
    const { el, core } = await startSidebar({ options: { texture: { value: 2, version: 1 } } });
    const mounts = countMounts(core);
    const computed = window.getComputedStyle.bind(window);
    vi.spyOn(window, "getComputedStyle").mockImplementation((element, pseudo) =>
      pseudo === "::details-content"
        ? ({ transitionDuration: "0.03s, 0.08s, 0s", transitionDelay: "0s" } as CSSStyleDeclaration)
        : computed(element, pseudo),
    );
    await open(el);
    await expand(el, "Extra options", "texture");

    click(summary(el, "Extra options"));
    await new Promise((resolve) => setTimeout(resolve, 40));
    expect(expanded(el)).toEqual([]);
    expect(optionsIn(el, "Extra options")).toEqual(["texture"]);
    expect(mounts.ended).toEqual([]);
    await vi.waitFor(() => {
      expect(mounts.ended).toEqual(["texture"]);
    });
    expect(optionsIn(el, "Extra options")).toEqual([]);
  });

  test("keeps its categories expanded as it closes and opens again, unpinned", async () => {
    const { el } = await startSidebar();
    await open(el);
    click(button(sidebarIn(el), "Pin sidebar"));
    await expand(el, "View options", "zoom");
    click(button(sidebarIn(el), "Close sidebar"));
    await drawn();

    await open(el);
    expect(expanded(el)).toEqual(["View options"]);
    expect(fieldOf(el, "zoom")).not.toBeNull();
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
    const close = button(sidebarIn(el), "Close sidebar");
    expect(close.hasAttribute("data-rac")).toBe(true);
    expect(close.title).toBe("Close");
    click(close);
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
    await expand(el, "View options", "zoom");
    const field = fieldOf(el, "zoom");
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

  test("stays open as the user picks from the list of a select in a board option's UI, which React Aria draws outside it, and closes on a press on the dock", async () => {
    const { el, core } = await startSidebar({ overlays: true });
    await open(el);
    await expand(el, "Overlay options", "shade");
    const field = shadeField(el);

    field.click();
    await drawn();
    const list = listOf(field);
    if (list === null) throw new Error("The select's list did not open.");
    expect(sidebarIn(el).contains(list)).toBe(false);
    const deep = optionOf(list, "Deep");
    deep.dispatchEvent(new PointerEvent("pointerdown", { bubbles: true }));
    deep.click();
    await vi.waitFor(() => {
      expect(core.store.board().options.shade).toBe("deep");
    });
    expect(isOpen(el)).toBe(true);

    // A press outside the sidebar and outside its overlays closes it, as it
    // closes the list.
    field.focus();
    press("ArrowDown");
    await drawn();
    expect(listOf(field)).not.toBeNull();
    find(el, ".blockr-dock").dispatchEvent(new PointerEvent("pointerdown", { bubbles: true }));
    await drawn();
    expect(isOpen(el)).toBe(false);
  });

  test("leaves Escape to the list of a select in a board option's UI, which closes before the sidebar", async () => {
    const { el } = await startSidebar({ overlays: true });
    await open(el);
    await expand(el, "Overlay options", "shade");
    const field = shadeField(el);

    field.focus();
    press("ArrowDown");
    await drawn();
    expect(listOf(field)).not.toBeNull();
    press("Escape");
    await drawn();
    expect(listOf(field)).toBeNull();
    expect(isOpen(el)).toBe(true);

    press("Escape");
    await drawn();
    expect(isOpen(el)).toBe(false);
  });

  test("stays open on a press on what a menu in a board option's UI lays over the page, which closes the menu, and on Escape in the menu", async () => {
    const { el, core } = await startSidebar({ overlays: true });
    await open(el);
    click(summary(el, "Overlay options"));
    const trigger = await vi.waitFor(() => button(sidebarIn(el), "Layout"));

    trigger.click();
    await drawn();
    const menu = layouts();
    if (menu === null) throw new Error("The menu did not open.");
    expect(sidebarIn(el).contains(menu)).toBe(false);
    const underlay = find(document, '[data-testid="underlay"]');
    expect(el.contains(underlay)).toBe(false);
    underlay.dispatchEvent(new PointerEvent("pointerdown", { bubbles: true }));
    underlay.dispatchEvent(new MouseEvent("click", { bubbles: true, detail: 1 }));
    await drawn();
    expect(layouts()).toBeNull();
    expect(isOpen(el)).toBe(true);

    trigger.click();
    await drawn();
    expect(layouts()).not.toBeNull();
    press("Escape");
    await drawn();
    expect(layouts()).toBeNull();
    expect(isOpen(el)).toBe(true);

    trigger.click();
    await drawn();
    const list = layouts();
    if (list === null) throw new Error("The menu did not open.");
    const item = [...list.querySelectorAll<HTMLElement>('[role="menuitem"]')].find((each) => each.textContent === "List");
    item?.dispatchEvent(new PointerEvent("pointerdown", { bubbles: true }));
    item?.click();
    await vi.waitFor(() => {
      expect(core.store.board().options.layout).toBe("list");
    });
    expect(isOpen(el)).toBe(true);
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
    const first = button(sidebarIn(el), "Expand all");
    const last = summaries(el).at(-1);
    if (last === undefined) throw new Error("The sidebar lists no category.");

    last.focus();
    expect(press("Tab")).toBe(true);
    expect(document.activeElement).toBe(first);
    expect(press("Tab", { shiftKey: true })).toBe(true);
    expect(document.activeElement).toBe(last);

    click(button(sidebarIn(el), "Pin sidebar"));
    await drawn();
    summary(el, "Board options").focus();
    expect(press("Tab")).toBe(false);
    first.focus();
    expect(press("Tab", { shiftKey: true })).toBe(true);
    expect(document.activeElement).toBe(last);
  });

  test("shows a board option that comes onto the board later", async () => {
    const { el, core } = await startSidebar();
    await open(el);
    // Nothing puts grain on the board.
    expect(ownNames(el)).toEqual(["Board options", "Theme options", "View options"]);

    await core.commands.run("blockr.core-set_option", { id: "grain", value: 5 });
    await drawn();
    expect(ownNames(el)).toEqual(["Board options", "Extra options", "Theme options", "View options"]);
  });

  test("drops a category whose board options leave the board, though the user expanded it", async () => {
    const { el, core } = await startSidebar({ options: { grain: { value: 5, version: 1 } } });
    await open(el);
    await expand(el, "Extra options", "grain");

    core.store.apply({ options: { unset: ["grain"] } });
    await vi.waitFor(() => {
      expect(names(el)).not.toContain("Extra options");
    });
    expect(core.store.board().options.grain).toBeUndefined();
  });

  test("shows the board options on a locked board too, where a change goes onto the board", async () => {
    const { el, core } = await startSidebar({ locked: true });
    await open(el);
    expect(ownNames(el)).toEqual(["Board options", "Theme options", "View options"]);

    await expand(el, "View options", "zoom");
    type(fieldOf(el, "zoom"), "4");
    await vi.waitFor(() => {
      expect(core.store.board().options.zoom).toBe(4);
    });
  });
});

// Waits for React to draw what an event changed.
async function drawn(): Promise<void> {
  await new Promise((resolve) => setTimeout(resolve, 0));
}
