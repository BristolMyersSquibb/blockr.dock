// The views menu on the bar, with its folders, its manage mode and the line
// of tabs, on blockr.core's CoreApi for tests
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.5#the-docks-own-parts).

import type { CoreApi, Json, JsonBoard } from "@blockr/core";
import { stubCore } from "@blockr/core/stubs";
import { describe, expect, onTestFinished, test } from "vitest";
import type { DockState, Folder, FolderId, View } from "../../state/index.ts";
import { DOCK, hello as dockHello, modules } from "./testing.ts";

function view(name: string, folder: FolderId | null, index: string): View {
  return { name, folder, index, grid: { orientation: "horizontal", children: [], sizes: [] } };
}

function folder(name: string, parent: FolderId | null, index: string): Folder {
  return { name, folder: parent, index };
}

// The page's board: Overview at the top, then the folder Safety, which holds
// Adverse events and the folder Nested, which holds Labs, and then Listing.
// Adverse events is the active view.
const navigation: DockState = {
  views: {
    overview: view("Overview", null, "a0"),
    ae: view("Adverse events", "safety", "a0"),
    labs: view("Labs", "nested", "a0"),
    listing: view("Listing", null, "a2"),
  },
  folders: { safety: folder("Safety", null, "a1"), nested: folder("Nested", "safety", "a1") },
  active: "ae",
  visible: {},
};

interface StartOptions {
  readonly state?: DockState;
  readonly tabs?: boolean;
  readonly locked?: boolean;
  // The navbar items as R writes them, none for the default bar.
  readonly navbar?: readonly Json[];
}

// Starts core with the dock as the app's frontend on a board that holds the
// dock's state given, the page's unless given, and stops it as the test
// finishes.
async function startViews(options: StartOptions = {}) {
  const board: JsonBoard = {
    format: 1,
    packages: { "blockr.core": "0.2.0", "blockr.dock": "0.2.1" },
    board: {
      id: "curious_otter",
      name: "Curious otter",
      blocks: {},
      links: {},
      stacks: {},
      options: {
        dark_mode: { value: "light", version: 1 },
        compact: { value: false, version: 1 },
        view_tabs: { value: options.tabs ?? false, version: 1 },
        sync_rails: { value: true, version: 1 },
      },
      plugins: { [DOCK]: { state: options.state ?? navigation, version: 1 } },
    },
  };
  const frontend: Record<string, Json> = options.navbar === undefined ? {} : { navbar: [...options.navbar] };
  const hello = { ...dockHello, lock: { board: options.locked ?? false, load: false }, frontend, board };
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

// The dock's state on the board.
function stateOf(core: CoreApi): DockState {
  return core.store.board().plugins[DOCK] as DockState;
}

function bar(el: HTMLElement): HTMLElement {
  const found = el.querySelector<HTMLElement>(".blockr-dock-bar");
  if (found === null) throw new Error("The dock draws no bar.");
  return found;
}

// The button of the views menu on the bar.
function viewsButton(el: HTMLElement): HTMLButtonElement {
  const found = bar(el).querySelector<HTMLButtonElement>(".blockr-dock-views-button, .blockr-dock-views-chevron");
  if (found === null) throw new Error("The bar draws no views menu.");
  return found;
}

// The open menu or manage mode whose label is given.
function popover(el: HTMLElement, label: string): HTMLElement {
  const found = el.querySelector<HTMLElement>(`[popover][aria-label="${label}"]`);
  if (found === null) throw new Error(`No menu "${label}".`);
  return found;
}

// The rows of the views and folders of a menu, each as its name, indented by
// two spaces for each folder that holds it, with a folder's name after "+"
// while it is closed and "-" while it is open, and "*" after the active
// view's.
function treeRows(menu: HTMLElement): string[] {
  const rows = [...menu.querySelectorAll<HTMLElement>(".blockr-dock-menu-row")];
  return rows.filter((row) => row.style.getPropertyValue("--depth") !== "").map((row) => {
    const depth = Number(row.style.getPropertyValue("--depth"));
    const fold = row.getAttribute("aria-expanded");
    const mark = fold === null ? "" : fold === "true" ? "- " : "+ ";
    const active = row.getAttribute("aria-checked") === "true" ? " *" : "";
    return `${"  ".repeat(depth)}${mark}${row.textContent}${active}`;
  });
}

// The rows of the menu in manage mode, each as its name, indented by two
// spaces for each folder that holds it.
function managedRows(menu: HTMLElement): string[] {
  return [...menu.querySelectorAll<HTMLElement>(".blockr-dock-views-managed")].map((row) => {
    const depth = Number(row.style.getPropertyValue("--depth"));
    return `${"  ".repeat(depth)}${row.textContent}`;
  });
}

// The row of a menu whose text is given.
function row(menu: HTMLElement, text: string): HTMLElement {
  const found = [...menu.querySelectorAll<HTMLElement>(".blockr-dock-menu-row")].find((candidate) => candidate.textContent === text);
  if (found === undefined) throw new Error(`No row "${text}".`);
  return found;
}

// The text of the row that the keyboard is on, the menu's active descendant.
function activeRow(menu: HTMLElement): string | undefined {
  const id = menu.getAttribute("aria-activedescendant");
  return id === null ? undefined : (menu.querySelector(`[id="${id}"]`)?.textContent ?? undefined);
}

// The button inside the element whose label or text is given.
function button(root: ParentNode, name: string): HTMLButtonElement {
  const found = [...root.querySelectorAll<HTMLButtonElement>("button")].find(
    (b) => b.getAttribute("aria-label") === name || b.textContent === name,
  );
  if (found === undefined) throw new Error(`No button "${name}".`);
  return found;
}

// The menu's row in manage mode of the view or folder whose name is given.
function managed(menu: HTMLElement, name: string): HTMLElement {
  const found = [...menu.querySelectorAll<HTMLElement>(".blockr-dock-views-managed")].find(
    (candidate) => candidate.querySelector(".blockr-dock-views-name")?.textContent === name,
  );
  if (found === undefined) throw new Error(`No row "${name}".`);
  return found;
}

// Opens the views menu with a click.
async function openMenu(el: HTMLElement): Promise<HTMLElement> {
  click(viewsButton(el));
  await drawn();
  return popover(el, "Views");
}

// Opens the views menu and turns it to manage mode.
async function openManage(el: HTMLElement): Promise<HTMLElement> {
  const menu = await openMenu(el);
  click(row(menu, "Manage views"));
  await drawn();
  return popover(el, "Manage views");
}

describe("the views menu", () => {
  test("shows the active view's name on the bar, with a rule after it", async () => {
    const { el } = await startViews();

    const views = viewsButton(el);
    expect(views.textContent).toBe("Adverse events");
    expect(views.getAttribute("aria-label")).toBe("Views: Adverse events");
    expect(views.getAttribute("aria-haspopup")).toBe("menu");
    expect(views.querySelector(".blockr-dock-chevron")).not.toBeNull();
    expect(views.parentElement?.lastElementChild?.className).toBe("blockr-dock-bar-rule");
    expect(bar(el).dataset.tabs).toBeUndefined();
  });

  test("lists the views and folders in the order of the navigation, with the folders that hold the active view open", async () => {
    const { el } = await startViews();
    const menu = await openMenu(el);

    expect(menu.getAttribute("role")).toBe("menu");
    expect(treeRows(menu)).toEqual(["Overview", "- Safety", "  Adverse events *", "  + Nested", "Listing"]);
    expect(row(menu, "Adverse events").getAttribute("role")).toBe("menuitemradio");
    // Every row of the views leaves room for a folder's chevron, so that the
    // names line up, while the rows after them leave none.
    expect(row(menu, "Overview").querySelector(".blockr-dock-menu-fold")).not.toBeNull();
    expect(row(menu, "Manage views").querySelector(".blockr-dock-menu-fold")).toBeNull();
    const rows = [...menu.querySelectorAll(".blockr-dock-menu-row")].map((r) => r.textContent);
    expect(rows.slice(-2)).toEqual(["Manage views", "Show views as tabs"]);
    expect(row(menu, "Manage views").previousElementSibling?.className).toBe("blockr-dock-menu-divider");
  });

  test("opens and closes a folder in place, and keeps which folders are open on the screen, out of the board", async () => {
    const { el, core } = await startViews();
    const before = core.store.board();
    let menu = await openMenu(el);

    click(row(menu, "Nested"));
    await drawn();
    expect(treeRows(menu)).toEqual(["Overview", "- Safety", "  Adverse events *", "  - Nested", "    Labs", "Listing"]);
    // The menu stays open.
    expect(viewsButton(el).getAttribute("aria-expanded")).toBe("true");

    click(row(menu, "Safety"));
    await drawn();
    expect(treeRows(menu)).toEqual(["Overview", "+ Safety", "Listing"]);

    // The menu opens again with the folders that hold the active view open,
    // and Nested still open inside Safety.
    click(viewsButton(el));
    await drawn();
    menu = await openMenu(el);
    expect(treeRows(menu)).toEqual(["Overview", "- Safety", "  Adverse events *", "  - Nested", "    Labs", "Listing"]);

    expect(core.store.board()).toBe(before);
    expect(core.store.canUndo()).toBe(false);
  });

  test("switches views on a pick, with no undo step, and closes", async () => {
    const { el, core } = await startViews();
    const menu = await openMenu(el);

    click(row(menu, "Listing"));
    await drawn();
    expect(stateOf(core).active).toBe("listing");
    expect(core.store.canUndo()).toBe(false);
    expect(viewsButton(el).getAttribute("aria-expanded")).toBe("false");
    expect(viewsButton(el).textContent).toBe("Listing");
  });

  test("switches views under the board lock, which leaves out managing them and the row of the tabs", async () => {
    const { el, core } = await startViews({ locked: true });
    const menu = await openMenu(el);

    expect(treeRows(menu)).toEqual(["Overview", "- Safety", "  Adverse events *", "  + Nested", "Listing"]);
    expect(menu.textContent).not.toContain("Manage views");
    expect(menu.textContent).not.toContain("Show views as tabs");
    click(row(menu, "Overview"));
    await drawn();
    expect(stateOf(core).active).toBe("overview");
  });

  test("moves through its rows with the keys of a menu, and opens and closes a folder with ArrowRight and ArrowLeft", async () => {
    const { el } = await startViews();
    viewsButton(el).focus();
    press("ArrowDown");
    await drawn();
    const menu = popover(el, "Views");

    // The keyboard opens the menu on the active view, while the focus stays on
    // the menu.
    expect(document.activeElement).toBe(menu);
    expect(activeRow(menu)).toBe("Adverse events");
    const step = async (key: string) => {
      press(key);
      await drawn();
    };
    await step("ArrowDown");
    expect(activeRow(menu)).toBe("Nested");
    await step("ArrowRight");
    expect(treeRows(menu)).toContain("    Labs");
    expect(activeRow(menu)).toBe("Nested");
    await step("ArrowRight");
    expect(activeRow(menu)).toBe("Labs");
    await step("ArrowLeft");
    expect(activeRow(menu)).toBe("Nested");
    await step("ArrowLeft");
    expect(treeRows(menu)).not.toContain("    Labs");
    await step("ArrowLeft");
    expect(activeRow(menu)).toBe("Safety");
    await step("Home");
    expect(activeRow(menu)).toBe("Overview");
    await step("ArrowUp");
    expect(activeRow(menu)).toBe("Show views as tabs");
    await step("ArrowUp");
    await step("Enter");
    expect(popover(el, "Manage views").getAttribute("role")).toBe("dialog");
  });

  test("turns the line of tabs on and off from the row Show views as tabs, whose check shows while it is on", async () => {
    const { el, core } = await startViews();
    let menu = await openMenu(el);
    const toggle = row(menu, "Show views as tabs");
    expect(toggle.getAttribute("role")).toBe("menuitemcheckbox");
    expect(toggle.getAttribute("aria-checked")).toBe("false");
    expect(toggle.querySelector(".blockr-dock-check")).toBeNull();

    click(toggle);
    await drawn();
    expect(core.store.board().options.view_tabs).toBe(true);
    expect(viewsButton(el).getAttribute("aria-expanded")).toBe("false");

    menu = await openMenu(el);
    expect(row(menu, "Show views as tabs").getAttribute("aria-checked")).toBe("true");
    expect(row(menu, "Show views as tabs").querySelector(".blockr-dock-check")).not.toBeNull();
    click(row(menu, "Show views as tabs"));
    await drawn();
    expect(core.store.board().options.view_tabs).toBe(false);
  });
});

describe("the views menu in manage mode", () => {
  test("turns on from Manage views, and back to the list on Done or as the menu closes", async () => {
    const { el } = await startViews();
    let menu = await openManage(el);

    expect(menu.getAttribute("role")).toBe("dialog");
    expect(managedRows(menu)).toEqual(["Overview", "Safety", "  Adverse events", "  Nested", "Listing"]);
    expect(document.activeElement).toBe(menu);
    expect([...menu.querySelectorAll(".blockr-dock-views-foot button")].map((b) => b.textContent)).toEqual([
      "New view",
      "New folder",
      "Done",
    ]);

    click(button(menu, "Done"));
    await drawn();
    menu = popover(el, "Views");
    expect(menu.getAttribute("role")).toBe("menu");
    expect(treeRows(menu)).toContain("Overview");

    click(row(menu, "Manage views"));
    await drawn();
    expect(popover(el, "Manage views").getAttribute("role")).toBe("dialog");
    click(viewsButton(el));
    await drawn();
    menu = await openMenu(el);
    expect(menu.getAttribute("role")).toBe("menu");

    // Escape closes manage mode, and the focus goes back to the button.
    click(row(menu, "Manage views"));
    await drawn();
    pressIn(popover(el, "Manage views"), "Escape");
    await drawn();
    expect(el.querySelector('[aria-label="Manage views"]')).toBeNull();
    expect(document.activeElement).toBe(viewsButton(el));
  });

  test("adds a view last at the top, makes it active, and opens its name in a field that renames it", async () => {
    const { el, core } = await startViews();
    const menu = await openManage(el);

    click(button(menu, "New view"));
    await drawn();
    const state = stateOf(core);
    const added = state.active ?? "";
    expect(state.views[added]).toMatchObject({ name: "View 5", folder: null });
    expect(managedRows(menu).at(-1)).toBe("");
    const field = menu.querySelector<HTMLInputElement>(".blockr-dock-views-field");
    expect(field?.value).toBe("View 5");
    expect(document.activeElement).toBe(field);

    if (field === null) throw new Error("No field.");
    type(field, "Demographics");
    // Enter's default goes, since the keypress that follows would click the
    // name, which takes the focus back, and open the field again.
    expect(pressIn(field, "Enter").defaultPrevented).toBe(true);
    await drawn();
    expect(stateOf(core).views[added]?.name).toBe("Demographics");
    expect(managedRows(menu).at(-1)).toBe("Demographics");
    // The keyboard stays on the row.
    expect(document.activeElement?.textContent).toBe("Demographics");
    expect(viewsButton(el).textContent).toBe("Demographics");
  });

  test("adds a folder last at the top, and opens its name in a field", async () => {
    const { el, core } = await startViews();
    const menu = await openManage(el);

    click(button(menu, "New folder"));
    await drawn();
    const added = Object.keys(stateOf(core).folders).find((id) => id !== "safety" && id !== "nested") ?? "";
    expect(stateOf(core).folders[added]).toMatchObject({ name: "Folder 3", folder: null });
    const field = menu.querySelector<HTMLInputElement>(".blockr-dock-views-field");
    expect(field?.value).toBe("Folder 3");
    if (field === null) throw new Error("No field.");
    type(field, "Efficacy");
    field.blur();
    await drawn();
    expect(stateOf(core).folders[added]?.name).toBe("Efficacy");
    expect(stateOf(core).active).toBe("ae");
  });

  test("renames a view or a folder on a click on its name, and gives up on Escape or an empty name, while the menu stays open", async () => {
    const { el, core } = await startViews();
    const menu = await openManage(el);
    const name = managed(menu, "Safety").querySelector<HTMLElement>(".blockr-dock-views-name");
    name?.dispatchEvent(new PointerEvent("pointerover", { bubbles: true }));
    expect(name?.title).toBe("Click to rename");

    click(name);
    await drawn();
    let field = menu.querySelector<HTMLInputElement>(".blockr-dock-views-field");
    if (field === null) throw new Error("No field.");
    expect(field.getAttribute("aria-label")).toBe("Folder name");
    type(field, "Safety tables");
    pressIn(field, "Enter");
    await drawn();
    expect(stateOf(core).folders.safety?.name).toBe("Safety tables");

    click(managed(menu, "Listing").querySelector(".blockr-dock-views-name"));
    await drawn();
    field = menu.querySelector<HTMLInputElement>(".blockr-dock-views-field");
    if (field === null) throw new Error("No field.");
    type(field, "Listings");
    const escape = pressIn(field, "Escape");
    await drawn();
    // Escape gives up the edit alone, and keeps the menu open.
    expect(escape.defaultPrevented).toBe(true);
    expect(stateOf(core).views.listing?.name).toBe("Listing");
    expect(menu.querySelector(".blockr-dock-views-field")).toBeNull();

    click(managed(menu, "Listing").querySelector(".blockr-dock-views-name"));
    await drawn();
    field = menu.querySelector<HTMLInputElement>(".blockr-dock-views-field");
    if (field === null) throw new Error("No field.");
    type(field, "   ");
    pressIn(field, "Enter");
    await drawn();
    expect(stateOf(core).views.listing?.name).toBe("Listing");
    expect(viewsButton(el).getAttribute("aria-expanded")).toBe("true");
  });

  test("removes a view or a folder once its row asks, and gives the question up on Escape", async () => {
    const { el, core } = await startViews();
    const menu = await openManage(el);

    click(button(managed(menu, "Listing"), "Remove Listing"));
    await drawn();
    const asking = menu.querySelector<HTMLElement>("[data-confirming]");
    expect(asking?.textContent).toBe("Remove “Listing”?Remove");
    expect(document.activeElement?.textContent).toBe("Remove");
    pressIn(document.activeElement, "Escape");
    await drawn();
    expect(menu.querySelector("[data-confirming]")).toBeNull();
    expect(Object.keys(stateOf(core).views)).toContain("listing");

    click(button(managed(menu, "Listing"), "Remove Listing"));
    await drawn();
    click(button(menu, "Remove"));
    await drawn();
    expect(Object.keys(stateOf(core).views)).not.toContain("listing");
    expect(core.store.canUndo()).toBe(true);

    // Removing a folder moves what it holds up, in its place.
    click(button(managed(menu, "Safety"), "Remove Safety"));
    await drawn();
    click(button(menu, "Remove"));
    await drawn();
    expect(managedRows(menu)).toEqual(["Overview", "Adverse events", "Nested"]);
  });

  test("offers no cross for the last view, which the dock does not remove", async () => {
    const lone: DockState = { views: { only: view("Only", null, "a0") }, folders: {}, active: "only", visible: {} };
    const { el } = await startViews({ state: lone });
    const menu = await openManage(el);

    expect(managedRows(menu)).toEqual(["Only"]);
    expect(managed(menu, "Only").querySelector(".blockr-dock-views-remove")).toBeNull();
  });

  test("moves a view or a folder where a drag by its grip lets go, as one undo step, and marks where it would land", async () => {
    const { el, core } = await startViews();
    const menu = await openManage(el);
    stackRows(menu);

    // Listing goes into Safety, over the middle of Safety's row, the second.
    const grip = gripOf(managed(menu, "Listing"));
    grip.dispatchEvent(new PointerEvent("pointerdown", { bubbles: true, button: 0, pointerId: 1, clientY: 150 }));
    await drawn();
    expect(managed(menu, "Listing").dataset.dragging).toBe("");
    grip.dispatchEvent(new PointerEvent("pointermove", { bubbles: true, pointerId: 1, clientY: 51 }));
    await drawn();
    expect(managed(menu, "Safety").dataset.drop).toBe("into");
    grip.dispatchEvent(new PointerEvent("pointerup", { bubbles: true, pointerId: 1, clientY: 51 }));
    await drawn();
    expect(stateOf(core).views.listing?.folder).toBe("safety");
    expect(managedRows(menu)).toEqual(["Overview", "Safety", "  Adverse events", "  Nested", "  Listing"]);
    expect(menu.querySelector("[data-drop]")).toBeNull();
    expect(menu.querySelector("[data-dragging]")).toBeNull();

    core.store.undo();
    await drawn();
    expect(stateOf(core).views.listing).toEqual(navigation.views.listing);
  });

  test("moves nothing where the drag lets go where its view or folder already is, or a folder into itself", async () => {
    const { el, core } = await startViews();
    const menu = await openManage(el);
    stackRows(menu);
    const before = core.store.board();

    const grip = gripOf(managed(menu, "Safety"));
    grip.dispatchEvent(new PointerEvent("pointerdown", { bubbles: true, button: 0, pointerId: 1, clientY: 50 }));
    grip.dispatchEvent(new PointerEvent("pointermove", { bubbles: true, pointerId: 1, clientY: 51 }));
    await drawn();
    expect(menu.querySelector("[data-drop]")).toBeNull();
    grip.dispatchEvent(new PointerEvent("pointermove", { bubbles: true, pointerId: 1, clientY: 119 }));
    await drawn();
    expect(menu.querySelector("[data-drop]")).toBeNull();
    grip.dispatchEvent(new PointerEvent("pointerup", { bubbles: true, pointerId: 1, clientY: 119 }));
    await drawn();
    expect(core.store.board()).toBe(before);
  });
});

describe("the line of tabs", () => {
  test("draws a tab for each view and folder at the top, then the menu as a chevron alone, and grows the bar over the dock", async () => {
    const { el } = await startViews({ tabs: true });

    const line = bar(el).querySelector<HTMLElement>(".blockr-dock-bar-views");
    expect(line?.dataset.tabs).toBe("");
    expect([...(line?.querySelectorAll(".blockr-dock-tab") ?? [])].map((tab) => tab.textContent)).toEqual([
      "Overview",
      "Safety",
      "Listing",
    ]);
    const chevron = viewsButton(el);
    expect(chevron.className).toContain("blockr-dock-views-chevron");
    expect(chevron.textContent).toBe("");
    expect(chevron.getAttribute("aria-label")).toBe("Views");
    expect(line?.querySelector(".blockr-dock-bar-rule")).toBeNull();
    expect(bar(el).dataset.tabs).toBe("");
    expect(el.querySelector<HTMLElement>(".blockr-dock")?.dataset.tabs).toBe("");
  });

  test("switches views from a view's tab, and marks the tab of the active view or of the folder that holds it", async () => {
    const { el, core } = await startViews({ tabs: true });
    const tab = (name: string) => button(bar(el).querySelector(".blockr-dock-tabs") ?? el, name);

    expect(tab("Safety").getAttribute("aria-current")).toBe("true");
    expect(tab("Overview").getAttribute("aria-current")).toBeNull();
    click(tab("Listing"));
    await drawn();
    expect(stateOf(core).active).toBe("listing");
    expect(tab("Listing").getAttribute("aria-current")).toBe("page");
    expect(tab("Safety").getAttribute("aria-current")).toBeNull();
    expect(core.store.canUndo()).toBe(false);
  });

  test("opens from a folder's tab a menu of what the folder holds, in which folders open and close", async () => {
    const { el, core } = await startViews({ tabs: true });
    const safety = button(bar(el).querySelector(".blockr-dock-tabs") ?? el, "Safety");
    expect(safety.getAttribute("aria-haspopup")).toBe("menu");

    click(safety);
    await drawn();
    const menu = popover(el, "Safety");
    expect(safety.getAttribute("aria-expanded")).toBe("true");
    expect(treeRows(menu)).toEqual(["Adverse events *", "+ Nested"]);
    click(row(menu, "Nested"));
    await drawn();
    expect(treeRows(menu)).toEqual(["Adverse events *", "- Nested", "  Labs"]);

    // The views menu shows Nested open too.
    click(viewsButton(el));
    await drawn();
    expect(treeRows(popover(el, "Views"))).toContain("    Labs");
    click(viewsButton(el));
    await drawn();

    click(safety);
    await drawn();
    click(row(popover(el, "Safety"), "Labs"));
    await drawn();
    expect(stateOf(core).active).toBe("labs");
    expect(safety.getAttribute("aria-expanded")).toBe("false");
    expect(safety.getAttribute("aria-current")).toBe("true");
  });

  test("leaves the bar and the dock as they are where the bar holds no views menu", async () => {
    const navbar: Json[] = [
      { kind: "part", id: "logo" },
      { kind: "part", id: "options" },
    ];
    const { el } = await startViews({ tabs: true, navbar });

    expect(bar(el).querySelector(".blockr-dock-tabs")).toBeNull();
    expect(bar(el).dataset.tabs).toBeUndefined();
    expect(el.querySelector<HTMLElement>(".blockr-dock")?.dataset.tabs).toBeUndefined();
  });
});

// Has each row of the menu in manage mode sit 34 pixels below the one before,
// from the top of the window, as a browser lays them out, which happy-dom
// does not.
function stackRows(menu: HTMLElement): void {
  menu.querySelectorAll<HTMLElement>(".blockr-dock-views-managed").forEach((managedRow, i) => {
    managedRow.getBoundingClientRect = () => new DOMRect(0, 34 * i, 220, 34);
  });
}

function gripOf(managedRow: HTMLElement): HTMLElement {
  const grip = managedRow.querySelector<HTMLElement>(".blockr-dock-views-grip");
  if (grip === null) throw new Error("No grip.");
  return grip;
}

// Clicks the element as a pointer does, which goes down on it first.
function click(target: Element | null | undefined): void {
  if (target === null || target === undefined) throw new Error("Nothing to click.");
  target.dispatchEvent(new PointerEvent("pointerdown", { bubbles: true }));
  target.dispatchEvent(new MouseEvent("click", { bubbles: true, detail: 1 }));
}

// Presses a key in the element that has the focus.
function press(key: string): void {
  pressIn(document.activeElement, key);
}

// Presses a key in the element given, and gives the event.
function pressIn(target: Element | null, key: string): KeyboardEvent {
  if (target === null) throw new Error("Nothing to press a key in.");
  const event = new KeyboardEvent("keydown", { key, bubbles: true, cancelable: true });
  target.dispatchEvent(event);
  return event;
}

// Types the text into the field, as React hears it.
function type(field: HTMLInputElement, text: string): void {
  Object.getOwnPropertyDescriptor(HTMLInputElement.prototype, "value")?.set?.call(field, text);
  field.dispatchEvent(new Event("input", { bubbles: true }));
}

// Waits for React to draw what an event changed.
async function drawn(): Promise<void> {
  await new Promise((resolve) => setTimeout(resolve, 0));
}
