// The card that the dock draws in each block's panel, on blockr.core's
// CoreApi for tests, with stand-ins for the modules of core's block classes:
// the UI of the rbind block draws nothing, as core's does, and that of every
// other class draws a line.

import type { BlockId, JsonBoard } from "@blockr/core";
import { HELLO, stubCore } from "@blockr/core/stubs";
import { describe, expect, onTestFinished, test, vi } from "vitest";
import type { DockState } from "../../../state/index.ts";
import { DOCK, hello, modules } from "../testing.ts";
import { menuSpot, tooltipSpot } from "./place.ts";
import { grouped } from "./picker.tsx";

// A block's UI that draws a line, and one that draws nothing.
const drawsLine = {
  api: 1,
  default: {
    render({ el }: { el: HTMLElement }) {
      const line = el.ownerDocument.createElement("p");
      line.textContent = "The block's controls";
      el.append(line);
      return undefined;
    },
  },
};
const drawsNothing = { api: 1, default: { render: () => undefined } };

// The stand-ins, by the module that each class of core's registry names,
// and a renderer that draws nothing for the output of each class.
const blockModules: Readonly<Record<string, unknown>> = Object.fromEntries(
  Object.values(HELLO.block_registry).flatMap((entry): [string, unknown][] => [
    [entry.module, entry.id === "blockr.core-rbind_block" ? drawsNothing : drawsLine],
    ...(entry.renderer === undefined ? [] : [[entry.renderer.module, drawsNothing] as [string, unknown]]),
  ]),
);

// The dock's state with one view, which shows Iris (a), Head (b), in front of
// Merge (c), and Bind (e) side by side, with the Edit board plugin on the
// left rail.
const shown: DockState = {
  views: {
    main: {
      name: "Main",
      folder: null,
      index: "a0",
      grid: {
        orientation: "horizontal",
        children: [
          { panels: [{ block: "a" }], active: { block: "a" } },
          { panels: [{ block: "b" }, { block: "c" }], active: { block: "b" } },
          { panels: [{ block: "e" }], active: { block: "e" } },
        ],
        sizes: [0.25, 0.25, 0.5],
        rails: {
          left: {
            panels: [{ plugin: "blockr.core-edit_board_plugin" }],
            active: { plugin: "blockr.core-edit_board_plugin" },
            collapsed: false,
            size: 260,
          },
        },
      },
    },
  },
  folders: {},
  active: "main",
  visible: {},
};

// The board of core's fixture, with the dock's state given and the locks of
// its blocks given.
function boardWith(state: DockState, locked: Readonly<Record<BlockId, boolean | null>> = {}): JsonBoard {
  const { board } = HELLO.board;
  return {
    ...HELLO.board,
    packages: { ...HELLO.board.packages, "blockr.dock": "0.2.1" },
    board: {
      ...board,
      versions: { ...board.versions, [DOCK]: 1 },
      blocks: Object.fromEntries(
        Object.entries(board.blocks).map(([id, block]) => [id, { ...block, locked: locked[id] ?? block.locked }]),
      ),
      plugins: { [DOCK]: state },
    },
  };
}

// Starts core with the dock drawing the board given in an element of the
// page, under the board lock where asked.
async function startCards(board: JsonBoard = boardWith(shown), lock = false) {
  const el = document.createElement("div");
  document.body.append(el);
  const s = await stubCore({
    hello: { ...hello, board, ...(lock ? { lock: { board: true, load: false } } : {}) },
    modules: { ...modules, ...blockModules },
    el,
  });
  onTestFinished(() => {
    s.stop();
    el.remove();
  });
  await s.settled();
  return { ...s, el };
}

// The card of the block of the name given.
function cardOf(el: HTMLElement, name: string): HTMLElement {
  const card = el.querySelector<HTMLElement>(`.blockr-card[aria-label="${name}"]`);
  if (card === null) throw new Error(`No card shows ${name}.`);
  return card;
}

// The labels of a card's tools.
function tools(card: HTMLElement): (string | null)[] {
  return [...card.querySelectorAll(".blockr-card-tool")].map((tool) => tool.getAttribute("aria-label"));
}

function toolOf(card: HTMLElement, label: string): HTMLButtonElement {
  const tool = card.querySelector<HTMLButtonElement>(`.blockr-card-tool[aria-label="${label}"]`);
  if (tool === null) throw new Error(`The card has no tool ${label}.`);
  return tool;
}

function sectionOf(card: HTMLElement, name: string): HTMLElement | null {
  return card.querySelector<HTMLElement>(`.blockr-card-section[data-section="${name}"]`);
}

// The menu or the "+" menu that is open, in the dock's layer.
function openMenu(el: HTMLElement): HTMLElement | null {
  return el.querySelector<HTMLElement>(".blockr-dock-layer .blockr-card-menu");
}

function rows(menu: HTMLElement): string[] {
  return [...menu.querySelectorAll(".blockr-card-menu-label")].map((label) => label.textContent);
}

function rowOf(menu: HTMLElement, label: string): HTMLElement {
  const row = [...menu.querySelectorAll<HTMLElement>(".blockr-card-menu-row")].find(
    (each) => each.querySelector(".blockr-card-menu-label")?.textContent === label,
  );
  if (row === undefined) throw new Error(`The menu has no row ${label}.`);
  return row;
}

const press = (target: Element, key: string) => {
  target.dispatchEvent(new KeyboardEvent("keydown", { key, bubbles: true, cancelable: true }));
};

const pointer = (target: Element, type: string) => {
  target.dispatchEvent(new PointerEvent(type, { bubbles: true, cancelable: true, pointerType: "mouse" }));
};

// A click of the pointer, whose detail counts the clicks, while a click that
// the keyboard makes, as click() does, has none.
const clickOn = (target: Element) => {
  target.dispatchEvent(new MouseEvent("click", { bubbles: true, cancelable: true, detail: 1 }));
};

// Types `value` into a field, as React reads it.
function typeInto(field: HTMLInputElement, value: string): void {
  Object.getOwnPropertyDescriptor(HTMLInputElement.prototype, "value")?.set?.call(field, value);
  field.dispatchEvent(new Event("input", { bubbles: true }));
}

const visibleOf = (core: { store: { board(): { plugins: Readonly<Record<string, unknown>> } } }) =>
  (core.store.board().plugins[DOCK] as DockState).visible;

describe("a block's card", () => {
  test("draws the block's mark, with its class and package, its name and its tools", async () => {
    const { el, stop } = await startCards();

    const card = cardOf(el, "Head");
    const mark = card.querySelector(".blockr-card-mark");
    expect(mark?.getAttribute("aria-label")).toBe("head/tail block, blockr.core");
    expect(mark?.getAttribute("data-category")).toBe("transform");
    expect(mark?.querySelector("svg")).not.toBeNull();
    expect(card.querySelector(".blockr-card-name")?.textContent).toBe("Head");
    expect(tools(card)).toEqual(["Controls", "Preview", "Locked for readers", "More actions"]);
    stop();
  });

  test("mounts the block's UI in its inputs and its output in its outputs, until the panel goes", async () => {
    const { core, el, settled } = await startCards();

    const card = cardOf(el, "Head");
    const ui = sectionOf(card, "inputs")?.querySelector('[role="region"][aria-label="Inputs"]');
    expect(ui?.textContent).toBe("The block's controls");
    expect(sectionOf(card, "outputs")?.querySelector('[role="region"][aria-label="Output"]')).not.toBeNull();

    await core.commands.run("blockr.core-remove_block", { id: "b" });
    await settled();
    expect(el.querySelector('.blockr-card[aria-label="Head"]')).toBeNull();
    expect(ui?.childNodes).toHaveLength(0);
  });

  test("keeps the cards of the panels behind a tab mounted", async () => {
    const { core, el, settled } = await startCards();

    const ui = cardOf(el, "Head").querySelector('[aria-label="Inputs"]');
    await core.commands.run("blockr.dock-activate_panel", { view: "main", panel: { block: "c" } });
    await settled();
    expect(cardOf(el, "Merge")).toBeDefined();
    expect(ui?.textContent).toBe("The block's controls");
  });

  test("gives no Controls toggle to a block whose UI draws nothing, whose inputs take no room", async () => {
    const { el } = await startCards();

    const card = cardOf(el, "Bind");
    expect(tools(card)).toEqual(["Preview", "Locked for readers", "More actions"]);
    expect(sectionOf(card, "inputs")?.hidden).toBe(true);
    expect(sectionOf(card, "outputs")?.hidden).toBe(false);
  });

  test("shows the sections as the dock's visible says, both where it holds no entry for the block", async () => {
    const { el } = await startCards(boardWith({ ...shown, visible: { b: ["outputs"] } }));

    const head = cardOf(el, "Head");
    expect(sectionOf(head, "inputs")?.hidden).toBe(true);
    expect(sectionOf(head, "outputs")?.hidden).toBe(false);
    expect(toolOf(head, "Controls").getAttribute("aria-pressed")).toBe("false");
    expect(toolOf(head, "Preview").getAttribute("aria-pressed")).toBe("true");

    const iris = cardOf(el, "Iris");
    expect(sectionOf(iris, "inputs")?.hidden).toBe(false);
    expect(sectionOf(iris, "outputs")?.hidden).toBe(false);
  });

  test("sets the sections with blockr.dock-set_sections as a toggle is clicked, as one undo step", async () => {
    const { core, el, settled } = await startCards();

    const card = cardOf(el, "Head");
    toolOf(card, "Preview").click();
    await settled();
    expect(visibleOf(core)).toEqual({ b: ["inputs"] });
    expect(sectionOf(card, "outputs")?.hidden).toBe(true);
    expect(sectionOf(card, "inputs")?.getAttribute("data-open")).toBe("true");
    expect(toolOf(card, "Preview").getAttribute("aria-pressed")).toBe("false");

    toolOf(card, "Controls").click();
    await settled();
    expect(visibleOf(core)).toEqual({ b: [] });
    expect(sectionOf(card, "inputs")?.hidden).toBe(true);

    core.store.undo();
    await settled();
    expect(visibleOf(core)).toEqual({ b: ["inputs"] });
    expect(sectionOf(card, "inputs")?.hidden).toBe(false);
  });

  test("folds a closing section before it hides it, and keeps it where it opens again before the fold ends", async () => {
    const { el, settled } = await startCards();

    const card = cardOf(el, "Head");
    const outputs = sectionOf(card, "outputs");
    if (outputs === null) throw new Error("The card has no outputs.");
    // The transitions of two folds, which end as the test says.
    const closing = Promise.withResolvers<undefined>();
    const opening = Promise.withResolvers<undefined>();
    const transition = (finished: Promise<undefined>) => ({ transitionProperty: "grid-template-rows", finished }) as unknown as Animation;
    const animations = vi.spyOn(outputs, "getAnimations").mockReturnValue([transition(closing.promise)]);

    toolOf(card, "Preview").click();
    await settled();
    expect(outputs.getAttribute("data-open")).toBe("false");
    expect(outputs.hasAttribute("data-folding")).toBe(true);
    expect(outputs.hidden).toBe(false);

    animations.mockReturnValue([transition(opening.promise)]);
    toolOf(card, "Preview").click();
    await settled();
    closing.resolve(undefined);
    await settled();
    expect(outputs.getAttribute("data-open")).toBe("true");
    expect(outputs.hidden).toBe(false);

    opening.resolve(undefined);
    await settled();
    expect(outputs.hasAttribute("data-folding")).toBe(false);
    expect(outputs.hidden).toBe(false);

    animations.mockReturnValue([]);
    toolOf(card, "Preview").click();
    await settled();
    expect(outputs.hidden).toBe(true);
    expect(outputs.hasAttribute("data-folding")).toBe(false);
  });

  test("shows the padlock closed while the block follows the board lock, and opens it with blockr.core-set_block_lock", async () => {
    const { core, el, settled } = await startCards();

    const card = cardOf(el, "Head");
    expect(toolOf(card, "Locked for readers").getAttribute("aria-pressed")).toBe("false");
    toolOf(card, "Locked for readers").click();
    await settled();
    expect(core.store.board().blocks.b?.locked).toBe(false);
    expect(toolOf(card, "Open to readers").getAttribute("aria-pressed")).toBe("true");

    toolOf(card, "Open to readers").click();
    await settled();
    expect(core.store.board().blocks.b?.locked).toBeNull();
  });

  test("turns the name into a field on a double-click, which renames the block on Enter, as one undo step", async () => {
    const { core, el, settled } = await startCards();

    const card = cardOf(el, "Head");
    card.querySelector(".blockr-card-name")?.dispatchEvent(new MouseEvent("dblclick", { bubbles: true }));
    await settled();
    const field = card.querySelector<HTMLInputElement>(".blockr-card-name-field");
    if (field === null) throw new Error("No field renames the block.");
    expect(field.value).toBe("Head");
    expect(document.activeElement).toBe(field);
    expect([field.selectionStart, field.selectionEnd]).toEqual([0, 4]);

    typeInto(field, "First rows");
    press(field, "Enter");
    await settled();
    expect(core.store.board().blocks.b?.name).toBe("First rows");
    expect(card.querySelector(".blockr-card-name")?.textContent).toBe("First rows");

    core.store.undo();
    await settled();
    expect(core.store.board().blocks.b?.name).toBe("Head");
  });

  test("keeps the name on Escape, refuses an empty name on Enter and drops it on a click elsewhere", async () => {
    const { core, el, settled } = await startCards();

    const card = cardOf(el, "Head");
    const rename = async () => {
      card.querySelector(".blockr-card-name")?.dispatchEvent(new MouseEvent("dblclick", { bubbles: true }));
      await settled();
      const field = card.querySelector<HTMLInputElement>(".blockr-card-name-field");
      if (field === null) throw new Error("No field renames the block.");
      return field;
    };

    const first = await rename();
    typeInto(first, "Gone");
    press(first, "Escape");
    await settled();
    expect(card.querySelector(".blockr-card-name-field")).toBeNull();
    expect(core.store.board().blocks.b?.name).toBe("Head");

    const second = await rename();
    typeInto(second, "  ");
    press(second, "Enter");
    await settled();
    expect(card.querySelector(".blockr-card-name-error")?.textContent).toBe("A block needs a name");
    expect(second.getAttribute("aria-invalid")).toBe("true");
    second.blur();
    await settled();
    expect(card.querySelector(".blockr-card-name-field")).toBeNull();
    expect(core.store.board().blocks.b?.name).toBe("Head");
    expect(core.store.canUndo()).toBe(false);
  });

  test("turns the board option compact into a compact header", async () => {
    const { core, el, settled } = await startCards();

    expect(cardOf(el, "Head").hasAttribute("data-compact")).toBe(false);
    await core.commands.run("blockr.core-set_option", { id: "compact", value: true });
    await settled();
    expect(cardOf(el, "Head").hasAttribute("data-compact")).toBe(true);
  });
});

describe("a block's menu", () => {
  test("opens on a click with Rename, Append block, Copy block ID and Remove block", async () => {
    const { el, settled } = await startCards();

    const button = toolOf(cardOf(el, "Head"), "More actions");
    clickOn(button);
    await settled();
    const menu = openMenu(el);
    if (menu === null) throw new Error("No menu opened.");
    expect(menu.getAttribute("role")).toBe("menu");
    expect(rows(menu)).toEqual(["Rename", "Append block", "Copy block ID", "Remove block"]);
    expect(rowOf(menu, "Copy block ID").querySelector(".blockr-card-menu-meta")?.textContent).toBe("b");
    expect(button.getAttribute("aria-expanded")).toBe("true");
    expect(document.activeElement).toBe(menu);
    expect(menu.hasAttribute("aria-activedescendant")).toBe(false);

    press(menu, "Escape");
    await settled();
    expect(openMenu(el)).toBeNull();
    expect(document.activeElement).toBe(button);
  });

  test("opens on ArrowDown on its first row, which the arrows move and Enter picks", async () => {
    const { el, settled } = await startCards();

    const card = cardOf(el, "Head");
    const button = toolOf(card, "More actions");
    button.focus();
    press(button, "ArrowDown");
    await settled();
    const menu = openMenu(el);
    if (menu === null) throw new Error("No menu opened.");
    const active = () => document.getElementById(menu.getAttribute("aria-activedescendant") ?? "")?.textContent;
    expect(active()).toBe("Rename");
    press(menu, "ArrowUp");
    await settled();
    expect(active()).toBe("Remove block");
    press(menu, "Home");
    await settled();
    expect(active()).toBe("Rename");

    press(menu, "Enter");
    await settled();
    expect(openMenu(el)).toBeNull();
    expect(document.activeElement).toBe(card.querySelector(".blockr-card-name-field"));
  });

  test("closes on a click elsewhere", async () => {
    const { el, settled } = await startCards();

    toolOf(cardOf(el, "Head"), "More actions").click();
    await settled();
    pointer(cardOf(el, "Iris"), "pointerdown");
    await settled();
    expect(openMenu(el)).toBeNull();
  });

  test("removes the block with Remove block", async () => {
    const { core, el, settled } = await startCards();

    toolOf(cardOf(el, "Head"), "More actions").click();
    await settled();
    const menu = openMenu(el);
    if (menu === null) throw new Error("No menu opened.");
    rowOf(menu, "Remove block").click();
    await settled();
    expect(Object.hasOwn(core.store.board().blocks, "b")).toBe(false);
  });

  test("puts the block's id on the clipboard with Copy block ID", async () => {
    const { el, settled } = await startCards();
    const written = vi.spyOn(navigator.clipboard, "writeText").mockResolvedValue();
    onTestFinished(() => {
      written.mockRestore();
    });

    toolOf(cardOf(el, "Head"), "More actions").click();
    await settled();
    const menu = openMenu(el);
    if (menu === null) throw new Error("No menu opened.");
    rowOf(menu, "Copy block ID").click();
    await settled();
    expect(written).toHaveBeenCalledWith("b");
    expect(openMenu(el)).toBeNull();
  });

  test("appends a block of the class picked in the dock's + menu, as blockr.core-append_block does", async () => {
    const { core, el, settled } = await startCards();

    const before = Object.keys(core.store.board().blocks);
    toolOf(cardOf(el, "Head"), "More actions").click();
    await settled();
    const menu = openMenu(el);
    if (menu === null) throw new Error("No menu opened.");
    rowOf(menu, "Append block").click();
    await settled();

    const picker = openMenu(el);
    if (picker === null) throw new Error("No + menu opened.");
    expect(picker.querySelector(".blockr-card-menu-caption")?.textContent).toBe("Append to Head");
    // Only classes that take a link, so neither of the input blocks.
    expect(rows(picker)).toEqual(["scatter plot block", "head/tail block", "merge block", "rbind block", "subset block"]);
    expect([...picker.querySelectorAll(".blockr-card-menu-title")].map((title) => title.textContent)).toEqual(["plot", "transform"]);

    const search = picker.querySelector<HTMLInputElement>('input[role="combobox"]');
    if (search === null) throw new Error("The + menu has no search.");
    expect(document.activeElement).toBe(search);
    typeInto(search, "scatter");
    await settled();
    expect(rows(picker)).toEqual(["scatter plot block"]);
    press(search, "Enter");
    await settled();

    const board = core.store.board();
    const added = Object.keys(board.blocks).filter((id) => !before.includes(id));
    expect(added.map((id) => board.blocks[id]?.registry_id)).toEqual(["blockr.core-scatter_block"]);
    expect(Object.values(board.links)).toContainEqual({ from: "b", to: added[0], input: "data" });
  });

  test("links a block of a class with several inputs from its first input", async () => {
    const { core, el, settled } = await startCards();

    toolOf(cardOf(el, "Head"), "More actions").click();
    await settled();
    const menu = openMenu(el);
    if (menu === null) throw new Error("No menu opened.");
    rowOf(menu, "Append block").click();
    await settled();
    const picker = openMenu(el);
    if (picker === null) throw new Error("No + menu opened.");
    rowOf(picker, "merge block").click();
    await settled();

    const links = Object.values(core.store.board().links).filter((link) => link.from === "b");
    expect(links.map((link) => link.input)).toContain("x");
  });
});

describe("focus", () => {
  test("goes to a block as the user clicks into its card or onto its tab", async () => {
    const { core, el } = await startCards();

    expect(core.focus.get()).toBeNull();
    pointer(cardOf(el, "Head"), "pointerdown");
    expect(core.focus.get()).toBe("b");

    const tab = [...el.querySelectorAll(".dv-default-tab")].find((each) => each.textContent.trim() === "Bind");
    if (tab === undefined) throw new Error("Bind has no tab.");
    pointer(tab, "pointerdown");
    expect(core.focus.get()).toBe("e");
  });

  test("goes to a block as the keyboard's focus moves into its card", async () => {
    const { core, el } = await startCards();

    toolOf(cardOf(el, "Iris"), "Preview").focus();
    expect(core.focus.get()).toBe("a");
  });

  test("stays where it is on a click into a plugin's panel or onto its tab", async () => {
    const { core, el } = await startCards();

    pointer(cardOf(el, "Head"), "pointerdown");
    const panel = el.querySelector('.blockr-dock-panel[data-plugin="blockr.core-edit_board_plugin"]');
    const tab = [...el.querySelectorAll(".dv-default-tab")].find((each) => each.textContent.trim() === "Edit board");
    if (panel === null || tab === undefined) throw new Error("Edit board shows no panel.");
    pointer(panel, "pointerdown");
    pointer(tab, "pointerdown");
    expect(core.focus.get()).toBe("b");
  });

  test("does not go to a block as the pointer moves over its card", async () => {
    const { core, el } = await startCards();

    pointer(cardOf(el, "Head"), "pointerover");
    expect(core.focus.get()).toBeNull();
  });
});

describe("the card on a locked board", () => {
  test("has no toggle, no padlock and no rename, and Copy block ID alone in its menu", async () => {
    const { el, settled } = await startCards(boardWith(shown), true);

    const card = cardOf(el, "Head");
    expect(tools(card)).toEqual(["More actions"]);
    card.querySelector(".blockr-card-name")?.dispatchEvent(new MouseEvent("dblclick", { bubbles: true }));
    await settled();
    expect(card.querySelector(".blockr-card-name-field")).toBeNull();

    toolOf(card, "More actions").click();
    await settled();
    const menu = openMenu(el);
    if (menu === null) throw new Error("No menu opened.");
    expect(rows(menu)).toEqual(["Copy block ID"]);
  });

  test("mounts the UI only of a block that the board lock leaves open, whatever visible says", async () => {
    const { el } = await startCards(boardWith({ ...shown, visible: { a: ["outputs"] } }, { a: false, b: false }), true);

    expect(sectionOf(cardOf(el, "Head"), "inputs")?.textContent).toBe("The block's controls");
    expect(sectionOf(cardOf(el, "Iris"), "inputs")?.hidden).toBe(true);
    expect(sectionOf(cardOf(el, "Bind"), "inputs")).toBeNull();
    expect(sectionOf(cardOf(el, "Bind"), "outputs")?.hidden).toBe(false);
  });
});

describe("the card's tooltips", () => {
  const wait = (ms: number) =>
    new Promise((resolve) => {
      setTimeout(resolve, ms);
    });
  const tooltip = (el: HTMLElement) => el.querySelector('.blockr-dock-layer [role="tooltip"]');

  test("show a tool's name after the pointer rests 300ms, and leave as the pointer does", async () => {
    const { el, settled } = await startCards();

    const preview = toolOf(cardOf(el, "Head"), "Preview");
    pointer(preview, "pointerover");
    await wait(100);
    expect(tooltip(el)).toBeNull();
    await wait(300);
    expect(tooltip(el)?.textContent).toBe("Preview");

    pointer(preview, "pointerout");
    await wait(150);
    await settled();
    expect(tooltip(el)).toBeNull();
  });

  test("show at once on the keyboard's focus, and leave on Escape", async () => {
    const { el, settled } = await startCards();

    toolOf(cardOf(el, "Head"), "More actions").focus();
    await settled();
    expect(tooltip(el)?.textContent).toBe("More actions");
    press(document.body, "Escape");
    await settled();
    expect(tooltip(el)).toBeNull();
  });

  test("give the mark the name of the block's class, with its package as a badge", async () => {
    const { el } = await startCards();

    const mark = cardOf(el, "Head").querySelector(".blockr-card-mark");
    if (mark === null) throw new Error("The card has no mark.");
    pointer(mark, "pointerover");
    await wait(350);
    expect(tooltip(el)?.querySelector(".blockr-card-tooltip-badge")?.textContent).toBe("blockr.core");
    expect(tooltip(el)?.textContent).toBe("head/tail block blockr.core");
  });
});

describe("the placement of a menu and of a tooltip", () => {
  const view = { width: 800, height: 600 };
  const trigger = { left: 700, top: 100, right: 726, bottom: 126, width: 26 };

  test("puts a menu 4px below its trigger, lined up with its right edge", () => {
    expect(menuSpot(trigger, { width: 200, height: 150 }, view)).toEqual({ left: 526, top: 130 });
  });

  test("puts a menu above its trigger where it fits there but not below", () => {
    const low = { ...trigger, top: 500, bottom: 526 };
    expect(menuSpot(low, { width: 200, height: 150 }, view)).toEqual({ left: 526, top: 346 });
  });

  test("keeps a menu 8px from the window's edges", () => {
    const left = { left: 10, top: 100, right: 36, bottom: 126, width: 26 };
    expect(menuSpot(left, { width: 200, height: 150 }, view).left).toBe(8);
  });

  test("puts a tooltip 6px above its element, centred on it, and below where there is no room above", () => {
    expect(tooltipSpot(trigger, { width: 60, height: 24 }, view)).toEqual({ left: 683, top: 70 });
    const top = { ...trigger, top: 10, bottom: 36 };
    expect(tooltipSpot(top, { width: 60, height: 24 }, view).top).toBe(42);
  });
});

describe("the + menu's classes", () => {
  const entries = Object.values(HELLO.block_registry);

  test("come by category, then by name, with the classes of no category last", () => {
    const groups = grouped([...entries, { ...entries[0], id: "blockr.test-thing_block", name: "a thing", category: "" } as (typeof entries)[0]], "");
    expect(groups.map((group) => group.category)).toEqual(["input", "plot", "transform", "uncategorized"]);
    expect(groups.find((group) => group.category === "transform")?.entries.map((entry) => entry.name)).toEqual([
      "head/tail block",
      "merge block",
      "rbind block",
      "subset block",
    ]);
  });

  test("are those whose name, id, category or keywords hold every word searched", () => {
    expect(grouped(entries, "Block MERGE").flatMap((group) => group.entries.map((entry) => entry.id))).toEqual(["blockr.core-merge_block"]);
    expect(grouped(entries, "plot").flatMap((group) => group.entries.map((entry) => entry.id))).toEqual(["blockr.core-scatter_block"]);
    expect(grouped(entries, "nothing like it")).toEqual([]);
  });
});
