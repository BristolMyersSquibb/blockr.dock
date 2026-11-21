// The card that the dock draws in each block's panel, on blockr.core's
// CoreApi for tests, with the stand-ins for the modules of core's block
// classes that ../testing.ts gives: the UI of the rbind block draws nothing,
// as core's does, and that of every other class draws a line.

import type { BlockId, JsonBoard } from "@blockr/core";
import { HELLO, stubCore } from "@blockr/core/stubs";
import type { StubCoreOptions } from "@blockr/core/stubs";
import { describe, expect, onTestFinished, test, vi } from "vitest";
import type { DockState } from "../../../state/index.ts";
import { DOCK, hello, modules } from "../testing.ts";
import { matches } from "../picker.tsx";
import { menuSpot, tooltipSpot } from "../place.ts";
import { classGroups } from "./classes.tsx";

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
      blocks: Object.fromEntries(
        Object.entries(board.blocks).map(([id, block]) => [id, { ...block, locked: locked[id] ?? block.locked }]),
      ),
      plugins: { [DOCK]: { state, version: 1 } },
    },
  };
}

// What R does otherwise than in core's fixture: how it replies to an
// evaluation, and the block registry that it gives.
interface Played {
  readonly evaluate?: StubCoreOptions["evaluate"];
  readonly registry?: typeof hello.block_registry;
}

// Starts core with the dock drawing the board given in an element of the
// page, under the board lock where asked, with R played as given.
async function startCards(board: JsonBoard = boardWith(shown), lock = false, played: Played = {}) {
  const el = document.createElement("div");
  document.body.append(el);
  const s = await stubCore({
    hello: {
      ...hello,
      board,
      ...(lock ? { lock: { board: true, load: false } } : {}),
      ...(played.registry === undefined ? {} : { block_registry: played.registry }),
    },
    modules,
    ...(played.evaluate === undefined ? {} : { evaluate: played.evaluate }),
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

// The block's menu that is open, which React Aria draws in a portal at the end
// of the page's body.
function blockMenu(): HTMLElement | null {
  return document.querySelector<HTMLElement>('[role="menu"][aria-label="More actions"]');
}

// The "+" menu that is open, one of the dock's pickers, which core's React SDK
// draws in a portal at the end of the page's body.
function plusMenu(): HTMLElement | null {
  return document.querySelector<HTMLElement>(".blockr-dock-picker");
}

function rows(menu: HTMLElement): string[] {
  return [...menu.querySelectorAll(".blockr-dock-picker-label")].map((label) => label.textContent);
}

// Waits for the search of the "+" menu to take the focus, which React Aria
// moves a frame after a click without a pointer, as click() makes.
async function focusedSearch(menu: HTMLElement): Promise<HTMLInputElement> {
  return await vi.waitFor(() => {
    const search = menu.querySelector("input");
    if (search === null || document.activeElement !== search) throw new Error("The search of the + menu has not taken the focus.");
    return search;
  });
}

// What names the element to assistive technology through the attribute given,
// as aria-labelledby or aria-describedby, or else its text.
function named(el: Element, attribute: string): string {
  const ids = (el.getAttribute(attribute) ?? "").split(" ").filter((id) => id !== "");
  if (ids.length === 0) return el.textContent.trim();
  return ids.map((id) => el.ownerDocument.getElementById(id)?.textContent.trim() ?? "").join(" ");
}

// The items of the block's menu, by their labels, in order.
function items(menu: HTMLElement): string[] {
  return [...menu.querySelectorAll('[role="menuitem"]')].map((item) => named(item, "aria-labelledby"));
}

function itemOf(menu: HTMLElement, label: string): HTMLElement {
  const item = [...menu.querySelectorAll<HTMLElement>('[role="menuitem"]')].find((each) => named(each, "aria-labelledby") === label);
  if (item === undefined) throw new Error(`The menu has no item ${label}.`);
  return item;
}

// Opens the block's menu of the card as the pointer does, which React Aria
// opens as the pointer goes down on the button.
async function pressMenu(card: HTMLElement, settled: () => Promise<void>): Promise<HTMLElement> {
  const button = toolOf(card, "More actions");
  mouse(button, "pointerdown");
  mouse(button, "pointerup");
  clickOn(button);
  await settled();
  const menu = blockMenu();
  if (menu === null) throw new Error("No menu opened.");
  return menu;
}

function rowOf(menu: HTMLElement, label: string): HTMLElement {
  const row = [...menu.querySelectorAll<HTMLElement>(".blockr-dock-picker-row")].find(
    (each) => each.querySelector(".blockr-dock-picker-label")?.textContent === label,
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

// A pointer event of a mouse that has a size and a pressure, which React Aria
// tells from the event that a screen reader makes.
const mouse = (target: Element, type: string) => {
  target.dispatchEvent(
    new PointerEvent(type, { bubbles: true, cancelable: true, pointerType: "mouse", pointerId: 1, width: 1, height: 1, pressure: 0.5, detail: 1 }),
  );
};

// A click of the pointer, whose detail counts the clicks, while a click that
// the keyboard makes, as click() does, has none.
const clickOn = (target: Element) => {
  target.dispatchEvent(new MouseEvent("click", { bubbles: true, cancelable: true, detail: 1 }));
};

// Types `value` into a field, as React reads it.
// The browser first tells React Aria that the user inserts text, by the event
// beforeinput, on which React Aria has the first row that a search keeps take
// the virtual focus.
function typeInto(field: HTMLInputElement, value: string): void {
  field.dispatchEvent(new InputEvent("beforeinput", { inputType: "insertText", data: value, bubbles: true, cancelable: true }));
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

// The parts of a card that show the block's status and the notifications
// about it.
const dotOf = (card: HTMLElement) => card.querySelector<HTMLElement>(".blockr-card-dot");
const outputOf = (card: HTMLElement) => card.querySelector<HTMLElement>(".blockr-card-output-frame");
const noteOf = (card: HTMLElement) => card.querySelector(".blockr-card-status-note")?.textContent ?? null;
const listed = (list: Element | null): [string | null, string][] =>
  [...(list?.querySelectorAll(".blockr-card-notification") ?? [])].map((item) => [
    item.getAttribute("data-severity"),
    item.textContent,
  ]);
const errors = (card: HTMLElement) => listed(card.querySelector('[aria-label="Errors"]'));
const foldOf = (card: HTMLElement) => sectionOf(card, "outputs")?.querySelector("details") ?? null;

// A condition of R's reply to an evaluation of a block.
const condition = (severity: "error" | "warning" | "message", message: string) => ({
  severity,
  class: [severity === "message" ? "simpleMessage" : `simple${severity === "error" ? "Error" : "Warning"}`, severity, "condition"],
  phase: "block_eval" as const,
  message,
});

describe("a block's status on its card", () => {
  test("shows in the status dot on the block's mark, filled for a ready block and a ring for one not yet evaluated", async () => {
    const { el } = await startCards();

    // Merge sits behind Head, so nothing demands it.
    const head = dotOf(cardOf(el, "Head"));
    expect([head?.getAttribute("data-status"), head?.getAttribute("aria-label"), head?.hasAttribute("data-ring")]).toEqual([
      "ready",
      "Up to date",
      false,
    ]);
    const merge = dotOf(cardOf(el, "Merge"));
    expect([merge?.getAttribute("data-status"), merge?.getAttribute("aria-label"), merge?.hasAttribute("data-ring")]).toEqual([
      "unevaluated",
      "Not evaluated yet",
      true,
    ]);
    expect(outputOf(cardOf(el, "Merge"))?.hidden).toBe(false);
  });

  test("fades the output of a stale block, until the block is ready again", async () => {
    const { core, el, settled } = await startCards();
    const show = async (block: BlockId) => {
      await core.commands.run("blockr.dock-activate_panel", { view: "main", panel: { block } });
      await settled();
    };

    // Merge runs while it shows, and goes stale behind Head, where nothing
    // demands it.
    await show("c");
    await show("b");
    core.scheduler.invalidate("c");
    await settled();
    const merge = cardOf(el, "Merge");
    expect(dotOf(merge)?.getAttribute("data-status")).toBe("stale");
    expect(outputOf(merge)?.hasAttribute("data-faded")).toBe(true);

    await show("c");
    expect(dotOf(merge)?.getAttribute("data-status")).toBe("ready");
    expect(outputOf(merge)?.hasAttribute("data-faded")).toBe(false);
  });

  test("hides the output of a block that failed, whose error shows above the sections as text", async () => {
    const { el } = await startCards(boardWith(shown), false, {
      evaluate: (request) =>
        request.block === "b"
          ? { status: "error", conditions: [condition("error", "Column \u001b[1m\u001b[34mSpecies\u001b[39m\u001b[22m is gone.")] }
          : undefined,
    });

    const head = cardOf(el, "Head");
    expect(dotOf(head)?.getAttribute("data-status")).toBe("failed");
    expect(errors(head)).toEqual([["error", "Column Species is gone."]]);
    expect([...head.children].map((child) => child.className)).toEqual([
      "blockr-card-header",
      "blockr-card-notifications",
      "blockr-card-sections",
    ]);
    expect(outputOf(head)?.hidden).toBe(true);
    // Nothing else shows in the outputs, which then take no room.
    expect(sectionOf(head, "outputs")?.hidden).toBe(true);
  });

  test("hides the output of an invalid block and of the blocks waiting on it, with the status note in its place", async () => {
    const { el } = await startCards(boardWith(shown), false, {
      evaluate: (request) => (request.block === "b" ? { status: "invalid", reason: "Choose how many rows." } : undefined),
    });

    const head = cardOf(el, "Head");
    expect(dotOf(head)?.getAttribute("data-status")).toBe("invalid");
    expect(dotOf(head)?.hasAttribute("data-ring")).toBe(false);
    expect(outputOf(head)?.hidden).toBe(true);
    expect(noteOf(head)).toBe("Choose how many rows.");
    expect(sectionOf(head, "outputs")?.hidden).toBe(false);

    // Bind reads Head.
    const bind = cardOf(el, "Bind");
    expect(dotOf(bind)?.getAttribute("data-status")).toBe("waiting");
    expect(dotOf(bind)?.hasAttribute("data-ring")).toBe(true);
    expect(outputOf(bind)?.hidden).toBe(true);
    expect(noteOf(bind)).toBe('This block is waiting for block "b".');
    expect(errors(bind)).toEqual([]);
  });

  test("keeps the output of a ready block whose output R could not make, while the dot reads failed", async () => {
    // R has no output to give for Head, whose renderer has no fixtures.
    const entry = hello.block_registry["blockr.core-head_block"];
    const renderer = entry?.renderers?.[0];
    if (entry === undefined || renderer === undefined) throw new Error("The fixture has no head block with a renderer.");
    const { el } = await startCards(boardWith(shown), false, {
      registry: {
        ...hello.block_registry,
        "blockr.core-head_block": { ...entry, renderers: [{ ...renderer, id: "blockr.dock-no_fixtures" }] },
      },
    });

    const head = cardOf(el, "Head");
    expect(dotOf(head)?.getAttribute("data-status")).toBe("failed");
    expect(dotOf(head)?.getAttribute("aria-label")).toBe("Evaluation failed");
    expect(errors(head)).toEqual([["error", 'The fixtures hold no output of block "b".']]);
    expect(outputOf(head)?.hidden).toBe(false);
  });
});

describe("the notifications on a block's card", () => {
  const raising = (block: BlockId, conditions: ReturnType<typeof condition>[]): Played => ({
    evaluate: (request) => (request.block === block ? { conditions } : undefined),
  });
  const warned = [
    condition("message", "Reading \u001b[1miris\u001b[22m."),
    condition("warning", "Dropped 2 rows."),
    condition("message", "Done."),
  ];

  test("fold the warnings and the messages under the output behind their counts, the warnings first", async () => {
    const { el } = await startCards(boardWith(shown), false, raising("b", warned));

    const head = cardOf(el, "Head");
    expect(errors(head)).toEqual([]);
    expect(dotOf(head)?.getAttribute("data-status")).toBe("ready");
    const fold = foldOf(head);
    expect(fold?.open).toBe(false);
    expect(fold?.querySelector("summary")?.textContent).toBe("1 warning, 2 messages");
    expect(listed(fold)).toEqual([
      ["warning", "Dropped 2 rows."],
      ["message", "Reading iris."],
      ["message", "Done."],
    ]);
  });

  test("leave the card as core withdraws them", async () => {
    let conditions = warned;
    const { core, el, settled } = await startCards(boardWith(shown), false, {
      evaluate: (request) => (request.block === "b" ? { conditions } : undefined),
    });

    expect(listed(foldOf(cardOf(el, "Head")))).toHaveLength(3);
    conditions = [];
    core.scheduler.invalidate("b");
    await settled();
    expect(foldOf(cardOf(el, "Head"))).toBeNull();
  });

  test("hold what a plugin raises about the block too, which shows as no toast", async () => {
    const { core, el, settled } = await startCards();

    const withdraw = core.notifications.notify("blockr.core-edit_board_plugin", {
      severity: "error",
      message: "The edit was refused.",
      code: "edit_refused",
      about: { block: "b" },
    });
    await settled();
    expect(errors(cardOf(el, "Head"))).toEqual([["error", "The edit was refused."]]);
    expect(el.querySelector(".blockr-dock-toast")).toBeNull();

    withdraw();
    await settled();
    expect(errors(cardOf(el, "Head"))).toEqual([]);
  });
});

describe("a block's menu", () => {
  test("opens on a press, at the end of the page's body, with Rename, Append block, Copy block ID and, after a divider, Remove block", async () => {
    const { core, el, settled } = await startCards();

    const card = cardOf(el, "Head");
    const button = toolOf(card, "More actions");
    // The button and the menu are core's React SDK's, React Aria's.
    expect(button.hasAttribute("data-rac")).toBe(true);
    const menu = await pressMenu(card, settled);
    expect(el.contains(menu)).toBe(false);
    expect(items(menu)).toEqual(["Rename", "Append block", "Copy block ID", "Remove block"]);
    const copy = itemOf(menu, "Copy block ID");
    expect(copy.querySelector(".blockr-card-menu-meta")?.textContent).toBe("b");
    expect(named(copy, "aria-describedby")).toBe("b");
    const remove = itemOf(menu, "Remove block");
    expect(remove.closest('[role="group"]')).not.toBe(itemOf(menu, "Rename").closest('[role="group"]'));
    expect(remove.closest('[role="group"]')?.classList.contains("blockr-card-menu-divided")).toBe(true);
    expect(remove.querySelector("svg")).not.toBeNull();
    expect(button.getAttribute("aria-expanded")).toBe("true");
    // The block takes the focus, as on any press into its card.
    expect(core.focus.get()).toBe("b");

    press(document.activeElement ?? menu, "Escape");
    await settled();
    expect(blockMenu()).toBeNull();
    expect(button.getAttribute("aria-expanded")).toBe("false");
  });

  test("opens on ArrowDown while the button has the keyboard's focus, and picks an item on Enter", async () => {
    const { el, settled } = await startCards();

    const card = cardOf(el, "Head");
    const button = toolOf(card, "More actions");
    button.focus();
    press(button, "ArrowDown");
    await settled();
    const menu = blockMenu();
    if (menu === null) throw new Error("No menu opened.");

    const rename = itemOf(menu, "Rename");
    press(rename, "Enter");
    rename.dispatchEvent(new KeyboardEvent("keyup", { key: "Enter", bubbles: true, cancelable: true }));
    await settled();
    expect(blockMenu()).toBeNull();
    expect(document.activeElement).toBe(card.querySelector(".blockr-card-name-field"));
  });

  test("turns the name into a field on Rename, which takes the focus once the menu has closed", async () => {
    const { el, settled } = await startCards();

    const card = cardOf(el, "Head");
    const menu = await pressMenu(card, settled);
    itemOf(menu, "Rename").click();
    await settled();
    expect(blockMenu()).toBeNull();
    const field = card.querySelector<HTMLInputElement>(".blockr-card-name-field");
    expect(document.activeElement).toBe(field);
    expect([field?.selectionStart, field?.selectionEnd]).toEqual([0, "Head".length]);
  });

  test("closes on a press elsewhere", async () => {
    const { el, settled } = await startCards();

    await pressMenu(cardOf(el, "Head"), settled);
    mouse(cardOf(el, "Iris"), "pointerdown");
    clickOn(cardOf(el, "Iris"));
    await settled();
    expect(blockMenu()).toBeNull();
  });

  test("removes the block with Remove block", async () => {
    const { core, el, settled } = await startCards();

    toolOf(cardOf(el, "Head"), "More actions").click();
    await settled();
    const menu = blockMenu();
    if (menu === null) throw new Error("No menu opened.");
    itemOf(menu, "Remove block").click();
    await settled();
    expect(Object.hasOwn(core.store.board().blocks, "b")).toBe(false);
  });

  test("puts the block's id on the clipboard with Copy block ID, within the click that picks it", async () => {
    const { el, settled } = await startCards();
    const written = vi.spyOn(navigator.clipboard, "writeText").mockResolvedValue();
    onTestFinished(() => {
      written.mockRestore();
    });

    toolOf(cardOf(el, "Head"), "More actions").click();
    await settled();
    const menu = blockMenu();
    if (menu === null) throw new Error("No menu opened.");
    itemOf(menu, "Copy block ID").click();
    // The copy has run before anything that the click set off.
    expect(written).toHaveBeenCalledWith("b");
    await settled();
    expect(blockMenu()).toBeNull();
  });

  test("appends a block of the class picked in the dock's + menu, which opens from the same button, and one undo takes back", async () => {
    const { core, el, settled } = await startCards();

    const before = Object.keys(core.store.board().blocks);
    const links = core.store.board().links;
    const button = toolOf(cardOf(el, "Head"), "More actions");
    button.click();
    await settled();
    const menu = blockMenu();
    if (menu === null) throw new Error("No menu opened.");
    itemOf(menu, "Append block").click();
    await settled();

    expect(blockMenu()).toBeNull();
    const picker = plusMenu();
    if (picker === null) throw new Error("No + menu opened.");
    // The + menu is core's React SDK's, at the end of the page's body, outside
    // the browser's top layer.
    expect(el.contains(picker)).toBe(false);
    expect(picker.hasAttribute("popover")).toBe(false);
    expect(button.getAttribute("aria-expanded")).toBe("true");
    expect(picker.querySelector(".blockr-dock-picker-caption")?.textContent).toBe("Append to Head");
    // Only classes that take a link, so neither of the input blocks.
    expect(rows(picker)).toEqual(["scatter plot block", "head/tail block", "merge block", "rbind block", "subset block"]);
    expect([...picker.querySelectorAll("header")].map((title) => title.textContent)).toEqual(["plot", "transform"]);
    expect(rowOf(picker, "merge block").getAttribute("aria-label")).toBe("merge block, blockr.core");

    // The search takes the focus as the + menu opens, with no wait for the
    // block's menu.
    const search = await focusedSearch(picker);
    typeInto(search, "scatter");
    await settled();
    expect(rows(picker)).toEqual(["scatter plot block"]);
    press(search, "Enter");
    await settled();

    const board = core.store.board();
    const added = Object.keys(board.blocks).filter((id) => !before.includes(id));
    expect(added.map((id) => board.blocks[id]?.registry_id)).toEqual(["blockr.core-scatter_block"]);
    expect(Object.values(board.links)).toContainEqual({ from: "b", to: added[0], input: "data" });

    core.store.undo();
    expect(Object.keys(core.store.board().blocks)).toEqual(before);
    expect(core.store.board().links).toEqual(links);
  });

  test("closes the + menu alone on a press on its button", async () => {
    const { el, settled } = await startCards();

    const card = cardOf(el, "Head");
    const menu = await pressMenu(card, settled);
    itemOf(menu, "Append block").click();
    await settled();
    const picker = plusMenu();
    if (picker === null) throw new Error("No + menu opened.");
    await focusedSearch(picker);

    const button = toolOf(card, "More actions");
    mouse(button, "pointerdown");
    mouse(button, "pointerup");
    clickOn(button);
    await settled();
    expect(plusMenu()).toBeNull();
    expect(blockMenu()).toBeNull();
    expect(button.getAttribute("aria-expanded")).toBe("false");
  });

  test("clears the search of the + menu on Escape, and closes the menu on a second", async () => {
    const { el, settled } = await startCards();

    const button = toolOf(cardOf(el, "Head"), "More actions");
    button.click();
    await settled();
    const menu = blockMenu();
    if (menu === null) throw new Error("No menu opened.");
    itemOf(menu, "Append block").click();
    await settled();
    const picker = plusMenu();
    if (picker === null) throw new Error("No + menu opened.");
    const search = await focusedSearch(picker);

    typeInto(search, "merge");
    await settled();
    expect(rows(picker)).toEqual(["merge block"]);
    press(search, "Escape");
    await settled();
    expect(search.value).toBe("");
    expect(plusMenu()).not.toBeNull();

    press(search, "Escape");
    await settled();
    expect(plusMenu()).toBeNull();
    expect(button.getAttribute("aria-expanded")).toBe("false");
  });

  test("links a block of a class with several inputs from its first input", async () => {
    const { core, el, settled } = await startCards();

    toolOf(cardOf(el, "Head"), "More actions").click();
    await settled();
    const menu = blockMenu();
    if (menu === null) throw new Error("No menu opened.");
    itemOf(menu, "Append block").click();
    await settled();
    const picker = plusMenu();
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
    const menu = blockMenu();
    if (menu === null) throw new Error("No menu opened.");
    expect(items(menu)).toEqual(["Copy block ID"]);
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
  const tooltip = (el: HTMLElement) => el.querySelector('[role="tooltip"]');

  test("show a tool's name after the pointer rests 300ms, and leave as the pointer does", async () => {
    const { el, settled } = await startCards();

    const preview = toolOf(cardOf(el, "Head"), "Preview");
    pointer(preview, "pointerover");
    await wait(100);
    expect(tooltip(el)).toBeNull();
    await wait(300);
    expect(tooltip(el)?.textContent).toBe("Preview");

    pointer(preview, "pointerout");
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

  test("puts a menu 4px below its button, lined up with the button's left edge", () => {
    const left = { left: 100, top: 100, right: 126, bottom: 126, width: 26 };
    expect(menuSpot(left, { width: 200, height: 150 }, view)).toEqual({ left: 100, top: 130 });
  });

  test("lines a menu up with its button's right edge, for a button at the end of a row", () => {
    expect(menuSpot(trigger, { width: 200, height: 150 }, view, "end")).toEqual({ left: 526, top: 130 });
  });

  test("puts a menu above its button where it fits there but not below", () => {
    const low = { ...trigger, top: 500, bottom: 526 };
    expect(menuSpot(low, { width: 200, height: 150 }, view, "end")).toEqual({ left: 526, top: 346 });
  });

  test("keeps a menu 8px from the window's edges", () => {
    const left = { left: 10, top: 100, right: 36, bottom: 126, width: 26 };
    expect(menuSpot(left, { width: 200, height: 150 }, view, "end").left).toBe(8);
    expect(menuSpot(trigger, { width: 200, height: 150 }, view).left).toBe(592);
  });

  test("puts a tooltip 6px above its element, centred on it, and below where there is no room above", () => {
    expect(tooltipSpot(trigger, { width: 60, height: 24 }, view)).toEqual({ left: 683, top: 70 });
    const top = { ...trigger, top: 10, bottom: 36 };
    expect(tooltipSpot(top, { width: 60, height: 24 }, view).top).toBe(42);
  });
});

describe("the + menu's classes", () => {
  const entries = Object.values(HELLO.block_registry);
  const pick = () => undefined;

  test("come by category, then by name, with the classes of no category last", () => {
    const thing = { ...entries[0], id: "blockr.test-thing_block", name: "a thing", category: "" } as (typeof entries)[0];
    const groups = classGroups([...entries, thing], pick);
    expect(groups.map((group) => group.title)).toEqual(["input", "plot", "transform", "uncategorized"]);
    expect(groups.find((group) => group.title === "transform")?.rows.map((row) => row.label)).toEqual([
      "head/tail block",
      "merge block",
      "rbind block",
      "subset block",
    ]);
    expect(groups.find((group) => group.title === "uncategorized")?.rows.map((row) => row.badge)).toEqual(["blockr.test"]);
  });

  test("are those whose name, id, category or keywords hold every word searched", () => {
    const found = (searched: string) =>
      classGroups(entries, pick).flatMap((group) => group.rows.filter((row) => matches(row, searched)).map((row) => row.key));
    expect(found("Block MERGE")).toEqual(["blockr.core-merge_block"]);
    expect(found("plot")).toEqual(["blockr.core-scatter_block"]);
    expect(found("nothing like it")).toEqual([]);
  });
});
