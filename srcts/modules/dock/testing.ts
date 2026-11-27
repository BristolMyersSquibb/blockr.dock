// The dock's frontend on blockr.core's CoreApi for tests, which starts core
// as the page of an app that runs the dock would, with R played from the
// fixtures of core's wire protocol, so that the tests need neither R nor a
// browser (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.8#testing).
// The tests of the dock's frontend and of its cards start core through it.
// It also gives the tests what a user does, the keys that a user presses, the
// clicks and what the user types into a picker's search, through blockr.ui's
// press() and type().
// Each runs inside React's act(), as the act environment asks that blockr.ui's
// stubs turn on, so that React draws what it set off before the test reads
// the page, and warns of no update outside act().

import type { Json, JsonBoard } from "@blockr/core";
import { HELLO, stubCore } from "@blockr/core/stubs";
import { press, type } from "@blockr/ui/stubs";
import { act } from "react";
import * as compactOption from "../compact-option/index.ts";
import * as syncRailsOption from "../sync-rails-option/index.ts";
import * as viewTabsOption from "../view-tabs-option/index.ts";
import * as dock from "./index.tsx";

export const DOCK = "blockr.dock-dock_plugin";

// Presses a key on the element, as blockr.ui's press() does, in an event that
// a handler can take, as a browser's is, and says whether one took it.
// A handler of the dock takes a key that it acts on, so that nothing else acts
// on it.
export function pressOn(el: Element | null, key: string, init: KeyboardEventInit = {}): boolean {
  if (!(el instanceof HTMLElement)) throw new Error(`Found no element to press ${key} on.`);
  const heard: Event[] = [];
  el.addEventListener("keydown", (event) => heard.push(event), { once: true });
  act(() => {
    press(el, key, { cancelable: true, ...init });
  });
  return heard[0]?.defaultPrevented ?? false;
}

// Presses a key on the element that has the focus, as pressOn() does.
export function pressFocused(key: string, init: KeyboardEventInit = {}): boolean {
  return pressOn(document.activeElement, key, init);
}

// Types the text into the field, as blockr.ui's type() does.
export function typeInto(field: HTMLInputElement | HTMLTextAreaElement, text: string): void {
  act(() => {
    type(field, text);
  });
}

// Types the text into the search of one of the dock's pickers, as blockr.ui's
// type() does, after the event beforeinput, which a browser fires as the user
// types, and on which React Aria's Autocomplete has the first row that the
// search keeps take the virtual focus.
// The type() of blockr.ui fires no beforeinput.
export function typeSearch(search: HTMLInputElement, text: string): void {
  act(() => {
    search.focus();
    search.dispatchEvent(new InputEvent("beforeinput", { inputType: "insertText", data: text, bubbles: true, cancelable: true }));
    type(search, text);
  });
}

// Clicks the element as a user's pointer does: it goes down on it, then
// clicks.
export function click(target: Element | null | undefined): void {
  if (target === null || target === undefined) throw new Error("Nothing to click.");
  act(() => {
    target.dispatchEvent(new PointerEvent("pointerdown", { bubbles: true }));
    target.dispatchEvent(new MouseEvent("click", { bubbles: true, detail: 1 }));
  });
}

// Lets the page run until what the last step set off has drawn, which takes
// a turn of the event loop after React's render.
export async function drawn(): Promise<void> {
  await act(async () => {
    await new Promise((resolve) => setTimeout(resolve, 0));
  });
}

// Tries the check until it passes, as Vitest's vi.waitFor() does, for up to a
// second, and gives what it returns.
// Before each try, the page runs inside act(), first for a turn of the event
// loop, so that what the last step set off runs inside act() too, and then
// for 10ms at a time.
// A try runs outside act(), since React draws what it set off inside an act()
// only as the act() ends.
export async function waitFor<T>(check: () => T): Promise<T> {
  const ends = Date.now() + 1000;
  for (let ms = 0; ; ms = 10) {
    await act(async () => {
      await new Promise((resolve) => setTimeout(resolve, ms));
    });
    try {
      return check();
    } catch (error) {
      if (Date.now() > ends) throw error;
    }
  }
}

// The tests run on happy-dom, which has no Popover API, with which the dock
// shows its menus and tooltips in the top layer and hides them again.
// So the tests give every element a showPopover() and a hidePopover() that do
// nothing, and read what the dock draws.
for (const method of ["showPopover", "hidePopover"]) {
  if (!(method in HTMLElement.prototype)) {
    Object.defineProperty(HTMLElement.prototype, method, { configurable: true, value: () => undefined });
  }
}

// R's reply to hello in an app that runs the dock in place of core's basic
// frontend, beside core's other plugins.
// The entry of the dock plugin's class gives the class's version, 1, the
// board options that the dock reads and where R serves the class's module.
// It names the board options as the method of R's plugin_options() for the
// class does.
// The reply also says where R serves the modules of the dock's board options,
// beside core's.
export const hello = {
  ...HELLO,
  packages: { ...HELLO.packages, "blockr.dock": "0.2.1" },
  plugins: {
    ...Object.fromEntries(
      Object.entries(HELLO.plugins).filter(([id]) => id !== "blockr.core-basic_frontend_plugin"),
    ),
    [DOCK]: {
      name: "Dock",
      version: 1,
      options: ["compact", "view_tabs", "sync_rails", "dark_mode"],
      module: "blockr.dock/js/dock.js",
    },
  },
  options: {
    ...HELLO.options,
    compact: { package: "blockr.dock", module: "blockr.dock/compact-option.js", version: 1 },
    view_tabs: { package: "blockr.dock", module: "blockr.dock/view-tabs-option.js", version: 1 },
    sync_rails: { package: "blockr.dock", module: "blockr.dock/sync-rails-option.js", version: 1 },
  },
};

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

// Stand-ins for the modules of core's block classes, by the module that each
// entry of core's registry names, so that the cards that the dock draws mount
// a UI and an output without a notification that a module failed.
// The UI of the rbind block draws nothing, as core's does, and that of every
// other class draws a line, and each renderer of a class's outputs draws
// nothing.
const blockModules: Readonly<Record<string, unknown>> = Object.fromEntries(
  Object.values(HELLO.block_registry).flatMap((entry): [string, unknown][] => [
    [entry.module, entry.id === "blockr.core-rbind_block" ? drawsNothing : drawsLine],
    ...(entry.renderers ?? []).map((renderer): [string, unknown] => [renderer.module, drawsNothing]),
  ]),
);

// What each module of the dock exports, by the id that R's reply names it
// under, and the stand-ins for core's block classes.
export const modules = {
  ...blockModules,
  [DOCK]: dock,
  compact: compactOption,
  view_tabs: viewTabsOption,
  sync_rails: syncRailsOption,
};

// Starts core with the dock's modules, with the values of blockr options that
// R holds, none unless given, where the frontend of the dock plugin's class
// draws the page in `el` where a test gives one, on the board that R's reply
// to hello gives, the fixture's unless one is given.
export function startDock(el?: HTMLElement, blockrOptions: Readonly<Record<string, Json>> = {}, board?: JsonBoard) {
  return stubCore({ hello: board === undefined ? hello : { ...hello, board }, modules, blockrOptions, el });
}
