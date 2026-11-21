// Where keepFocus() moves the focus on Tab and Shift+Tab, in an element that
// the test builds.
// The test does not use the options sidebar, since the board options that
// core puts on the board decide which control comes last there.

import { describe, expect, onTestFinished, test } from "vitest";
import { keepFocus } from "./focus.ts";

// An element that can take the focus itself, as the options sidebar can.
// It holds a button, then a field, the last control that the user can reach.
// After the field, it holds controls that Tab skips: a button with
// `display: none`, one with `visibility: hidden`, and one in an inert element.
// The element leaves the page as the test finishes.
function build(): { readonly root: HTMLElement; readonly first: HTMLElement; readonly field: HTMLElement } {
  const root = document.createElement("div");
  root.tabIndex = -1;
  const first = document.createElement("button");
  first.type = "button";
  const field = document.createElement("input");
  field.type = "number";
  const removed = document.createElement("button");
  removed.type = "button";
  removed.style.display = "none";
  const invisible = document.createElement("button");
  invisible.type = "button";
  invisible.style.visibility = "hidden";
  const inert = document.createElement("div");
  inert.inert = true;
  inert.append(document.createElement("button"));
  root.append(first, field, removed, invisible, inert);
  document.body.append(root);
  onTestFinished(() => {
    root.remove();
  });
  return { root, first, field };
}

// Has keepFocus() hear Tab, or Shift+Tab where `shift` says, and says whether
// it took the key.
function tab(root: HTMLElement, shift = false): boolean {
  const event = new KeyboardEvent("keydown", { key: "Tab", shiftKey: shift, cancelable: true });
  keepFocus(root, event);
  return event.defaultPrevented;
}

describe("keepFocus()", () => {
  test("moves the focus from the last control to the first on Tab, and back on Shift+Tab, leaving out a control that is hidden or inert", () => {
    const { root, first, field } = build();
    field.focus();
    expect(tab(root)).toBe(true);
    expect(document.activeElement).toBe(first);
    expect(tab(root, true)).toBe(true);
    expect(document.activeElement).toBe(field);
  });

  test("leaves Tab and Shift+Tab to the browser where they keep the focus inside the element", () => {
    const { root, first, field } = build();
    first.focus();
    expect(tab(root)).toBe(false);
    expect(document.activeElement).toBe(first);
    field.focus();
    expect(tab(root, true)).toBe(false);
    expect(document.activeElement).toBe(field);
  });

  test("moves the focus from the element itself to its first control on Tab, and to its last on Shift+Tab", () => {
    const { root, first, field } = build();
    root.focus();
    expect(tab(root)).toBe(true);
    expect(document.activeElement).toBe(first);
    root.focus();
    expect(tab(root, true)).toBe(true);
    expect(document.activeElement).toBe(field);
  });

  test("keeps the focus on the element itself where it holds no control that the user can reach", () => {
    const { root, first, field } = build();
    first.remove();
    field.remove();
    root.focus();
    expect(tab(root)).toBe(true);
    expect(document.activeElement).toBe(root);
    expect(tab(root, true)).toBe(true);
    expect(document.activeElement).toBe(root);
  });
});
