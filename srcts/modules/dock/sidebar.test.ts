// What the dock holds about its options sidebar for the page, outside the
// board.

import { describe, expect, test } from "vitest";
import { createSidebar } from "./sidebar.ts";

describe("the state of the options sidebar", () => {
  test("is closed and unpinned, with every category collapsed, as the dock starts", () => {
    expect(createSidebar().state()).toEqual({ open: false, pinned: false, expanded: [] });
  });

  test("pins the sidebar only while it is open", () => {
    const sidebar = createSidebar();
    sidebar.pin(true);
    expect(sidebar.state().pinned).toBe(false);

    sidebar.set(true);
    sidebar.pin(true);
    expect(sidebar.state()).toEqual({ open: true, pinned: true, expanded: [] });
  });

  test("expands several categories at once, in the order in which they expand, and collapses each", () => {
    const sidebar = createSidebar();
    sidebar.set(true);
    sidebar.expand("Theme options", true);
    sidebar.expand("Board options", true);
    expect(sidebar.state().expanded).toEqual(["Theme options", "Board options"]);

    sidebar.expand("Theme options", false);
    expect(sidebar.state().expanded).toEqual(["Board options"]);
  });

  test("unpins the sidebar as it closes, and keeps its categories expanded", () => {
    const sidebar = createSidebar();
    sidebar.set(true);
    sidebar.pin(true);
    sidebar.expand("Theme options", true);
    sidebar.set(false);
    expect(sidebar.state()).toEqual({ open: false, pinned: false, expanded: ["Theme options"] });

    sidebar.set(true);
    expect(sidebar.state()).toEqual({ open: true, pinned: false, expanded: ["Theme options"] });
  });

  test("keeps the same state until it changes, and calls each listener once for each change, until it unsubscribes", () => {
    const sidebar = createSidebar();
    const heard: boolean[] = [];
    const off = sidebar.subscribe(() => {
      heard.push(sidebar.state().open);
    });
    const before = sidebar.state();
    sidebar.set(false);
    sidebar.pin(false);
    sidebar.expand("Theme options", false);
    expect(sidebar.state()).toBe(before);
    expect(heard).toEqual([]);

    sidebar.set(true);
    sidebar.set(true);
    off();
    sidebar.set(false);
    expect(heard).toEqual([true]);
  });

  test("gives each sidebar an id of its own", () => {
    expect(createSidebar().id).not.toBe(createSidebar().id);
  });
});
