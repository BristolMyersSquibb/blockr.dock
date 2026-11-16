// What the dock holds about its options sidebar for the page, outside the
// board.

import { describe, expect, test } from "vitest";
import { createSidebar } from "./sidebar.ts";

describe("the state of the options sidebar", () => {
  test("is closed, unpinned and on the list of categories as the dock starts", () => {
    expect(createSidebar().state()).toEqual({ open: false, pinned: false, page: undefined });
  });

  test("pins and shows a page only while the sidebar is open", () => {
    const sidebar = createSidebar();
    sidebar.pin(true);
    sidebar.show("Theme options");
    expect(sidebar.state()).toEqual({ open: false, pinned: false, page: undefined });

    sidebar.set(true);
    sidebar.pin(true);
    sidebar.show("Theme options");
    expect(sidebar.state()).toEqual({ open: true, pinned: true, page: "Theme options" });
  });

  test("unpins the sidebar and returns it to the list as it closes", () => {
    const sidebar = createSidebar();
    sidebar.set(true);
    sidebar.pin(true);
    sidebar.show("Theme options");
    sidebar.set(false);
    sidebar.set(true);
    expect(sidebar.state()).toEqual({ open: true, pinned: false, page: undefined });
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
