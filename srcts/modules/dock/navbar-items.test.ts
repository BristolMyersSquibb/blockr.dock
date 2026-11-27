// How the dock reads the navbar items that R sends in its reply to hello.

import type { Json } from "@blockr/core";
import { describe, expect, test } from "vitest";
import { DEFAULT_NAVBAR, PARTS, readNavbar } from "./navbar-items.ts";

describe("readNavbar()", () => {
  test("gives the default bar, the dock's own parts in their order, where R sends no navbar", () => {
    expect(readNavbar({})).toBe(DEFAULT_NAVBAR);
    expect(DEFAULT_NAVBAR.map((item) => item.id)).toEqual([...PARTS]);
    expect(DEFAULT_NAVBAR.every((item) => item.kind === "part")).toBe(true);
  });

  test("reads every kind of item as R writes it, in its order", () => {
    const navbar: Json[] = [
      { kind: "part", id: "logo" },
      { kind: "storage", id: "avatar" },
      { kind: "link", id: "docs", label: "Docs", href: "https://example.org" },
      {
        kind: "menu",
        id: "help",
        label: "Help",
        items: [
          { kind: "link", id: "issues", label: "Report an issue", href: "https://example.org/issues" },
          { kind: "menu", id: "more", label: "More", items: [] },
        ],
      },
      { kind: "module", id: "deck", module: { package: "blockr.test", id: "deck_button" } },
    ];
    expect(readNavbar({ navbar })).toEqual(navbar);
  });

  test("fails, naming the item, for an item that the dock cannot draw", () => {
    expect(() => readNavbar({ navbar: [{ kind: "part", id: "clock" }] })).toThrow('The dock has no part "clock" to draw on the navbar.');
    expect(() => readNavbar({ navbar: [{ kind: "widget", id: "w" }] })).toThrow('The navbar item "w" is of the kind "widget"');
    expect(() => readNavbar({ navbar: [{ kind: "link", id: "docs", label: "Docs" }] })).toThrow(
      'The href of the navbar item "docs" is undefined',
    );
    // A module item names its module by a record of two non-empty strings, its
    // package and its id.
    expect(() => readNavbar({ navbar: [{ kind: "module", id: "deck", module: "blockr.test-deck_button" }] })).toThrow(
      'The module of the navbar item "deck" is "blockr.test-deck_button", where the dock takes the package and the id of a module',
    );
    expect(() => readNavbar({ navbar: [{ kind: "module", id: "deck", module: { package: "blockr.test", id: "" } }] })).toThrow(
      'The module of the navbar item "deck" is {"package":"blockr.test","id":""}',
    );
    expect(() => readNavbar({ navbar: [{ kind: "part" }] })).toThrow("has no kind and id that the dock reads");
    expect(() => readNavbar({ navbar: "logo" })).toThrow('The navbar is "logo"');
  });

  test("fails for a menu that holds anything but links and menus, or ids that two items share", () => {
    const menu = (items: Json[]): Json => ({ kind: "menu", id: "help", label: "Help", items });
    expect(() => readNavbar({ navbar: [menu([{ kind: "part", id: "logo" }])] })).toThrow(
      'The navbar menu holds the item "logo", which is neither a link nor a menu.',
    );
    expect(() => readNavbar({ navbar: [{ kind: "menu", id: "help", label: "Help", items: {} }] })).toThrow(
      'The items of the navbar menu "help" are {}',
    );
    expect(() =>
      readNavbar({
        navbar: [
          { kind: "part", id: "logo" },
          { kind: "storage", id: "logo" },
        ],
      }),
    ).toThrow('More than one navbar item on the bar has the id "logo".');
    const link = { kind: "link", id: "a", label: "A", href: "https://example.org" };
    expect(() => readNavbar({ navbar: [menu([link, link])] })).toThrow('More than one navbar item in the menu "help" has the id "a".');
  });
});
