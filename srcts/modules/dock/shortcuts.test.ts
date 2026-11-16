// How the bar reads, names and matches the shortcuts that it binds.

import { describe, expect, test } from "vitest";
import { ariaShortcuts, matchesShortcut, shortcutLabel } from "./shortcuts.ts";

function key(init: KeyboardEventInit): KeyboardEvent {
  return new KeyboardEvent("keydown", init);
}

describe("matchesShortcut()", () => {
  test("matches Mod to Ctrl or Cmd, and the key in any case", () => {
    expect(matchesShortcut("Mod+S", key({ key: "s", ctrlKey: true }))).toBe(true);
    expect(matchesShortcut("Mod+S", key({ key: "S", metaKey: true }))).toBe(true);
    expect(matchesShortcut("mod+s", key({ key: "s", ctrlKey: true }))).toBe(true);
    expect(matchesShortcut("Mod+S", key({ key: "s" }))).toBe(false);
    expect(matchesShortcut("Mod+S", key({ key: "s", ctrlKey: true, shiftKey: true }))).toBe(false);
  });

  test("matches Ctrl and Meta each to its own key, and reads the key + after the last +", () => {
    expect(matchesShortcut("Ctrl+Alt+K", key({ key: "k", ctrlKey: true, altKey: true }))).toBe(true);
    expect(matchesShortcut("Ctrl+K", key({ key: "k", metaKey: true }))).toBe(false);
    expect(matchesShortcut("Meta+K", key({ key: "k", metaKey: true }))).toBe(true);
    expect(matchesShortcut("Mod++", key({ key: "+", ctrlKey: true }))).toBe(true);
  });

  test("matches no shortcut that it cannot read", () => {
    expect(matchesShortcut("Mod+Ctrl+S", key({ key: "s", ctrlKey: true }))).toBe(false);
    expect(matchesShortcut("Hyper+S", key({ key: "s" }))).toBe(false);
    expect(matchesShortcut("Shift+Shift+S", key({ key: "s", shiftKey: true }))).toBe(false);
    expect(matchesShortcut("", key({ key: "s" }))).toBe(false);
  });
});

describe("ariaShortcuts()", () => {
  test("writes a shortcut as aria-keyshortcuts takes it, with Mod as both Control and Meta", () => {
    expect(ariaShortcuts("Mod+Shift+S")).toBe("Control+Shift+S Meta+Shift+S");
    expect(ariaShortcuts("Ctrl+Alt+K")).toBe("Control+Alt+K");
    expect(ariaShortcuts(undefined)).toBeUndefined();
    expect(ariaShortcuts("Mod+Meta+S")).toBeUndefined();
  });
});

describe("shortcutLabel()", () => {
  test("names a shortcut in glyphs on a Mac and in words elsewhere, as blockr.ui's design system has it", () => {
    expect(shortcutLabel("Mod+Z", true)).toBe("⌘Z");
    expect(shortcutLabel("Mod+Shift+Z", true)).toBe("⌘⇧Z");
    expect(shortcutLabel("Mod+Z", false)).toBe("Ctrl+Z");
    expect(shortcutLabel("Mod+Shift+Z", false)).toBe("Ctrl+Shift+Z");
    expect(shortcutLabel("Alt+Backspace", false)).toBe("Alt+Backspace");
  });
});
