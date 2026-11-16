// Whether two Json values are the same.

import { describe, expect, test } from "vitest";
import { sameJson } from "./json.ts";

describe("sameJson()", () => {
  test("is true for the same scalars, and false for different ones or kinds", () => {
    expect(sameJson(1, 1)).toBe(true);
    expect(sameJson("a", "a")).toBe(true);
    expect(sameJson(null, null)).toBe(true);
    expect(sameJson(true, false)).toBe(false);
    expect(sameJson(1, "1")).toBe(false);
    expect(sameJson(null, false)).toBe(false);
  });

  test("compares arrays by their values in order", () => {
    expect(sameJson(["warning", "error"], ["warning", "error"])).toBe(true);
    expect(sameJson(["warning", "error"], ["error", "warning"])).toBe(false);
    expect(sameJson(["error"], ["error", "warning"])).toBe(false);
    expect(sameJson([], {})).toBe(false);
  });

  test("compares objects by their values by name, in any order", () => {
    expect(sameJson({ a: 1, b: [2] }, { b: [2], a: 1 })).toBe(true);
    expect(sameJson({ a: 1 }, { a: 1, b: null })).toBe(false);
    expect(sameJson({ a: { b: 1 } }, { a: { b: 2 } })).toBe(false);
  });
});
