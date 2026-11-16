// Checks of the kind of a Json value, or of anything else, such as what a
// module exports, which TypeScript takes to narrow the value's type, and a
// check of whether two Json values are the same.

import type { Json } from "@blockr/core";

// Whether a value is an array, as Array.isArray() says, which TypeScript does
// not take to narrow a readonly array.
export function isList(value: Json | undefined): value is readonly Json[] {
  return Array.isArray(value);
}

// Whether a value is an object, which an array is not.
export function isObject(value: Json | undefined): value is Readonly<Record<string, Json>>;
export function isObject(value: unknown): value is Readonly<Record<string, unknown>>;
export function isObject(value: unknown): boolean {
  return value !== null && typeof value === "object" && !Array.isArray(value);
}

// Whether two Json values are the same: the same scalar, arrays of the same
// values in the same order, or objects with the same values by name, in any
// order.
export function sameJson(a: Json, b: Json): boolean {
  if (a === b) return true;
  if (isList(a) || isList(b)) {
    return isList(a) && isList(b) && a.length === b.length && a.every((value, i) => i in b && sameJson(value, b[i] ?? null));
  }
  if (!isObject(a) || !isObject(b)) return false;
  const names = Object.keys(a);
  return (
    names.length === Object.keys(b).length &&
    names.every((name) => Object.hasOwn(b, name) && sameJson(a[name] ?? null, b[name] ?? null))
  );
}
