// Checks of the kind of a Json value, which TypeScript takes to narrow the
// value's type.

import type { Json } from "@blockr/core";

// Whether a value is an array, as Array.isArray() says, which TypeScript does
// not take to narrow a readonly array.
export function isList(value: Json | undefined): value is readonly Json[] {
  return Array.isArray(value);
}

// Whether a value is an object, which an array is not.
export function isObject(value: Json | undefined): value is Readonly<Record<string, Json>> {
  return value !== null && typeof value === "object" && !Array.isArray(value);
}
