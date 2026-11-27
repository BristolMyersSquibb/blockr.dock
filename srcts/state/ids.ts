// The ids of what packages define, as the dock's state holds them: plain
// records of a package and an id, since the state is JSON.
// The code of the state has no `core` to make an Id of a record, so it checks,
// compares and names an id by its two fields.

import type { Json } from "@blockr/core";
import { isObject } from "./json.ts";
import type { IdRecord } from "./types.ts";

// Whether a value holds an id as JSON holds one: an object with a package and
// an id, each a string.
export function isIdRecord(value: Json | undefined): value is IdRecord {
  return isObject(value) && typeof value.package === "string" && typeof value.id === "string";
}

// Whether two ids name the same, by their packages and their ids, as a record
// that JSON gave and an Id that core made do.
export function sameId(a: IdRecord, b: IdRecord): boolean {
  return a.package === b.package && a.id === b.id;
}

// How text names an id, as core's messages name one: "edit_board_plugin of
// blockr.core", or the id alone for what an app script defines.
// Core's idText() writes the same text, but no entry of @blockr/core exports
// it.
export function idText(x: { readonly package: string | null; readonly id: string }): string {
  return x.package === null ? x.id : `${x.id} of ${x.package}`;
}
