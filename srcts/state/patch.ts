// JSON Patch for the dock's state.
// It applies a delta's patches as the store does: RFC 6902's add, remove and
// replace, at JSON Pointers from the board's root (RFC 6901).
// The package @blockr/core ships types alone, so the dock cannot call the
// store's own code for this.
// The `update` uses it to learn what a delta's patches leave of the dock's
// state, and to write the patches that it returns.

import type { Json, Patch, PluginClassId } from "@blockr/core";
import { isList, isObject } from "./json.ts";

// The JSON Pointer to the segments given, from the board's root.
// It writes "~" as "~0" and "/" as "~1", and gives
// "/plugins/blockr.dock-dock_plugin" for ["plugins", "blockr.dock-dock_plugin"].
export function pointer(segments: readonly string[]): string {
  return segments.map((segment) => `/${segment.replaceAll("~", "~0").replaceAll("/", "~1")}`).join("");
}

// The segments of a JSON Pointer, or undefined where the text is no pointer
// below the board's root.
function segmentsOf(path: string): string[] | undefined {
  if (!path.startsWith("/")) return undefined;
  const segments = path.slice(1).split("/");
  if (segments.some((segment) => /~(?![01])/.test(segment))) return undefined;
  return segments.map((segment) => segment.replaceAll("~1", "/").replaceAll("~0", "~"));
}

// The state of the plugin `plugin` after the patches.
// It applies the patches that change the plugin's state, in order, to
// `state`, the plugin's state in the board before them, and skips the
// others.
// A patch at the plugin's own path replaces the state whole.
// It gives undefined where a patch cannot apply, as where its path leads to
// nothing, since the store then refuses the delta.
export function patched(state: Json, patches: readonly Patch[], plugin: PluginClassId): Json | undefined {
  let at: Json | undefined = state;
  for (const patch of patches) {
    const segments = segmentsOf(patch.path);
    if (segments?.[0] !== "plugins" || segments[1] !== plugin) continue;
    at = at === undefined ? undefined : applied(at, segments.slice(2), patch);
  }
  return at;
}

// The value after a patch at the path `segments` below it, or undefined where
// the patch cannot apply.
function applied(at: Json, segments: readonly string[], patch: Patch): Json | undefined {
  const [segment, ...rest] = segments;
  if (segment === undefined) return patch.op === "replace" ? patch.value : undefined;

  if (isList(at)) {
    const items = at;
    const last = rest.length === 0;
    const index = last && segment === "-" && patch.op === "add" ? items.length : arrayIndex(segment);
    const bound = last && patch.op === "add" ? items.length : items.length - 1;
    if (index === undefined || index > bound) return undefined;
    const copy = [...items];
    if (!last) {
      const inner = applied(items[index] as Json, rest, patch);
      if (inner === undefined) return undefined;
      copy.splice(index, 1, inner);
      return copy;
    }
    if (patch.op === "remove") copy.splice(index, 1);
    else if (patch.value === undefined) return undefined;
    else copy.splice(index, patch.op === "add" ? 0 : 1, patch.value);
    return copy;
  }

  if (!isObject(at)) return undefined;
  const record = at;
  const has = Object.hasOwn(record, segment);

  if (rest.length > 0) {
    if (!has) return undefined;
    const inner = applied(record[segment] as Json, rest, patch);
    return inner === undefined ? undefined : { ...record, [segment]: inner };
  }

  if (patch.op === "remove") {
    if (!has) return undefined;
    return Object.fromEntries(Object.entries(record).filter(([name]) => name !== segment));
  }
  if (patch.value === undefined || (patch.op === "replace" && !has)) return undefined;
  return { ...record, [segment]: patch.value };
}

// An array's index, as JSON Pointer writes one: digits without a leading zero.
function arrayIndex(segment: string): number | undefined {
  return /^(0|[1-9]\d*)$/.test(segment) ? Number(segment) : undefined;
}
