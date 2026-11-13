// The schemas of the args of the dock's commands, which describe them to
// menus and to the assistant's tools
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.2#commands).
// A panel and a grid take the definitions of the schema of the dock's state,
// which the build generates from the state's types.

import type { Json, JsonSchema } from "@blockr/core";
// The schema of the dock's state is the file that the build writes for R,
// beside the bundle.
// eslint-disable-next-line no-restricted-imports
import schema from "../../inst/js/dock.schema.json" with { type: "json" };
import { EDGES, SIDES } from "../state/index.ts";

// The schema of an object with the properties given, which takes no other.
// It holds, under $defs, each definition of the state's schema that the
// properties refer to, and those that these refer to in turn.
export function object(properties: Readonly<Record<string, JsonSchema>>, required: readonly string[] = []): JsonSchema {
  const defs = definitionsFor(properties);
  return {
    type: "object",
    properties,
    required,
    additionalProperties: false,
    ...(Object.keys(defs).length === 0 ? {} : { $defs: defs }),
  };
}

// The definitions of the state's schema, with each reference pointing under
// $defs, where object() puts them.
// The build writes the schema in the draft 7 of JSON Schema, whose keywords
// these definitions share with the draft 2020-12, which core reads a
// command's args by.
const DEFINITIONS = JSON.parse(
  JSON.stringify(schema.definitions).replaceAll('"#/definitions/', '"#/$defs/'),
) as Readonly<Record<string, Json>>;

// A reference to a definition of the state's schema, with a description of
// its own.
export function ref(name: keyof typeof schema.definitions, description: string): JsonSchema {
  return { $ref: `#/$defs/${name}`, description };
}

function definitionsFor(value: Json): Record<string, Json> {
  const picked = new Map<string, Json>();
  const queue = referenced(value);
  for (let name = queue.pop(); name !== undefined; name = queue.pop()) {
    if (picked.has(name)) continue;
    const definition = DEFINITIONS[name];
    if (definition === undefined) throw new Error(`The schema of the dock's state defines no "${name}".`);
    picked.set(name, definition);
    queue.push(...referenced(definition));
  }
  return Object.fromEntries([...picked].sort(([a], [b]) => (a < b ? -1 : 1)));
}

function referenced(value: Json): string[] {
  return [...JSON.stringify(value).matchAll(/"#\/\$defs\/([^"]+)"/g)].flatMap((match) => match.slice(1, 2));
}

export const view: JsonSchema = { type: "string", description: "The id of a view." };

export const folder: JsonSchema = { type: "string", description: "The id of a folder." };

// The folder that a view or folder goes into, or null for the top, where it
// goes too where `optional` says that the args may leave the folder out.
export function into(what: "view" | "folder", optional = false): JsonSchema {
  const top = optional ? " It goes to the top where this is left out." : "";
  return {
    type: ["string", "null"],
    description: `The id of the folder that the ${what} goes into, or null for the top of the navigation.${top}`,
  };
}

// The name of a view or folder, which holds a character other than a space,
// as a card refuses a block's name otherwise
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.1#commands).
// Two views or folders may share a name, since each is known by its id.
export function name(what: "view" | "folder", description: string): JsonSchema {
  return {
    type: "string",
    pattern: "\\S",
    description: `${description} It holds a character other than a space, and may be the name of another ${what}.`,
  };
}

export const before: JsonSchema = {
  description:
    'The view or folder that it goes before, as { "view": id } or { "folder": id }, which sits in the folder that it goes into. It goes at the end of that folder where this is left out.',
  anyOf: [object({ view }, ["view"]), object({ folder }, ["folder"])],
};

export const panel: JsonSchema = ref("Panel", 'A panel, as a grid names it: { "block": id } for a block\'s card, or { "plugin": id } for the UI of a plugin, by the id of its class.');

export function near(what: string): JsonSchema {
  return ref(
    "Panel",
    `A panel of the view, by whose tab group the panel ${what}: in it, or beside it on the side that \`side\` names. Where this panel sits on a rail, the panel ${what} onto that rail, and \`side\` is within.`,
  );
}

export const side: JsonSchema = {
  type: "string",
  enum: SIDES,
  description:
    "Where the panel goes by the tab group of the panel that `near` names: within it, as a new tab, or in a new tab group to its left or right, or above or below it. It is within unless given. Without `near`, a side other than within places the panel at that edge of the whole grid.",
};

export const rail: JsonSchema = {
  type: "string",
  enum: EDGES,
  description:
    "The edge of the view whose rail takes the panel, as a new tab, which opens a rail there where the view has none. It takes precedence over `near` and `side`.",
};

export const edge: JsonSchema = { type: "string", enum: EDGES, description: "The edge of the view that the rail sits on." };

export const grid: JsonSchema = ref("Grid", "A view's grid, which says which panels it shows and where.");

