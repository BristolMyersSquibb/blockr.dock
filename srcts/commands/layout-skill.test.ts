// The dock's skill `layout`, which teaches the assistant's model the dock's
// commands and grids
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.4.5#what-a-package-adds).
// R in blockr.assistant lists it from inst/assistant/skills, with the skills
// of every package of the app.
// The skill is text that only the model reads, so these tests are what fails
// where a command that it names goes or is renamed.

import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { id, stubCore } from "@blockr/core/stubs";
import { expect, onTestFinished, test } from "vitest";
import { DOCK, dockCommands } from "./index.ts";

// The text of a file, by its path from this file.
// Vite rewrites `new URL()` of a string literal into the URL that its server
// serves the file at, so the path comes in as an argument.
const read = (path: string) => readFileSync(fileURLToPath(new URL(path, import.meta.url)), "utf8");

const SKILL = read("../../inst/assistant/skills/layout/SKILL.md");

// How many characters of a skill's description the catalogue of the system
// prompt shows, the blockr option assistant_skill_description_max_chars
// unless a deployment sets it.
const DESCRIPTION_MAX_CHARS = 1024;

// The top-level keys of the skill's YAML frontmatter, each with its value.
// A value that the key's line leaves open, as `>-` does, takes the indented
// lines below it, joined with spaces, as YAML folds them.
function frontmatter(text: string): Record<string, string> {
  const [first, ...lines] = text.split("\n");
  if (first !== "---") throw new Error("The skill starts with no YAML frontmatter.");
  const end = lines.indexOf("---");
  if (end === -1) throw new Error("The skill's YAML frontmatter has no end.");
  const keys: Record<string, string> = {};
  let key: string | undefined;
  for (const line of lines.slice(0, end)) {
    const top = /^([A-Za-z][\w-]*):\s*(.*)$/.exec(line);
    if (top?.[1] !== undefined) {
      key = top[1];
      const value = top[2] ?? "";
      keys[key] = value === ">-" || value === ">" ? "" : value;
    } else if (key !== undefined && /^\s+\S/.test(line)) {
      keys[key] = `${keys[key] ?? ""} ${line.trim()}`.trim();
    }
  }
  return keys;
}

test("the skill is named after its directory and names no class, so the catalogue lists it in every app that runs the dock", () => {
  const keys = frontmatter(SKILL);
  expect(Object.keys(keys)).toEqual(["name", "description"]);
  expect(keys.name).toBe("layout");
});

test("the catalogue shows the skill's description in full", () => {
  const description = frontmatter(SKILL).description ?? "";
  expect(description).not.toBe("");
  expect(description.length).toBeLessThanOrEqual(DESCRIPTION_MAX_CHARS);
});

// The ids that the skill names, each by its package and its id, as the model
// passes an id to the assistant's tools, as
// { "package": "blockr.dock", "id": "add_view" }, as core makes them.
function namedIds(text: string) {
  return new Set([...text.matchAll(/\{ "package": "([\w.]+)", "id": "(\w+)" \}/g)].map(([, pkg = "", within = ""]) => id(pkg, within)));
}

test("the skill names each command and class by its package and its id, and none by one string, as a joined id did", () => {
  expect(namedIds(SKILL).size).toBeGreaterThan(0);
  expect(SKILL.match(/\bblockr\.(?:core|dock)-\w+/g)).toBeNull();
});

test("the skill names only commands that the dock and core register", async () => {
  const { core, settled, stop } = await stubCore();
  onTestFinished(stop);
  await settled();
  // Core registers the dock's commands under the package of the dock plugin's
  // class.
  const registered = new Set([...core.commands.registered().keys(), ...dockCommands.map((command) => id(DOCK.package, command.id))]);

  // The skill names plugins too, by the ids of their classes, as the dock's
  // and that of core's Edit board plugin.
  const named = namedIds(SKILL);
  named.delete(id(DOCK.package, DOCK.id));
  for (const plugin of core.plugins.appPlugins().keys()) named.delete(plugin);

  expect(named.size).toBeGreaterThan(0);
  expect([...named].filter((command) => !registered.has(command))).toEqual([]);
});
