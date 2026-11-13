// The dock's board options, compact, view_tabs and sync_rails: what each
// defines, the module of each, the default each reads from its blockr option,
// and their UI modules, mounted with the stub of their host.

import type { CoreApi, Json, Option } from "@blockr/core";
import { mount, stubOptionHost } from "@blockr/core/stubs";
import type { Mounted } from "@blockr/core/stubs";
import { act } from "react";
import { describe, expect, test } from "vitest";
// The modules of the dock's board options, which the build bundles on their
// own.
// eslint-disable-next-line no-restricted-imports
import compactModule, * as compactExports from "../modules/compact-option/index.ts";
// eslint-disable-next-line no-restricted-imports
import syncRailsModule, * as syncRailsExports from "../modules/sync-rails-option/index.ts";
// eslint-disable-next-line no-restricted-imports
import viewTabsModule, * as viewTabsExports from "../modules/view-tabs-option/index.ts";
import { compact, dockOptions, optionSchemas, syncRails, viewTabs } from "./index.ts";

// React warns where an update in a test runs outside act().
(globalThis as { IS_REACT_ACT_ENVIRONMENT?: boolean }).IS_REACT_ACT_ENVIRONMENT = true;

// CoreApi with its blockrOption(), with the values that R holds, which records
// what it is asked for.
function blockr(values: Readonly<Record<string, Json>> = {}) {
  const asked: string[] = [];
  const core = {
    blockrOption(name: string): Promise<Json | undefined> {
      asked.push(name);
      return Promise.resolve(Object.hasOwn(values, name) ? values[name] : undefined);
    },
  } as CoreApi;
  return { core, asked };
}

// The value that the board option's default resolves with, as core calls it
// with CoreApi.
function resolveDefault(option: Omit<Option, "id">, core: CoreApi): Promise<Json> {
  const given = option.default;
  return typeof given === "function" ? given(core) : Promise.resolve(given);
}

// The checkbox that a board option's UI draws in `el`.
function checkbox(el: HTMLElement): HTMLInputElement {
  const found = el.querySelector('input[type="checkbox"]');
  if (!(found instanceof HTMLInputElement)) throw new Error("Found no checkbox.");
  return found;
}

// Mounts the module, through React's act(), which runs what React scheduled
// before the test reads the page. React's act() resolves with what the mount
// resolved with, but its then() chains nothing, so this awaits it.
async function mountReact(exports: unknown, host: unknown): Promise<Mounted> {
  return await act(() => mount(exports, host));
}

// Does what the test does to the page or the host, and then what React
// scheduled in turn.
async function step(fn: () => void): Promise<void> {
  await act(() => {
    fn();
    return Promise.resolve();
  });
}

const LABELS = [
  ["compact", compact, "Compact"],
  ["view_tabs", viewTabs, "Views as tabs"],
  ["sync_rails", syncRails, "Sync rails across views"],
] as const;

describe("the dock's board options", () => {
  test("are defined as today's R constructors define them, with the schemas that R writes values by", () => {
    const options = Object.entries(dockOptions);

    expect(options.map(([id, { label, category }]) => ({ id, label, category }))).toEqual([
      { id: "compact", label: "Compact", category: "Theme options" },
      { id: "view_tabs", label: "Views as tabs", category: "Theme options" },
      { id: "sync_rails", label: "Sync rails across views", category: "Board options" },
    ]);
    expect(Object.fromEntries(options.map(([id, { schema }]) => [id, schema]))).toEqual(optionSchemas);
    // Each schema takes a logical, and describes it for a model.
    for (const [, { schema }] of options) {
      expect(Object.keys(schema).sort()).toEqual(["description", "type"]);
      expect(schema.type).toBe("boolean");
      expect(typeof schema.description).toBe("string");
    }
  });

  test("are each the default export of a module of its own", () => {
    const modules = [
      [compactExports, compactModule, compact],
      [viewTabsExports, viewTabsModule, viewTabs],
      [syncRailsExports, syncRailsModule, syncRails],
    ] as const;

    for (const [exports, def, option] of modules) {
      expect(exports.api).toBe(1);
      expect(def).toBe(option);
      // The module gives no id, which R names, and which core adds as it
      // registers the board option.
      expect("id" in def).toBe(false);
    }
  });

  test("have nothing to do on the page and hold values of their first shape", () => {
    for (const option of Object.values(dockOptions)) {
      expect("onChange" in option).toBe(false);
      expect("migrate" in option).toBe(false);
    }
  });

  test("take as their defaults the blockr options of their names, else their fallbacks", async () => {
    const { core, asked } = blockr();

    expect(await resolveDefault(compact, core)).toBe(false);
    expect(await resolveDefault(viewTabs, core)).toBe(false);
    expect(await resolveDefault(syncRails, core)).toBe(true);
    expect(asked).toEqual(["compact", "view_tabs", "sync_rails"]);

    const set = blockr({ compact: true, view_tabs: true, sync_rails: false }).core;
    expect(await resolveDefault(compact, set)).toBe(true);
    expect(await resolveDefault(viewTabs, set)).toBe(true);
    expect(await resolveDefault(syncRails, set)).toBe(false);
  });

  test("read the string that an environment variable sets as the logical that R's as.logical() reads", async () => {
    const reads = [
      ["TRUE", true],
      [" true ", true],
      ["True", true],
      ["T", true],
      ["FALSE", false],
      ["false", false],
      ["False", false],
      ["F", false],
    ] as const;

    for (const [text, value] of reads) {
      const { core } = blockr({ compact: text, view_tabs: text, sync_rails: text });
      expect(await resolveDefault(compact, core)).toBe(value);
      expect(await resolveDefault(viewTabs, core)).toBe(value);
      expect(await resolveDefault(syncRails, core)).toBe(value);
    }
  });

  test("fail where a blockr option holds a value other than a logical, naming it", async () => {
    await expect(resolveDefault(compact, blockr({ compact: "yes" }).core)).rejects.toThrow(
      'The blockr option compact is "yes", which the board option compact does not take.',
    );
    await expect(resolveDefault(viewTabs, blockr({ view_tabs: 1 }).core)).rejects.toThrow(
      "The blockr option view_tabs is 1, which the board option view_tabs does not take.",
    );
    await expect(resolveDefault(syncRails, blockr({ sync_rails: null }).core)).rejects.toThrow(
      "The blockr option sync_rails is null, which the board option sync_rails does not take.",
    );
    await expect(resolveDefault(syncRails, blockr({ sync_rails: [true, false] }).core)).rejects.toThrow(
      "The blockr option sync_rails is [true,false], which the board option sync_rails does not take.",
    );
  });
});

describe("the UIs of the dock's board options", () => {
  test.each(LABELS)("%s is a checkbox with its label that submits whether the user checked it", async (id, option, label) => {
    const host = stubOptionHost<boolean>({ option: id, value: false });
    const { el } = await mountReact({ api: 1, default: option.ui }, host);
    const box = checkbox(el);
    expect(el.querySelector("label")?.textContent).toBe(label);
    expect(box.checked).toBe(false);

    await step(() => {
      box.click();
    });
    expect(host.submitted).toEqual([true]);
    expect(checkbox(el).checked).toBe(true);

    await step(() => {
      checkbox(el).click();
    });
    expect(host.submitted).toEqual([true, false]);
    expect(checkbox(el).checked).toBe(false);
  });

  test("shows a value set from outside the UI, as the assistant sets one, and submits nothing", async () => {
    const host = stubOptionHost<boolean>({ option: "sync_rails", value: true });
    const { el } = await mountReact({ api: 1, default: syncRails.ui }, host);
    expect(checkbox(el).checked).toBe(true);

    await step(() => {
      host.setValue(false);
    });
    expect(checkbox(el).checked).toBe(false);
    expect(host.submitted).toEqual([]);
  });

  test("empties its element as core unmounts it", async () => {
    const host = stubOptionHost<boolean>({ option: "compact", value: false });
    const mounted = await mountReact({ api: 1, default: compact.ui }, host);

    await step(() => {
      mounted.unmount();
    });
    expect(mounted.el.childElementCount).toBe(0);
  });
});
