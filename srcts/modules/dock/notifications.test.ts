// Where the dock shows each notification of CoreApi, by who raised it and what
// it concerns, and how the dock shows the message of one.

import type { BoardObject, Notification, NotificationSource } from "@blockr/core";
import { describe, expect, test } from "vitest";
import { onCard, plainText } from "./notifications.ts";

// A notification from the source given, about the board object given, or
// about the board.
function from(source: NotificationSource, about?: BoardObject): Notification {
  return { id: "n", severity: "error", message: "Failed.", source, ...(about === undefined ? {} : { about }) };
}

// Who raises notifications: R in a condition of a block's evaluation and of
// its reply to a plugin's request, core where it fails at something, a
// block's renderer, a board option's module and a plugin.
const sources = {
  evaluation: { kind: "r", class: ["simpleError", "error", "condition"], request: { type: "evaluate", phase: "block_eval" } },
  request: {
    kind: "r",
    class: ["simpleError", "error", "condition"],
    request: { type: "plugin", name: "layout" },
    module: { object: { plugin: "blockr.dag-dag_plugin" }, package: "blockr.dag" },
  },
  core: { kind: "core", code: "module_failed", module: { object: { block: "a" }, package: "blockr.core" } },
  renderer: {
    kind: "module",
    module: { object: { block: "a" }, renderer: "blockr.core-basic_table", package: "blockr.core" },
    code: "render_failed",
  },
  option: { kind: "module", module: { object: { option: "n_rows" }, package: "blockr.core" }, code: "rows_capped" },
  plugin: { kind: "module", module: { object: { plugin: "blockr.dag-dag_plugin" }, package: "blockr.dag" }, code: "link_refused" },
} satisfies Record<string, NotificationSource>;

describe("where the dock shows a notification", () => {
  test("is the card of the block that it concerns, where R, core or a module other than a plugin's raised it", () => {
    const { evaluation, core, renderer, option, plugin } = sources;
    expect([evaluation, core, renderer, option].map((source) => onCard(from(source, { block: "a" })))).toEqual([
      true,
      true,
      true,
      true,
    ]);
    expect(onCard(from(plugin, { block: "a" }))).toBe(false);
  });

  test("is a toast where it concerns no block, whoever raised it", () => {
    const abouts: (BoardObject | undefined)[] = [
      undefined,
      { link: "ab" },
      { stack: "s" },
      { option: "n_rows" },
      { plugin: "blockr.dag-dag_plugin" },
    ];
    for (const source of Object.values(sources)) {
      expect(abouts.map((about) => onCard(from(source, about)))).toEqual([false, false, false, false, false]);
    }
  });
});

describe("the message that the dock shows", () => {
  test("is the text of a condition's message, without the escapes of cli's styles and hyperlinks, as cli's ansi_strip() gives it", () => {
    // What conditionMessage() gives for an error that cli::cli_abort() raises
    // in cli 3.6.6, under the options cli.num_colors = 256 and
    // cli.hyperlink = TRUE, and what cli::ansi_strip() makes of it.
    const message =
      '\u001b[1m\u001b[22mColumn \u001b[34m"x"\u001b[39m is not in the data.\n\u001b[36mℹ\u001b[39m See \u001b]8;;https://blockr.cloud\u0007the guide\u001b]8;;\u0007 or `dplyr::filter()`.';
    expect(plainText(message)).toBe('Column "x" is not in the data.\nℹ See the guide or `dplyr::filter()`.');
  });

  test("is without a hyperlink that ends with ESC and a backslash, and without a control sequence that opens with the one-byte CSI", () => {
    expect(plainText("\u001b]8;;https://blockr.cloud\u001b\\the guide\u001b]8;;\u001b\\ and \u009b1mbold\u009b22m")).toBe(
      "the guide and bold",
    );
  });

  test("is a message without escapes as it is, with its line breaks", () => {
    const message = "The board opened without:\n- package blockr.io 0.1.0 [x]";
    expect(plainText(message)).toBe(message);
  });
});
