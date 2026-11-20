// Where the dock shows each notification of CoreApi, by what it concerns and
// by the active view, and how the dock shows the message of one.

import type { Board, BoardObject, Notification, NotificationSource } from "@blockr/core";
import { describe, expect, test } from "vitest";
import { DOCK } from "../../commands/index.ts";
import type { DockState, Grid } from "../../state/index.ts";
import { asToast, inActiveView, onCard, panelPlugin, plainText, tabNotifications } from "./notifications.ts";

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

// The DAG's plugin, whose panel the dock draws, and the storage plugin, which
// has no `ui`, so that the dock draws no panel for it.
const DAG = "blockr.dag-dag_plugin";
const STORAGE = "blockr.core-file_storage_plugin";
const drawn = new Map([[DAG, "DAG"]]);

// A grid of one tab group that shows a's panel, with the DAG's panel beside
// it where `dag` says where: in the tab group, behind a's panel, or on a
// collapsed rail.
function gridWith(dag?: "group" | "rail"): Grid {
  const panels = dag === "group" ? [{ block: "a" }, { plugin: DAG }] : [{ block: "a" }];
  return {
    orientation: "horizontal",
    children: [{ panels, active: { block: "a" } }],
    sizes: [1],
    ...(dag === "rail" ? { rails: { left: { panels: [{ plugin: DAG }], active: { plugin: DAG }, collapsed: true, size: 260 } } } : {}),
  };
}

// A board whose dock state holds the views main and side, of the grids
// given, with the view given active.
function boardWith(main: Grid, side: Grid, active: "main" | "side" | null = "main"): Board {
  const state: DockState = {
    views: {
      main: { name: "Main", folder: null, index: "a0", grid: main },
      side: { name: "Side", folder: null, index: "a1", grid: side },
    },
    folders: {},
    active,
    visible: {},
  };
  return { id: "board", name: "Board", blocks: {}, links: {}, stacks: {}, options: {}, plugins: { [DOCK]: state } };
}

describe("where the dock shows a notification", () => {
  test("is the card of the block that it concerns, whoever raised it, a plugin included", () => {
    for (const source of Object.values(sources)) {
      expect(onCard(from(source, { block: "a" }))).toBe(true);
      expect(asToast(from(source, { block: "a" }), boardWith(gridWith(), gridWith()), drawn)).toBe(false);
    }
  });

  test("is a toast where it concerns no block and no plugin whose panel the dock draws, whoever raised it", () => {
    const abouts: (BoardObject | undefined)[] = [undefined, { link: "ab" }, { stack: "s" }, { option: "n_rows" }, { plugin: STORAGE }];
    // The active view holds the DAG's panel, which changes nothing for them.
    const board = boardWith(gridWith("group"), gridWith());
    for (const source of Object.values(sources)) {
      expect(abouts.map((about) => onCard(from(source, about)))).toEqual([false, false, false, false, false]);
      expect(abouts.map((about) => asToast(from(source, about), board, drawn))).toEqual([true, true, true, true, true]);
    }
  });

  test("is the panel of the plugin that it concerns while the active view holds the panel, behind another tab or on a collapsed rail too, and a toast otherwise", () => {
    const { request, core, plugin } = sources;
    const about = { plugin: DAG };
    for (const source of [request, core, plugin]) {
      const notification = from(source, about);
      expect(panelPlugin(notification, drawn)).toBe(DAG);
      expect(asToast(notification, boardWith(gridWith("group"), gridWith()), drawn)).toBe(false);
      expect(asToast(notification, boardWith(gridWith("rail"), gridWith()), drawn)).toBe(false);
      // The view side holds the panel, but main is active.
      expect(asToast(notification, boardWith(gridWith(), gridWith("group")), drawn)).toBe(true);
      expect(asToast(notification, boardWith(gridWith("group"), gridWith(), "side"), drawn)).toBe(true);
      // A state without an active view holds no panel there.
      expect(asToast(notification, boardWith(gridWith("group"), gridWith(), null), drawn)).toBe(true);
    }
    expect(panelPlugin(from(plugin, { plugin: STORAGE }), drawn)).toBeUndefined();
    expect(panelPlugin(from(plugin, { block: "a" }), drawn)).toBeUndefined();
  });

  test("holds a plugin's panel in the active view where its grid places the panel, in a tab group or on a rail", () => {
    expect(inActiveView(boardWith(gridWith("group"), gridWith()), DAG)).toBe(true);
    expect(inActiveView(boardWith(gridWith("rail"), gridWith()), DAG)).toBe(true);
    expect(inActiveView(boardWith(gridWith(), gridWith("rail")), DAG)).toBe(false);
    // A board that holds no state of the dock's holds no view.
    expect(inActiveView({ ...boardWith(gridWith("group"), gridWith()), plugins: {} }, DAG)).toBe(false);
  });
});

describe("what the tab of a plugin's panel tells of", () => {
  // A notification about the DAG's plugin, of the severity given, under the
  // id given.
  const about = (id: string, severity: Notification["severity"]): Notification => ({
    ...from(sources.plugin, { plugin: DAG }),
    id,
    severity,
  });

  test("is the errors about the plugin, then its warnings, each in the order they came, and none of its messages", () => {
    // The notifications about the plugin, which useNotified() gives the tab.
    const notified = [about("w1", "warning"), about("e1", "error"), about("m1", "message"), about("w2", "warning"), about("e2", "error")];
    expect(tabNotifications(notified).map((notification) => notification.id)).toEqual(["e1", "e2", "w1", "w2"]);
    expect(tabNotifications([about("m1", "message")])).toEqual([]);
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
