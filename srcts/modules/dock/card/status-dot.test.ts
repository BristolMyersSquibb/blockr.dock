// The spec of the status dot, against the table of the page
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.7#the-status-dot),
// as blockr.graph checks its copy of the spec too.

import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import type { BlockStatus, Notification, NotificationSource, Severity, Status } from "@blockr/core";
import { describe, expect, test } from "vitest";
import { statusDot } from "./status-dot.ts";

// The table of the page: each status, with whether its dot is a ring, the
// design system's token of its colour, and its tooltip.
const TABLE: readonly [Status, "filled" | "ring", string, string][] = [
  ["stale", "filled", "text-muted", "Inputs changed since this block last ran"],
  ["waiting", "ring", "border-warning", "Waiting for a data input"],
  ["invalid", "filled", "border-warning", "Set this block's inputs"],
  ["failed", "filled", "border-danger", "Evaluation failed"],
  ["ready", "filled", "border-success", "Up to date"],
  ["unevaluated", "ring", "text-muted", "Not evaluated yet"],
];

function blockStatus(status: Status): BlockStatus {
  return {
    status,
    revision: 2,
    evaluated: status === "unevaluated" ? null : 1,
    running: false,
    conditions: [],
    stdout: "",
    stderr: "",
    summary: null,
    reason: null,
    keptWaiting: null,
    unsent: null,
  };
}

function notification(severity: Severity, source: NotificationSource): Notification {
  return { id: `${severity}-${source.kind}`, severity, message: "The output failed.", source, about: { block: "b" } };
}

const ofOutput: NotificationSource = { kind: "r", class: ["simpleError", "error", "condition"], request: { type: "output" } };
const unanswered: NotificationSource = { kind: "core", code: "output_unanswered" };

describe("the status dot", () => {
  test("of each status is filled or a ring, with the tooltip of the table", () => {
    for (const [status, shape, , tooltip] of TABLE) {
      expect(statusDot(blockStatus(status), [])).toEqual({ status, ring: shape === "ring", tooltip });
    }
  });

  test("of each status takes core's colour for it, with the light value of the table's token as the fallback", () => {
    // The light values of the tokens, as the notes of the page give them.
    const light: Readonly<Record<string, string>> = {
      "text-muted": "#6b7280",
      "border-warning": "#d97706",
      "border-danger": "#dc2626",
      "border-success": "#16a34a",
    };
    // The card's stylesheet without its comments, by its path from this file.
    const read = (path: string) => readFileSync(fileURLToPath(new URL(path, import.meta.url)), "utf8");
    const css = read("./card.css").replace(/\/\*[\s\S]*?\*\//g, "");
    for (const [status, , token] of TABLE) {
      expect(css).toContain(
        `.blockr-card-dot[data-status="${status}"] { color: var(--bc-color-status-${status}, ${light[token] ?? token}); }`,
      );
    }
  });

  test("reads failed while an error about the block's output holds, R's or core's", () => {
    for (const status of ["ready", "waiting", "invalid", "unevaluated"] as const) {
      expect(statusDot(blockStatus(status), [notification("error", ofOutput)])).toEqual({
        status: "failed",
        ring: false,
        tooltip: "Evaluation failed",
      });
      expect(statusDot(blockStatus(status), [notification("error", unanswered)])?.status).toBe("failed");
    }
  });

  test("reads stale for a stale block whose output failed, whose errors were raised for its inputs before", () => {
    expect(statusDot(blockStatus("stale"), [notification("error", ofOutput)])?.status).toBe("stale");
  });

  test("reads the block's status beside a warning about its output, or an error that is not about its output", () => {
    const others: NotificationSource[] = [
      { kind: "r", class: ["simpleError", "error", "condition"], request: { type: "evaluate", phase: "block_eval" } },
      { kind: "r", class: ["simpleError", "error", "condition"], request: { type: "renderer", name: "columns" } },
      { kind: "core", code: "module_failed", module: { object: { block: "b" }, renderer: "blockr.core-table_output" } },
      { kind: "module", code: "render_failed", module: { object: { block: "b" }, renderer: "blockr.core-table_output" } },
    ];
    const notifications = [notification("warning", ofOutput), ...others.map((source) => notification("error", source))];
    expect(statusDot(blockStatus("ready"), notifications)?.status).toBe("ready");
  });

  test("is none while the scheduler holds no status of the block", () => {
    expect(statusDot(undefined, [notification("error", ofOutput)])).toBeNull();
  });
});
