// The spec of a block's status dot, which the card draws on the block's mark
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.7#the-status-dot).
// It takes the block's status and the notifications about the block, and
// nothing else, and it imports nothing of the dock's, so that blockr.graph
// can keep a copy of it for the nodes of the DAG, which give the same dot for
// the same block.
// A test in each package checks its copy against the table of the page.
//
// - The dot reads the block's status, except that it reads failed while an
//   error about the block's output holds: R's, of the request output, or
//   core's, of the code output_unanswered. So the dot of a block whose output
//   R could not make reads failed, though the block's status is ready.
// - A stale block is the exception, since its errors were raised for inputs
//   that it no longer has.
// - The dot is filled, or a ring for a block that is waiting or unevaluated,
//   so that the two grey dots differ by shape, as the two amber ones do.
// - Its colour is core's for the status that it reads, as
//   --bc-color-status-failed, so the spec gives none
//   (https://github.com/BristolMyersSquibb/blockr.core/issues/657).

import type { BlockStatus, Notification, Status } from "@blockr/core";

export interface StatusDot {
  // The status that the dot reads.
  readonly status: Status;
  // Whether the dot is a ring, rather than filled.
  readonly ring: boolean;
  readonly tooltip: string;
}

const DOTS: Readonly<Record<Status, Omit<StatusDot, "status">>> = {
  stale: { ring: false, tooltip: "Inputs changed since this block last ran" },
  waiting: { ring: true, tooltip: "Waiting for a data input" },
  invalid: { ring: false, tooltip: "Set this block's inputs" },
  failed: { ring: false, tooltip: "Evaluation failed" },
  ready: { ring: false, tooltip: "Up to date" },
  unevaluated: { ring: true, tooltip: "Not evaluated yet" },
};

// The dot of a block, from its status and the notifications about it, or no
// dot while the scheduler holds no status of the block.
export function statusDot(status: BlockStatus | undefined, notifications: readonly Notification[]): StatusDot | null {
  if (status === undefined) return null;
  const reads = status.status !== "stale" && notifications.some(isOutputError) ? "failed" : status.status;
  return { status: reads, ...DOTS[reads] };
}

// Whether the notification is an error about the block's output.
function isOutputError({ severity, source }: Notification): boolean {
  if (severity !== "error") return false;
  if (source.kind === "r") return source.request.type === "output";
  return source.kind === "core" && source.code === "output_unanswered";
}
