// Whether the logo on the bar turns, as the dock's busy indicator
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.5#the-docks-own-parts).
// It turns while a block that the dock demands reads `stale` or `unevaluated`,
// so while R evaluates what the user looks at.
// The dock demands the blocks whose cards are on screen (./demand.ts).
// The logo does not turn while R makes the output of a block that reads
// `ready`, whose renderer shows that wait on the block's card.
//
// It starts to turn only once the delay of the blockr option spinner_delay_ms
// is up, so that a short evaluation never shows:
// 1. A demanded block starts to read stale or unevaluated, and the delay
//    starts.
// 2. Where every demanded block reads another status again before the delay
//    is up, the logo never turns.
// 3. Otherwise the logo turns once the delay is up, and stops as soon as no
//    demanded block reads stale or unevaluated.
// It reads the statuses again whenever the scheduler says that some changed,
// and whenever the board changes or the window crosses the breakpoint of the
// narrow column, which change what the dock demands.

import type { CoreApi, PluginClassId } from "@blockr/core";
import { onScreen } from "./demand.ts";
import type { Narrow } from "./narrow.ts";

export interface Busy {
  // Whether the logo turns now.
  readonly get: () => boolean;
  // Calls `fn` whenever the logo starts or stops turning.
  readonly subscribe: (fn: () => void) => () => void;
  // Stops following the statuses, and stops the delay.
  readonly stop: () => void;
}

// Follows whether the logo turns, on a board where the dock draws the panels
// of the plugins given, after a delay in milliseconds.
export function trackBusy(
  core: Pick<CoreApi, "id" | "store" | "scheduler">,
  drawn: ReadonlyMap<PluginClassId, string>,
  narrow: Narrow,
  delay: number,
): Busy {
  let turning = false;
  let timer: ReturnType<typeof setTimeout> | undefined;
  const listeners = new Set<() => void>();

  const evaluating = () =>
    onScreen(core, core.store.board(), drawn, narrow.get()).blocks.some((block) => {
      const status = core.scheduler.status(block)?.status;
      return status === "stale" || status === "unevaluated";
    });

  function turn(on: boolean): void {
    if (on === turning) return;
    turning = on;
    for (const fn of [...listeners]) fn();
  }

  function check(): void {
    if (!evaluating()) {
      clearTimeout(timer);
      timer = undefined;
      turn(false);
    } else if (!turning && timer === undefined) {
      timer = setTimeout(() => {
        timer = undefined;
        turn(true);
      }, delay);
    }
  }

  const offs = [
    core.scheduler.subscribe(check),
    core.store.subscribe(() => {
      check();
    }),
    narrow.subscribe(check),
  ];
  check();

  return {
    get: () => turning,
    subscribe(fn) {
      listeners.add(fn);
      return () => {
        listeners.delete(fn);
      };
    },
    stop() {
      for (const off of offs) off();
      clearTimeout(timer);
      timer = undefined;
      listeners.clear();
    },
  };
}
