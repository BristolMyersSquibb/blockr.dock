// Whether the window is narrower than the blockr option narrow_breakpoint,
// where the dock draws the narrow column
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.1#drawing-a-view).
// It follows the window as it is resized, so the dock switches between the
// two layouts whenever the window crosses the breakpoint.
// Today's dock decides once per session, from the width that the browser
// reports as the session starts.

export interface Narrow {
  // Whether the window is narrow now.
  get(): boolean;
  // Calls `fn` each time the window crosses the breakpoint, until the
  // returned function runs.
  subscribe(fn: () => void): () => void;
  // Stops following the window.
  stop(): void;
}

// Follows the window against a breakpoint in CSS pixels: Infinity, which
// every window is narrower than, or undefined, which none is.
export function trackNarrow(win: Window, breakpoint: number | undefined): Narrow {
  const narrowNow = () => breakpoint !== undefined && win.innerWidth < breakpoint;
  let narrow = narrowNow();
  const listeners = new Set<() => void>();
  const onResize = () => {
    const now = narrowNow();
    if (now === narrow) return;
    narrow = now;
    for (const fn of [...listeners]) fn();
  };
  const follows = breakpoint !== undefined && Number.isFinite(breakpoint);
  if (follows) win.addEventListener("resize", onResize);
  return {
    get: () => narrow,
    subscribe(fn) {
      listeners.add(fn);
      return () => {
        listeners.delete(fn);
      };
    },
    stop() {
      if (follows) win.removeEventListener("resize", onResize);
      listeners.clear();
    },
  };
}
