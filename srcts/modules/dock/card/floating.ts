// A menu or a tooltip floats in the dock's layer, fixed to the window beside
// the element that it opens from.
// It follows that element as the page scrolls, as the window changes its size
// and as its own size changes, as the "+" menu's does while the user filters
// it.
// Floating outside dockview's panels, it is clipped by none of them.

import { useLayoutEffect, useRef } from "react";
import type { RefObject } from "react";
import type { Box, Size, Spot } from "./place.ts";

export type Placement = (trigger: Box, size: Size, view: Size) => Spot;

// Places the element of the returned ref beside `anchor`, as `placement`
// says, from before the browser paints it on.
export function useFloating<E extends HTMLElement>(anchor: HTMLElement, placement: Placement): RefObject<E | null> {
  const ref = useRef<E>(null);
  useLayoutEffect(() => {
    const el = ref.current;
    if (el === null) return;
    const doc = el.ownerDocument;
    const win = doc.defaultView ?? window;
    const place = () => {
      const view = { width: win.innerWidth, height: win.innerHeight };
      const spot = placement(anchor.getBoundingClientRect(), el.getBoundingClientRect(), view);
      el.style.left = `${String(Math.round(spot.left))}px`;
      el.style.top = `${String(Math.round(spot.top))}px`;
    };
    place();
    const resizes = typeof win.ResizeObserver === "function" ? new win.ResizeObserver(place) : undefined;
    resizes?.observe(el);
    win.addEventListener("resize", place);
    doc.addEventListener("scroll", place, true);
    return () => {
      resizes?.disconnect();
      win.removeEventListener("resize", place);
      doc.removeEventListener("scroll", place, true);
    };
  }, [anchor, placement]);
  return ref;
}
