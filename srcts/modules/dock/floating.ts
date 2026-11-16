// A menu or a tooltip is a popover, which the browser draws in the top layer,
// fixed to the window beside the element that it opens from.
// Where script places it, it follows that element as the page scrolls, as the
// window changes its size and as its own size changes, as a menu's does while
// the user searches it.

import { useLayoutEffect } from "react";
import type { RefObject } from "react";
import type { Box, Size, Spot } from "./place.ts";

export type Placement = (trigger: Box, size: Size, view: Size) => Spot;

// Shows the element of the ref as a popover while the component is drawn, and
// hides it as the component goes.
// The browser has hidden a popover of the auto kind already where the user
// closed it, by Escape or by a click outside it, so the hiding throws nothing
// then.
// A component calls it before useFloating(), since script measures a popover
// only once it shows.
export function usePopover(ref: RefObject<HTMLElement | null>, source: HTMLElement): void {
  useLayoutEffect(() => {
    const el = ref.current;
    if (el === null) return;
    el.showPopover({ source });
    return () => {
      try {
        el.hidePopover();
      } catch {
        // The popover was hidden already.
      }
    };
  }, [ref, source]);
}

// Places the element of the ref beside `anchor`, as `placement` says, from
// before the browser paints it on, unless `placement` is null, as where CSS
// places it.
export function useFloating(ref: RefObject<HTMLElement | null>, anchor: HTMLElement, placement: Placement | null): void {
  useLayoutEffect(() => {
    const el = ref.current;
    if (el === null || placement === null) return;
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
  }, [ref, anchor, placement]);
}
