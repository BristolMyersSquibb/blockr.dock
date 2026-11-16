// Where script places a popover that CSS anchor positioning would place, in a
// browser without anchor positioning: the panel picker (./picker.tsx) and the
// menus of the bar (./menu.tsx).
// The browser draws such a popover in the top layer, so that nothing clips it.
// - A popover below its anchor goes 4 pixels below it, or above it where the
//   window has no room below, and lines up with the anchor's left edge, or
//   moves left as far as the window needs.
// - A popover beside its anchor, such as a menu inside a menu, goes 4 pixels
//   to the anchor's right, or to its left where the window has no room on the
//   right, and lines up with the anchor's top, or moves up as far as the
//   window needs.

export type Side = "below" | "beside";

const GAP = 4;

// Places the popover by its anchor, unless the browser places it with CSS.
export function place(el: HTMLElement, anchor: HTMLElement, side: Side = "below"): void {
  const win = el.ownerDocument.defaultView;
  if (win === null || win.CSS.supports("position-area", "block-end")) return;
  const at = anchor.getBoundingClientRect();
  const { width, height } = el.getBoundingClientRect();
  let left: number;
  let top: number;
  if (side === "below") {
    const above = at.top - GAP - height;
    top = at.bottom + GAP + height > win.innerHeight && above >= 0 ? above : at.bottom + GAP;
    left = Math.max(0, Math.min(at.left, win.innerWidth - width));
  } else {
    const before = at.left - GAP - width;
    left = at.right + GAP + width > win.innerWidth && before >= 0 ? before : at.right + GAP;
    top = Math.max(0, Math.min(at.top, win.innerHeight - height));
  }
  el.style.left = `${String(left)}px`;
  el.style.top = `${String(top)}px`;
}
