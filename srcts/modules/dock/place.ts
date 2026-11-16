// Where a menu or a tooltip sits beside the element that it opens from, in
// the window's coordinates, as the design system places them
// (https://github.com/BristolMyersSquibb/blockr.ui/blob/e14e8310481e98b156871bf96669b41cb88c8435/vignettes/articles/design-system.Rmd#L1062-L1195).
// Both stay 8px from the window's edges.
// The browser places a menu with CSS anchor positioning where it has it, and
// script places it so where it does not (./menu.tsx).

export interface Box {
  readonly left: number;
  readonly top: number;
  readonly right: number;
  readonly bottom: number;
  readonly width: number;
}

export interface Size {
  readonly width: number;
  readonly height: number;
}

export interface Spot {
  readonly left: number;
  readonly top: number;
}

// Which edge of its button a menu lines up with: the left, or the right for a
// button at the end of a row, as the "…" button of a card's header.
export type Align = "start" | "end";

const EDGE = 8;
const MENU_GAP = 4;
const TOOLTIP_GAP = 6;

// The value nearest to `value` from `low` to `high`, or `low` where `high` is
// below it, as where a menu is wider than the window.
function clamp(value: number, low: number, high: number): number {
  return Math.max(low, Math.min(value, high));
}

// A menu sits 4px below its button, or above it where it fits there but not
// below, and lines up with the button's edge that `align` names.
export function menuSpot(trigger: Box, menu: Size, view: Size, align: Align = "start"): Spot {
  const below = trigger.bottom + MENU_GAP;
  const above = trigger.top - MENU_GAP - menu.height;
  const top = below + menu.height > view.height - EDGE && above >= EDGE ? above : below;
  const left = align === "end" ? trigger.right - menu.width : trigger.left;
  return {
    left: clamp(left, EDGE, view.width - EDGE - menu.width),
    top: clamp(top, EDGE, view.height - EDGE - menu.height),
  };
}

// A tooltip sits 6px above its element, centred on it, and below it only where
// there is no room above.
export function tooltipSpot(trigger: Box, tooltip: Size, view: Size): Spot {
  const above = trigger.top - TOOLTIP_GAP - tooltip.height;
  return {
    left: clamp(trigger.left + trigger.width / 2 - tooltip.width / 2, EDGE, view.width - EDGE - tooltip.width),
    top: above < EDGE ? trigger.bottom + TOOLTIP_GAP : above,
  };
}
