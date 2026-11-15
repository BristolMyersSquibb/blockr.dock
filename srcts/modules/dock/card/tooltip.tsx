// The tooltip of the design system, which the card gives its mark, its name
// and its tools
// (https://github.com/BristolMyersSquibb/blockr.ui/blob/e14e8310481e98b156871bf96669b41cb88c8435/vignettes/articles/design-system.Rmd#L1167-L1195).
// Blockr.ui's script, which draws it today, is not on the page that core
// serves, so the card draws it as blockr.ui's script does:
// - It is a light card of one line or several, where a line may hold a
//   neutral badge after its text, as the mark's package, or be muted, as the
//   gesture after a name that is cut off.
// - It shows after the pointer rests 300ms, at once on keyboard focus, and at
//   once within 400ms of another tooltip leaving, so that moving along a row
//   of icons does not wait at each one.
// - It leaves as the pointer or the focus leaves, on a click, on Escape and
//   as the page scrolls.
// - It sits above its element, and below only where there is no room above.
// Unlike blockr.ui's, it stays while the pointer moves from its element onto
// it, and leaves with the pointer from there, so that a user who magnifies the
// screen can read it (WCAG 1.4.13).
// No tooltip shows for a touch, which has no hover.

import { useCallback, useEffect, useRef, useState } from "react";
import type { FocusEvent, PointerEvent, ReactElement, ReactNode } from "react";
import { createPortal } from "react-dom";
import { useFloating } from "./floating.ts";
import { tooltipSpot } from "./place.ts";
import { useScope } from "./scope.ts";
import type { Tooltips } from "./scope.ts";

export interface TipLine {
  readonly text: string;
  readonly badge?: string;
  readonly muted?: boolean;
}

// What a tooltip shows: a line of text, or lines, or what a function gives
// for the element as the tooltip shows, or nothing for null.
export type Tip = string | readonly TipLine[];
export type TipSource = Tip | ((el: HTMLElement) => Tip | null) | null;

export interface TipProps {
  readonly onPointerEnter: (event: PointerEvent<HTMLElement>) => void;
  readonly onPointerLeave: () => void;
  readonly onPointerDown: () => void;
  readonly onFocus: (event: FocusEvent<HTMLElement>) => void;
  readonly onBlur: () => void;
}

const DELAY = 300;
const WARM = 400;
// How long a tooltip stays as the pointer leaves its element, for the pointer
// to reach the tooltip.
const GRACE = 100;

// The tooltips of a dock, of which one shows at a time.
export function createTooltips(): Tooltips {
  let current: (() => void) | undefined;
  let warmUntil = 0;
  return {
    showing(hide) {
      if (current !== undefined && current !== hide) current();
      current = hide;
    },
    left() {
      warmUntil = Date.now() + WARM;
    },
    warm: () => Date.now() < warmUntil,
  };
}

interface Shown {
  readonly el: HTMLElement;
  readonly tip: Tip;
}

type Timer = ReturnType<typeof setTimeout>;

// The props that give an element its tooltip, and the tooltip, which the
// element's component draws.
export function useTooltip(source: TipSource): { readonly props: TipProps; readonly tooltip: ReactNode } {
  const { layer, tooltips } = useScope();
  const [shown, setShown] = useState<Shown | null>(null);
  const timers = useRef<{ show?: Timer; hide?: Timer }>({});
  const showing = useRef(false);

  const hide = useCallback(() => {
    clearTimeout(timers.current.show);
    clearTimeout(timers.current.hide);
    timers.current = {};
    if (!showing.current) return;
    showing.current = false;
    tooltips.left();
    setShown(null);
  }, [tooltips]);

  const show = (el: HTMLElement) => {
    const tip = typeof source === "function" ? source(el) : source;
    if (tip === null || tip.length === 0 || !el.isConnected) return;
    tooltips.showing(hide);
    showing.current = true;
    setShown({ el, tip });
  };

  const stay = () => {
    clearTimeout(timers.current.hide);
  };

  useEffect(() => {
    if (shown === null) return;
    const doc = shown.el.ownerDocument;
    const onKey = (event: KeyboardEvent) => {
      if (event.key === "Escape") hide();
    };
    doc.addEventListener("keydown", onKey, true);
    doc.addEventListener("scroll", hide, true);
    return () => {
      doc.removeEventListener("keydown", onKey, true);
      doc.removeEventListener("scroll", hide, true);
    };
  }, [shown, hide]);

  useEffect(() => hide, [hide]);

  const props: TipProps = {
    onPointerEnter(event) {
      if (event.pointerType === "touch") return;
      stay();
      if (showing.current) return;
      const el = event.currentTarget;
      clearTimeout(timers.current.show);
      if (tooltips.warm()) show(el);
      else
        timers.current.show = setTimeout(() => {
          show(el);
        }, DELAY);
    },
    onPointerLeave() {
      clearTimeout(timers.current.show);
      if (showing.current) timers.current.hide = setTimeout(hide, GRACE);
    },
    onPointerDown: hide,
    onFocus(event) {
      // A click focuses a button too, right after its pointerdown hid the
      // tooltip, so only the keyboard's focus shows it.
      if (event.currentTarget.matches(":focus-visible")) show(event.currentTarget);
    },
    onBlur: hide,
  };

  const tooltip =
    shown === null || layer === null
      ? null
      : createPortal(<TooltipCard anchor={shown.el} tip={shown.tip} onPointerEnter={stay} onPointerLeave={hide} />, layer);
  return { props, tooltip };
}

function TooltipCard({
  anchor,
  tip,
  onPointerEnter,
  onPointerLeave,
}: {
  anchor: HTMLElement;
  tip: Tip;
  onPointerEnter: () => void;
  onPointerLeave: () => void;
}): ReactElement {
  const ref = useFloating<HTMLDivElement>(anchor, tooltipSpot);
  const lines = typeof tip === "string" ? [{ text: tip }] : tip;
  return (
    <div ref={ref} role="tooltip" className="blockr-card-tooltip" onPointerEnter={onPointerEnter} onPointerLeave={onPointerLeave}>
      {lines.map((line, i) => (
        <div key={i} className="blockr-card-tooltip-line" data-muted={line.muted === true ? "" : undefined}>
          {line.text}
          {line.badge === undefined ? null : (
            <>
              {" "}
              <span className="blockr-card-tooltip-badge">{line.badge}</span>
            </>
          )}
        </div>
      ))}
    </div>
  );
}
