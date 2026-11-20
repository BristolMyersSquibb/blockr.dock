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
// - It ignores the pointer, which reaches what lies under it, as the tab strip
//   under the tooltips of a card's header.
// - It is a popover of the manual kind, which the browser draws in the top
//   layer, so that no panel clips it, and which closes no menu as it shows.
// No tooltip shows for a touch, which has no hover.
// An element that React does not draw, such as the tab that dockview draws
// around a panel's tab, takes a tooltip through bindTip().

import { useCallback, useEffect, useRef, useState } from "react";
import type { ReactElement, ReactNode } from "react";
import { useDock } from "../context.ts";
import type { Tooltips } from "../context.ts";
import { useFloating, usePopover } from "../floating.ts";
import { tooltipSpot } from "../place.ts";

export interface TipLine {
  readonly text: string;
  readonly badge?: string;
  readonly muted?: boolean;
}

// What a tooltip shows: a line of text, or lines, or what a function gives
// for the element as the tooltip shows, or nothing for null.
export type Tip = string | readonly TipLine[];
export type TipSource = Tip | ((el: HTMLElement) => Tip | null) | null;

// What the handlers of a tooltip read of an event: the element whose handler
// runs, and the type of the pointer, which React's events and the browser's
// both give.
export interface TipEvent {
  readonly currentTarget: HTMLElement;
}

export interface TipPointerEvent extends TipEvent {
  readonly pointerType: string;
}

export interface TipProps {
  readonly onPointerEnter: (event: TipPointerEvent) => void;
  readonly onPointerLeave: () => void;
  readonly onPointerDown: () => void;
  readonly onFocus: (event: TipEvent) => void;
  readonly onBlur: () => void;
}

const DELAY = 300;
const WARM = 400;

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

// The props that give an element its tooltip, and the tooltip, which the
// element's component draws.
export function useTooltip(source: TipSource): { readonly props: TipProps; readonly tooltip: ReactNode } {
  const { tooltips } = useDock();
  const [shown, setShown] = useState<Shown | null>(null);
  // The timer that shows the tooltip once the pointer has rested.
  const timer = useRef<ReturnType<typeof setTimeout> | undefined>(undefined);
  const showing = useRef(false);

  const hide = useCallback(() => {
    clearTimeout(timer.current);
    timer.current = undefined;
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
      if (event.pointerType === "touch" || showing.current) return;
      const el = event.currentTarget;
      clearTimeout(timer.current);
      if (tooltips.warm()) show(el);
      else
        timer.current = setTimeout(() => {
          show(el);
        }, DELAY);
    },
    onPointerLeave: hide,
    onPointerDown: hide,
    onFocus(event) {
      // A click focuses a button too, right after its pointerdown hid the
      // tooltip, so only the keyboard's focus shows it.
      if (event.currentTarget.matches(":focus-visible")) show(event.currentTarget);
    },
    onBlur: hide,
  };

  const tooltip = shown === null ? null : <TooltipCard anchor={shown.el} tip={shown.tip} />;
  return { props, tooltip };
}

// Gives an element that React does not draw the tooltip whose props are
// given, as React gives one through the props, until the returned function
// runs.
export function bindTip(el: HTMLElement, props: TipProps): () => void {
  const handlers = {
    pointerenter: (event: PointerEvent) => {
      props.onPointerEnter({ currentTarget: el, pointerType: event.pointerType });
    },
    pointerleave: () => {
      props.onPointerLeave();
    },
    pointerdown: () => {
      props.onPointerDown();
    },
    focus: () => {
      props.onFocus({ currentTarget: el });
    },
    blur: () => {
      props.onBlur();
    },
  };
  el.addEventListener("pointerenter", handlers.pointerenter);
  el.addEventListener("pointerleave", handlers.pointerleave);
  el.addEventListener("pointerdown", handlers.pointerdown);
  el.addEventListener("focus", handlers.focus);
  el.addEventListener("blur", handlers.blur);
  return () => {
    el.removeEventListener("pointerenter", handlers.pointerenter);
    el.removeEventListener("pointerleave", handlers.pointerleave);
    el.removeEventListener("pointerdown", handlers.pointerdown);
    el.removeEventListener("focus", handlers.focus);
    el.removeEventListener("blur", handlers.blur);
  };
}

function TooltipCard({ anchor, tip }: { anchor: HTMLElement; tip: Tip }): ReactElement {
  const ref = useRef<HTMLDivElement>(null);
  usePopover(ref, anchor);
  useFloating(ref, anchor, tooltipSpot);
  const lines = typeof tip === "string" ? [{ text: tip }] : tip;
  return (
    <div ref={ref} popover="manual" role="tooltip" className="blockr-card-tooltip">
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
