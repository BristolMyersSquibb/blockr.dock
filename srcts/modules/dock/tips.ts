// The tooltips of the bar's controls whose labels the bar may cut short
// (./navbar.tsx, ./views-menu.tsx).
// A label cut short shows in full as the tooltip, where the control has no
// tooltip of its own, as blockr.ui's design system has it.
// A control reads whether its label is cut short as the pointer comes onto it
// or the focus moves into it, since the room that it has changes with the
// window.

import type { SyntheticEvent } from "react";

// Handlers that set an element's tooltip as the pointer comes onto it or the
// focus moves into it, from whether the bar cuts its label short.
export function cutTip(text: (cut: boolean) => string | undefined) {
  const tip = (event: SyntheticEvent<HTMLElement>) => {
    setTip(event.currentTarget, text);
  };
  return { onPointerEnter: tip, onFocus: tip };
}

// Sets the element's tooltip from whether its label is cut short, or takes
// the tooltip away where `text` gives none.
export function setTip(el: HTMLElement, text: (cut: boolean) => string | undefined): void {
  const label = el.querySelector<HTMLElement>(".blockr-dock-bar-label") ?? el;
  const tooltip = text(label.scrollWidth > label.clientWidth);
  if (tooltip === undefined) el.removeAttribute("title");
  else el.setAttribute("title", tooltip);
}
