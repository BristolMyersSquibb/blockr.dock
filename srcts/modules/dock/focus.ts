// Where Tab and Shift+Tab move the focus in the options sidebar
// (./options-sidebar.tsx), which keeps the focus inside it.

// What Tab moves the focus to, as today's options sidebar finds it, at
// 655e323 (inst/assets/js/options-sidebar.js), and the summary of a `details`
// element, such as a category of the options sidebar.
export const FOCUSABLE = [
  "a[href]",
  "button:not([disabled])",
  'input:not([disabled]):not([type="hidden"])',
  "select:not([disabled])",
  "textarea:not([disabled])",
  '[tabindex]:not([tabindex="-1"])',
  "details > summary:first-of-type",
].join(", ");

// Keeps the focus in `root` where Tab or Shift+Tab would take it out, by
// moving it to the other end of `root`.
// From `root` itself, Tab moves the focus to the first control, and Shift+Tab
// to the last.
// Where `root` holds no control that Tab reaches, `root` takes the focus.
// A control that is hidden or inert takes no part, since Tab skips it.
export function keepFocus(root: HTMLElement, event: KeyboardEvent): void {
  const focusable = [...root.querySelectorAll<HTMLElement>(FOCUSABLE)].filter(
    (el) => el.checkVisibility({ visibilityProperty: true }) && el.closest("[inert]") === null,
  );
  const first = focusable[0];
  const last = focusable.at(-1);
  const active = root.ownerDocument.activeElement;
  if (first === undefined || last === undefined) {
    event.preventDefault();
    root.focus();
  } else if (event.shiftKey && (active === first || active === root)) {
    event.preventDefault();
    last.focus();
  } else if (!event.shiftKey && (active === last || active === root)) {
    event.preventDefault();
    first.focus();
  }
}
