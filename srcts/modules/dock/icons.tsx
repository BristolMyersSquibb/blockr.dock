// The icons of the bar, its menus and the options sidebar, each an inline SVG
// in the colour of the text around it, which screen readers skip.
// The chevron, the warning and the cross are those of blockr.ui's icons,
// chevron.svg, warning.svg and remove.svg, drawn here since blockr.ui gives the
// client no icons yet.

import type { ReactElement } from "react";

// The one chevron of blockr.ui's design system: a 12 pixel box, stroked at
// 1.4 pixels whatever its size, which points down, or right as a "go to"
// arrow, such as that of a menu inside a menu.
// A menu's chevron flips up while the menu is open, and a category of the
// options sidebar turns its chevron to point right while it is collapsed
// (./index.css).
export function Chevron({ right = false }: { readonly right?: boolean }): ReactElement {
  return (
    <svg
      className="blockr-dock-chevron"
      width="12"
      height="12"
      viewBox="0 0 12 12"
      fill="none"
      stroke="currentColor"
      strokeWidth="1.4"
      strokeLinecap="round"
      strokeLinejoin="round"
      aria-hidden="true"
      focusable="false"
    >
      <polyline points={right ? "4.5 3 7.5 6 4.5 9" : "3 4.5 6 7.5 9 4.5"} vectorEffect="non-scaling-stroke" />
    </svg>
  );
}

// A warning's mark: an exclamation in a triangle.
export function WarningIcon(): ReactElement {
  return (
    <svg
      width="14"
      height="14"
      viewBox="0 0 16 16"
      fill="none"
      stroke="currentColor"
      strokeWidth="1.25"
      strokeLinecap="round"
      strokeLinejoin="round"
      aria-hidden="true"
      focusable="false"
    >
      <path d="M8 2.25l6.25 11H1.75z" />
      <path d="M8 5.75v2.5" />
      <circle cx="8" cy="10.75" r="0.75" fill="currentColor" stroke="none" />
    </svg>
  );
}

// A closed padlock, for the read-only pill and a locked board's empty view,
// 12 pixels square unless given.
export function LockIcon({ size = 12 }: { readonly size?: number }): ReactElement {
  return (
    <svg width={size} height={size} viewBox="0 0 16 16" fill="currentColor" aria-hidden="true" focusable="false">
      <path d="M8 1a3 3 0 0 0-3 3v3H4.5A1.5 1.5 0 0 0 3 8.5v5A1.5 1.5 0 0 0 4.5 15h7a1.5 1.5 0 0 0 1.5-1.5v-5A1.5 1.5 0 0 0 11.5 7H11V4a3 3 0 0 0-3-3Zm-1.75 6V4a1.75 1.75 0 1 1 3.5 0v3h-3.5Z" />
    </svg>
  );
}

// A pushpin, upright, in a 12 pixel box, stroked as the chevron is, for the
// pin button of the options sidebar, which tilts it while the sidebar is not
// pinned (./index.css).
// The icons of blockr.ui hold no pin, and today's sidebar draws Bootstrap's
// pin-angle through bsicons, which the dock no longer imports.
export function PinIcon(): ReactElement {
  return (
    <svg
      width="12"
      height="12"
      viewBox="0 0 16 16"
      fill="none"
      stroke="currentColor"
      strokeWidth="1.4"
      strokeLinecap="round"
      strokeLinejoin="round"
      aria-hidden="true"
      focusable="false"
    >
      <path d="M5.5 2h5M6.5 2v4.25L4.5 9h7l-2-2.75V2M8 9v5" vectorEffect="non-scaling-stroke" />
    </svg>
  );
}

// The cross of remove.svg, thin as every small icon of the design system, in
// a 10 pixel box, for the close button of the options sidebar.
export function CloseIcon(): ReactElement {
  return (
    <svg
      width="10"
      height="10"
      viewBox="0 0 10 10"
      fill="none"
      stroke="currentColor"
      strokeWidth="1"
      strokeLinecap="round"
      aria-hidden="true"
      focusable="false"
    >
      <path d="M2.5 2.5l5 5M7.5 2.5l-5 5" />
    </svg>
  );
}
