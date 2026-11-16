// The icons of the bar and its menus, each an inline SVG in the colour of the
// text around it, which screen readers skip.
// The chevron and the warning are those of blockr.ui's icons, chevron.svg and
// warning.svg, drawn here since blockr.ui gives the client no icons yet.

import type { ReactElement } from "react";

// The one chevron of blockr.ui's design system: a 12 pixel box, stroked at
// 1.4 pixels whatever its size, which points down, or right as a "go to"
// arrow, such as that of a menu inside a menu.
// A menu's chevron flips up while the menu is open (./index.css).
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

// A window with a panel at its right edge, for the button that opens the
// options sidebar, as today's options button draws Bootstrap's
// layout-sidebar-reverse rather than a gear, since the gear stands for a
// block's settings.
export function SidebarIcon(): ReactElement {
  return (
    <svg
      width="16"
      height="16"
      viewBox="0 0 16 16"
      fill="none"
      stroke="currentColor"
      strokeWidth="1.5"
      strokeLinejoin="round"
      aria-hidden="true"
      focusable="false"
    >
      <rect x="1.75" y="2.75" width="12.5" height="10.5" rx="1.75" />
      <path d="M10.25 2.75v10.5" />
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
