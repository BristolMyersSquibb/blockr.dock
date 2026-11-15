// The card's icons, which are blockr.ui's at e14e831, 14px on a 16px grid:
// the sliders, the eye, the dots and the bin.
// Blockr.ui has no padlock, so the closed and the open padlock are drawn in
// the stroke of its other icons.
// Each is decorative, since what holds it names what it does.

import type { ReactElement, ReactNode } from "react";

function Icon({ width = 1.25, children }: { width?: number; children: ReactNode }): ReactElement {
  return (
    <svg
      width="14"
      height="14"
      viewBox="0 0 16 16"
      fill="none"
      stroke="currentColor"
      strokeWidth={width}
      strokeLinecap="round"
      strokeLinejoin="round"
      aria-hidden="true"
    >
      {children}
    </svg>
  );
}

export function SlidersIcon(): ReactElement {
  return (
    <Icon>
      <path d="M2 4h12M2 8h12M2 12h12" />
      <circle className="blockr-card-icon-knob" cx="5" cy="4" r="1.6" />
      <circle className="blockr-card-icon-knob" cx="10" cy="8" r="1.6" />
      <circle className="blockr-card-icon-knob" cx="6" cy="12" r="1.6" />
    </Icon>
  );
}

export function EyeIcon(): ReactElement {
  return (
    <Icon width={1.4}>
      <path d="M1.5 8S4 3.5 8 3.5 14.5 8 14.5 8 12 12.5 8 12.5 1.5 8 1.5 8z" />
      <circle cx="8" cy="8" r="2" />
    </Icon>
  );
}

export function LockIcon({ open }: { open: boolean }): ReactElement {
  return (
    <Icon>
      <rect x="3.5" y="7" width="9" height="6.5" rx="1.5" />
      <path d={open ? "M5.5 7V5a2.5 2.5 0 0 1 4.67-1.25" : "M5.5 7V5a2.5 2.5 0 0 1 5 0v2"} />
    </Icon>
  );
}

export function DotsIcon(): ReactElement {
  return (
    <svg width="14" height="14" viewBox="0 0 16 16" fill="currentColor" aria-hidden="true">
      <circle cx="3.5" cy="8" r="1.2" />
      <circle cx="8" cy="8" r="1.2" />
      <circle cx="12.5" cy="8" r="1.2" />
    </svg>
  );
}

export function BinIcon(): ReactElement {
  return (
    <Icon>
      <path d="M2.5 4.5h11M6.5 4.5V3h3v1.5M4 4.5l.7 9h6.6l.7-9" />
    </Icon>
  );
}
