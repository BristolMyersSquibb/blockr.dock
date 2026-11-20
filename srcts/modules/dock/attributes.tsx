// What the dock's buttons give their element besides what core's React SDK
// draws.
// The SDK's Button is React Aria's, which leaves out of its element every
// attribute that it does not know, such as `title` and `aria-keyshortcuts`,
// and the SDK's Button takes no ref.
// React Aria's `render` draws the element in its place, with those attributes
// and the ref too.
// The dock's buttons keep the browser's tooltip, `title`, until core's React
// SDK has a tooltip
// (https://github.com/BristolMyersSquibb/blockr.dock/issues/646), and name
// their shortcuts to assistive technology in `aria-keyshortcuts`.
// The card's "…" button takes a ref, since the dock's "+" menu opens from it
// and the focus goes back to it (./card/header.tsx).

import type { ComponentProps, ReactElement, Ref, RefCallback } from "react";

export interface ButtonAttributes {
  readonly title?: string | undefined;
  readonly "aria-keyshortcuts"?: string | undefined;
  // What takes the element, as a ref does.
  readonly ref?: (el: HTMLButtonElement | null) => void;
}

// The `render` of the SDK's Button that draws its element with the attributes
// given, and hands the element to React Aria's ref and to the ref given.
// A component makes it once, so that React hands the element over once,
// rather than each time React Aria draws the button again, as on a hover.
export function withAttributes({ ref, ...attributes }: ButtonAttributes): (props: ComponentProps<"button">) => ReactElement {
  if (ref === undefined) return (props) => <button {...props} {...attributes} />;
  // The callback that hands the element to both refs, for the ref that React
  // Aria gave last.
  let merged: { readonly own: Ref<HTMLButtonElement> | undefined; readonly set: RefCallback<HTMLButtonElement> } | undefined;
  return ({ ref: own, ...props }) => {
    const set =
      merged !== undefined && merged.own === own
        ? merged.set
        : (el: HTMLButtonElement | null) => {
            const detach = attach(own, el);
            ref(el);
            return () => {
              detach();
              ref(null);
            };
          };
    merged = { own, set };
    return <button {...props} {...attributes} ref={set} />;
  };
}

// Hands the element to the ref, and returns what takes it back, as React does
// for a ref.
function attach<T>(ref: Ref<T> | undefined, el: T | null): () => void {
  if (typeof ref === "function") {
    const cleanup = ref(el);
    return typeof cleanup === "function"
      ? cleanup
      : () => {
          ref(null);
        };
  }
  if (ref === undefined || ref === null) return () => undefined;
  ref.current = el;
  return () => {
    ref.current = null;
  };
}
