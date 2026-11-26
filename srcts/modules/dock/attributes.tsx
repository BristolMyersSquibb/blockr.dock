// What the dock's buttons give their element besides what blockr.ui's Button
// draws.
// That Button is React Aria's, which leaves out of its element every
// attribute that it does not know, such as `title` and `aria-keyshortcuts`.
// React Aria's `render` draws the element in its place, with those attributes.
// The dock's buttons keep the browser's tooltip, `title`, until the dock
// draws blockr.ui's tooltip
// (https://github.com/BristolMyersSquibb/blockr.dock/issues/646), and name
// their shortcuts to assistive technology in `aria-keyshortcuts`.

import type { ComponentProps, ReactElement } from "react";

export interface ButtonAttributes {
  readonly title?: string | undefined;
  readonly "aria-keyshortcuts"?: string | undefined;
}

// The `render` of blockr.ui's Button that draws its element with the
// attributes given.
export function withAttributes(attributes: ButtonAttributes): (props: ComponentProps<"button">) => ReactElement {
  return (props) => <button {...props} {...attributes} />;
}
