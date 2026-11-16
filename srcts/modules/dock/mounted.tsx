// The element in which core mounts a module, as `mount` does it, until the
// component goes: a block's UI and its output on the block's card (./card), a
// plugin's UI in the frame of its panel (./dock.tsx), and the UI of a board
// option on a page of the options sidebar (./options-sidebar.tsx).
// Where it has a label, the element is a region of that name.
// Core mounts the module as React commits the element.
// The module may draw in a React root of its own, as core's Edit board plugin
// and the UIs of core's and the dock's board options do.
// Were the dock to end the mount as soon as React drops the element, this
// would happen:
// 1. React drops the element while it commits the dock's root, and runs the
//    cleanup of the element's effect.
// 2. The cleanup that core returned unmounts the module's root.
//    React puts that off until the commit is done.
// 3. That cleanup then empties the element.
// 4. Once the commit is done, React unmounts the module's root, and fails to
//    find what it drew in the element.
// So the dock ends the mount once the commit is done, as core's card does.

import { useLayoutEffect, useRef } from "react";
import type { ReactElement } from "react";

export interface MountedProps {
  // Mounts the module in the element, and returns what ends the mount.
  readonly mount: (el: HTMLElement) => () => void;
  readonly label?: string;
  readonly className?: string;
  // The element's data attributes, such as the plugin whose UI it holds.
  readonly data?: Readonly<Record<`data-${string}`, string>>;
}

export function Mounted({ mount, label, className, data }: MountedProps): ReactElement {
  const ref = useRef<HTMLDivElement>(null);
  useLayoutEffect(() => {
    const el = ref.current;
    if (el === null) return undefined;
    const end = mount(el);
    return () => {
      queueMicrotask(end);
    };
  }, [mount]);
  return <div ref={ref} role={label === undefined ? undefined : "region"} aria-label={label} className={className} {...data} />;
}
