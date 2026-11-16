// A section of a card, which shows as the dock's `visible` says, and holds the
// element in which core mounts the block's UI or its output (../mounted.tsx)
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.7#sections).
//
// A section folds as it closes and unfolds as it opens, whoever changed
// `visible`, as today's section-toggle.js does, at 655e323:
// - The stylesheet moves its row's height between 0 and the height of what
//   it holds over 0.22s, unless the user's system asks for reduced motion, as
//   no browser but Chromium yet moves a height to its content's own.
// - A closed section is hidden once it has folded, so that it takes no room
//   and an output in it measures 0 by 0, for which core asks R for no output.
// - It stays mounted, so that the block's UI keeps what it holds on screen.
// - A section whose content draws nothing, as the inputs of a block whose UI
//   is empty, is hidden at once, and shows at once as it draws.

import { useLayoutEffect, useRef } from "react";
import type { ReactElement, ReactNode } from "react";
import type { Section as Name } from "../../../state/index.ts";

export function Section({
  name,
  open,
  empty = false,
  children,
}: {
  name: Name;
  // Whether the dock's `visible` shows the section.
  open: boolean;
  // Whether what the section holds is empty, for which it takes no room.
  empty?: boolean;
  children: ReactNode;
}): ReactElement {
  const ref = useRef<HTMLElement>(null);
  const was = useRef<boolean | null>(null);

  useLayoutEffect(() => {
    const el = ref.current;
    if (el === null) return;
    const before = was.current;
    was.current = open;
    if (before === null || before === open || empty) {
      settle(el, open && !empty);
      return;
    }
    return fold(el, open);
  }, [open, empty]);

  return (
    <section ref={ref} className="blockr-card-section" data-section={name}>
      <div className="blockr-card-section-body">{children}</div>
    </section>
  );
}

// Shows or hides a section at once.
function settle(el: HTMLElement, shows: boolean): void {
  delete el.dataset.folding;
  el.dataset.open = String(shows);
  el.hidden = !shows;
}

// Folds a section closed or open, and gives the function that leaves it where
// it is, as where it folds back before it has finished.
function fold(el: HTMLElement, open: boolean): () => void {
  let current = true;
  el.dataset.folding = "";
  if (open) {
    el.hidden = false;
    // The closed section's rows apply before the open ones, so that the
    // browser moves between them.
    el.getBoundingClientRect();
  }
  el.dataset.open = String(open);
  const transitions = el.getAnimations().filter((animation) => "transitionProperty" in animation);
  void Promise.allSettled(transitions.map((transition) => transition.finished)).then(() => {
    if (!current) return;
    delete el.dataset.folding;
    if (!open) el.hidden = true;
  });
  return () => {
    current = false;
  };
}
