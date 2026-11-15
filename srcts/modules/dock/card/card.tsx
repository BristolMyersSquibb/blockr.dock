// The card that the dock draws in a block's panel
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.7#the-card):
// its header (./header.tsx), then its two sections, `inputs` above
// `outputs` (./section.tsx).
// The card fills its panel, on the panel's surface, with today's inset of
// 16px, and the output runs to the card's edges.
//
// Core mounts the block's UI with mountBlock() in the `inputs` section, and
// the block's output with mountOutput() in the `outputs` section.
// The card mounts both as dockview creates its panel, and calls their cleanups
// as dockview disposes of the panel, as when the panel closes or the block
// leaves the board.
// So the UI and the output stay mounted while the panel sits behind another
// tab.
// The card follows the board through store.subscribe(), and draws nothing
// once the block has left the board, before dockview disposes of the panel.
//
// The card watches the element of the block's UI with a MutationObserver, since
// a module written in React draws after its render() returns, and shows the
// Controls toggle once the UI has drawn anything there.
// Until then the `inputs` section takes no room, so a block whose UI draws
// nothing shows no empty section either.
//
// The output has a height of its own, as core's card gives it, since core asks
// R for an output only once its element measures more than 0 by 0.
//
// The board option compact turns the header into an eyebrow over the block's
// content (./card.css), which the card reads through core.options.
//
// The card gives its block focus in CoreApi.focus as the user clicks into it
// or moves the keyboard's focus into it, which reaches the card from its menus
// too, through React's tree, so that a plugin such as the assistant knows which
// block the user is looking at.
// Hovering over the card gives no focus.
//
// Under the board lock, the card mounts the UI only of a block that the board
// lock leaves open, as store.locked() says, and shows no `inputs` section for
// any other block, whatever `visible` says.

import type { BlockId } from "@blockr/core";
import { useCallback, useState } from "react";
import type { ReactElement } from "react";
import { SECTIONS, stateOf } from "../../../commands/index.ts";
import { Header } from "./header.tsx";
import { giveFocus, useBoard, useScope } from "./scope.ts";
import { Mounted, Section } from "./section.tsx";

export function BlockCard({ id }: { id: BlockId }): ReactElement | null {
  const { core } = useScope();
  const block = useBoard(core, (board) => board.blocks[id]);
  const visible = useBoard(core, (board) => stateOf(board)?.visible[id]);
  const compact = useBoard(core, () => core.options.value("compact") === true);
  const [drawn, setDrawn] = useState(false);

  const focus = useCallback(() => {
    giveFocus(core, id);
  }, [core, id]);

  const mountUi = useCallback(
    (el: HTMLElement) => {
      const cleanup = core.mountBlock(el, id);
      const seen = () => {
        setDrawn(el.hasChildNodes());
      };
      seen();
      const watch = new MutationObserver(seen);
      watch.observe(el, { childList: true });
      return () => {
        watch.disconnect();
        cleanup();
      };
    },
    [core, id],
  );
  const mountOutput = useCallback((el: HTMLElement) => core.mountOutput(el, id), [core, id]);

  if (block === undefined) return null;
  const sections = visible ?? SECTIONS;
  const entry = core.blockRegistry()[block.registry_id];
  const inputs = !core.store.locked(id);

  return (
    <article
      className="blockr-card"
      aria-label={block.name}
      data-compact={compact ? "" : undefined}
      onPointerDown={focus}
      onFocus={focus}
    >
      <Header id={id} block={block} entry={entry} sections={sections} drawn={inputs && drawn} />
      <div className="blockr-card-sections">
        {inputs ? (
          <Section name="inputs" open={sections.includes("inputs")} empty={!drawn}>
            <Mounted mount={mountUi} label="Inputs" />
          </Section>
        ) : null}
        <Section name="outputs" open={sections.includes("outputs")}>
          <Mounted mount={mountOutput} label="Output" className="blockr-card-output" />
        </Section>
      </div>
    </article>
  );
}
