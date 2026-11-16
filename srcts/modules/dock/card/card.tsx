// The card that the dock draws in a block's panel
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.7#the-card):
// its header (./header.tsx), the block's errors and warnings
// (./notifications.tsx), then its two sections, `inputs` above `outputs`
// (./section.tsx).
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
// The card follows the block's status in the scheduler, and the notifications
// about the block in CoreApi.
// The status dot on the block's mark draws from both (./status-dot.ts).
// Core keeps drawing the block's last output whatever the block's status, and
// the card sets how that output looks from the block's status alone
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.7#the-output):
// - A ready block's output shows as its renderer draws it, and that of an
//   unevaluated block shows empty, since no output has come yet.
// - A stale block's output fades, as Shiny fades an output that recalculates,
//   since the dock demands the block of a card on screen, so it is about to
//   run.
// - The output of a block that failed takes no room, and the errors above the
//   sections say why.
// - The output of a waiting or an invalid block is hidden, with the status
//   note in its place: one line that holds the reason in the block's status.
// The output of a block whose status is ready shows even while an error about
// the output holds, though its dot reads failed.
// Core asks R for the output again only while its element measures more than
// 0 by 0, and withdraws the error only as R replies, so a hidden output would
// keep the error for good.
// The messages about the block fold under the output.
// The outputs section takes no room while it shows neither the output, the
// status note nor a message, as for a block that failed with no messages.
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

import type { BlockId, Status } from "@blockr/core";
import { useCallback, useMemo, useState } from "react";
import type { ReactElement } from "react";
import { dockState, SECTIONS } from "../../../commands/index.ts";
import { giveFocus, useBoard, useDock, useNotified, useStatus } from "../context.ts";
import { onCard, plainText } from "../notifications.ts";
import { Header } from "./header.tsx";
import { ErrorsAndWarnings, Messages } from "./notifications.tsx";
import { Mounted, Section } from "./section.tsx";
import { statusDot } from "./status-dot.ts";

// The statuses under which the output takes no room, and those of them under
// which the status note takes its place.
const WITHOUT_OUTPUT: ReadonlySet<Status> = new Set(["failed", "waiting", "invalid"]);
const WITH_NOTE: ReadonlySet<Status> = new Set(["waiting", "invalid"]);

export function BlockCard({ id }: { id: BlockId }): ReactElement | null {
  const { core } = useDock();
  const board = useBoard(core);
  const status = useStatus(core, id);
  const notified = useNotified(core, id);
  const [drawn, setDrawn] = useState(false);

  // The notifications that the card shows: those that no plugin raised, of
  // the severities that the board option show_conditions names.
  const severities = core.options.value("show_conditions");
  const shown = useMemo(
    () =>
      notified.filter(
        (notification) => onCard(notification) && Array.isArray(severities) && severities.includes(notification.severity),
      ),
    [notified, severities],
  );

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

  const block = board.blocks[id];
  if (block === undefined) return null;
  const sections = dockState(board)?.visible[id] ?? SECTIONS;
  const compact = core.options.value("compact") === true;
  const entry = core.blockRegistry()[block.registry_id];
  const inputs = !core.store.locked(id);
  const hidden = status !== undefined && WITHOUT_OUTPUT.has(status.status);
  const note = status !== undefined && WITH_NOTE.has(status.status) ? status.reason : null;
  const messages = shown.filter((notification) => notification.severity === "message");

  return (
    <article
      className="blockr-card"
      aria-label={block.name}
      data-compact={compact ? "" : undefined}
      onPointerDown={focus}
      onFocus={focus}
    >
      <Header
        id={id}
        block={block}
        entry={entry}
        sections={sections}
        drawn={inputs && drawn}
        dot={statusDot(status, notified)}
      />
      <ErrorsAndWarnings notifications={shown.filter((notification) => notification.severity !== "message")} />
      <div className="blockr-card-sections">
        {inputs ? (
          <Section name="inputs" open={sections.includes("inputs")} empty={!drawn}>
            <Mounted mount={mountUi} label="Inputs" />
          </Section>
        ) : null}
        <Section name="outputs" open={sections.includes("outputs")} empty={hidden && note === null && messages.length === 0}>
          <div className="blockr-card-output-frame" hidden={hidden} data-faded={status?.status === "stale" ? "" : undefined}>
            <Mounted mount={mountOutput} label="Output" className="blockr-card-output" />
          </div>
          {note === null ? null : <p className="blockr-card-status-note">{plainText(note)}</p>}
          <Messages notifications={messages} />
        </Section>
      </div>
    </article>
  );
}
