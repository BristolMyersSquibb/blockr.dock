// The dock's command for the sections of a block's card
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.1#card-sections).
// The state lists only the cards that hide a section, so setting both
// sections removes the block's entry.
// On a locked board, no command sets a card's sections, as a locked card
// offers no toggle today.

import type { BlockId } from "@blockr/core";
import type { Section } from "../state/index.ts";
import { at, command, stateOf } from "./common.ts";
import * as schemas from "./schemas.ts";

// The sections of a card, in the order in which the card shows them.
const SECTIONS: readonly Section[] = ["inputs", "outputs"];

interface SetSections {
  readonly block: BlockId;
  readonly sections: readonly Section[];
}

export const sectionCommands = [
  command<SetSections>({
    id: "blockr.dock-set_sections",
    label: "Set card sections",
    description:
      "Sets the sections of a block's card that show: its inputs, its outputs, both, or neither. A card shows both unless this sets it otherwise.",
    args: schemas.object(
      {
        block: { type: "string", description: "The id of a block of the board." },
        sections: {
          type: "array",
          items: schemas.ref("Section", "A section of a block's card."),
          uniqueItems: true,
          description: 'The sections that the card shows, of "inputs" and "outputs".',
        },
      },
      ["block", "sections"],
    ),
    enabled(board, { block }, { lock }) {
      return !lock.board && stateOf(board) !== undefined && Object.hasOwn(board.blocks, block);
    },
    run(board, { block, sections }) {
      const state = stateOf(board);
      if (state === undefined) return [];
      const shown = SECTIONS.filter((section) => sections.includes(section));
      if (shown.length === SECTIONS.length) {
        return Object.hasOwn(state.visible, block) ? { patches: [{ op: "remove", path: at("visible", block) }] } : [];
      }
      return { patches: [{ op: "add", path: at("visible", block), value: shown }] };
    },
  }),
];
