// The JSON Schemas of the values of the dock's board options, by the board
// option's id, which say what values each takes, with a description for a
// model, as the assistant reads it.
// The store checks every value of a board option against its schema.
// The build writes each beside the board option's module, in a file named by
// the module, as inst/js/compact-option.schema.json, as core's build does for
// its own, by which R writes the value of a board option that an app script
// gives.

import type { JsonSchema, OptionId } from "@blockr/core";

export const optionSchemas = {
  compact: {
    type: "boolean",
    description: "Whether each block card shows a compact header over the block's content",
  },
  view_tabs: {
    type: "boolean",
    description: "Whether the views show as a line of tabs under the navbar",
  },
  sync_rails: {
    type: "boolean",
    description: "Whether collapsing or opening a view's rail does so on every view at once",
  },
} satisfies Record<OptionId, JsonSchema>;
