// srcts/options/schemas.ts
var optionSchemas = {
  compact: {
    type: "boolean",
    description: "Whether each block card shows a compact header over the block's content"
  },
  view_tabs: {
    type: "boolean",
    description: "Whether the views show as a line of tabs under the navbar"
  },
  sync_rails: {
    type: "boolean",
    description: "Whether collapsing or opening a view's rail does so on every view at once"
  }
};

// srcts/options/ui.tsx
import { reactModule, useValue } from "@blockr/core/react";
import { Checkbox } from "@blockr/ui";
import { jsx } from "react/jsx-runtime";
function checkboxUi(label) {
  function CheckboxUi({ host }) {
    return /* @__PURE__ */ jsx(Checkbox, { bound: useValue(host), children: label });
  }
  return reactModule(CheckboxUi);
}

// srcts/options/options.ts
var compact = {
  label: "Compact",
  category: "Theme options",
  schema: optionSchemas.compact,
  default: logicalDefault("compact", false),
  ui: checkboxUi("Compact")
};
var viewTabs = {
  label: "Views as tabs",
  category: "Theme options",
  schema: optionSchemas.view_tabs,
  default: logicalDefault("view_tabs", false),
  ui: checkboxUi("Views as tabs")
};
var syncRails = {
  label: "Sync rails across views",
  category: "Board options",
  schema: optionSchemas.sync_rails,
  default: logicalDefault("sync_rails", true),
  ui: checkboxUi("Sync rails across views")
};
function logicalDefault(id, fallback) {
  return (core) => logicalOption(core, id, fallback, `which the board option ${id} does not take`);
}
async function logicalOption(core, name, fallback, refused) {
  return await readOption(core, name, fallback, refused, (sent) => {
    const value = typeof sent === "string" ? asLogical(sent) : sent;
    return typeof value === "boolean" ? value : void 0;
  });
}
async function readOption(core, name, fallback, refused, read) {
  const sent = await core.blockrOption(name);
  if (sent === void 0) return fallback;
  const value = read(sent);
  if (value !== void 0) return value;
  throw new TypeError(`The blockr option ${name} is ${JSON.stringify(sent)}, ${refused}.`);
}
function asLogical(text) {
  const trimmed = text.trim();
  if (["TRUE", "true", "True", "T"].includes(trimmed)) return true;
  if (["FALSE", "false", "False", "F"].includes(trimmed)) return false;
  return void 0;
}

// srcts/modules/compact-option/index.ts
var api = 1;
var compact_option_default = compact;
export {
  api,
  compact_option_default as default
};
