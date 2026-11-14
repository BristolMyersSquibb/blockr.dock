// The dock's board options, compact, view_tabs and sync_rails, each without
// the id that R gives it, which the module of each in ../modules exports, with
// its UI module, and the schemas of their values, which the build writes
// beside those modules for R.

export { compact, dockOptions, logicalOption, numericOption, syncRails, viewTabs } from "./options.ts";
export type { BlockrOptions } from "./options.ts";
export { optionSchemas } from "./schemas.ts";
