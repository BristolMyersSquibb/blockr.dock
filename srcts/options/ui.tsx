// The UI module of each of the dock's board options, a React component drawn
// with the checkbox of core's React SDK and bound to the board option's
// value, so that it keeps the contract's rules as core's UIs do
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.7#the-contract).
// Today's dock draws a switch, which core's SDK has none of.

import type { OptionHost, OptionModule } from "@blockr/core";
import { Checkbox, reactModule, useValue } from "@blockr/core/react";
import type { ModuleProps } from "@blockr/core/react";
import type { ReactElement } from "react";

// A checkbox with the board option's label, which submits whether it is
// checked as the user clicks it.
export function checkboxUi(label: string): OptionModule<boolean> {
  function CheckboxUi({ host }: ModuleProps<OptionHost<boolean>>): ReactElement {
    return <Checkbox bound={useValue(host)} label={label} />;
  }
  return reactModule(CheckboxUi);
}
