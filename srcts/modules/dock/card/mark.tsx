// The mark of a block class, as blockr.ui's block_mark() draws it: a square
// in the colour of the class's category, tinted behind the class's icon
// (https://github.com/BristolMyersSquibb/blockr.ui/blob/e14e8310481e98b156871bf96669b41cb88c8435/inst/assets/css/blockr-blocks.css#L572-L611).
// The registry entry holds the icon as SVG markup, which R makes with
// bsicons, and which the mark draws as it is, as the registry is the app's
// own.
// A class that the registry lacks takes the colour of a class without a
// category, and no icon.

import type { RegistryEntry, RegistryId } from "@blockr/core";
import type { HTMLAttributes, ReactElement } from "react";

// The category of a class that gives none, as R's registry names it.
const UNCATEGORIZED = "uncategorized";

// The category of a class, or uncategorized where it gives none.
export function categoryOf(entry: RegistryEntry | undefined): string {
  return entry === undefined || entry.category === "" ? UNCATEGORIZED : entry.category;
}

// The package of a registry id, the part before its first "-", as
// "blockr.core" of "blockr.core-head_block", or null where it has none.
export function packageOf(id: RegistryId): string | null {
  const at = id.indexOf("-");
  return at > 0 ? id.slice(0, at) : null;
}

// The mark, 32px in a card's header, which a compact header draws at 20px,
// and 24px in a row of the "+" menu.
// The props are those of the element, as the tooltip's handlers, and its name
// where it is not decorative.
export function Mark({
  entry,
  size,
  ...rest
}: {
  entry: RegistryEntry | undefined;
  size: 24 | 32;
} & Omit<HTMLAttributes<HTMLSpanElement>, "className" | "children" | "dangerouslySetInnerHTML">): ReactElement {
  return (
    <span
      {...rest}
      className="blockr-card-mark"
      data-size={size}
      data-category={categoryOf(entry)}
      dangerouslySetInnerHTML={{ __html: entry?.icon ?? "" }}
    />
  );
}
