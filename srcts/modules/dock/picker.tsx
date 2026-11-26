// The dock's pickers, which pick from a list that the user searches: the panel
// picker, which adds a panel to the active view (./panel-picker.tsx), and the
// "+" menu of a block's card, which appends a block of the class that the user
// picks (./card/header.tsx, ./card/classes.tsx).
// Each draws with blockr.ui's components, as Picker below
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.7.3):
// - The Popover of blockr.ui holds it.
//   React Aria draws the popover in a portal at the end of the page's body,
//   never in the browser's top layer.
//   It opens 4px below the button that opened it, lined up with the button's
//   left edge, or with its right edge for a button at the end of a row, and
//   above the button where the window has no room below.
// - A caption on top says what the picker is for, and names the popover.
// - React Aria's Autocomplete holds the SearchField and the Menu of blockr.ui
//   below the caption.
//   The menu's rows come in groups, a group under its title where it has one.
//   The search keeps the rows whose label, detail or search text holds every
//   word typed, in any case, and the menu says so where it keeps none.
// - Where the picker has no row to list at all, a note says why in place of
//   the search and the menu.
// The keys are React Aria's:
// - The search takes the focus as the picker opens, and keeps it.
// - The arrows move through the rows, and Enter picks the row that they are
//   on.
//   Once the user has typed, that is the first row that the search keeps.
// - Escape clears the search, and closes the picker where the search is
//   empty.
// A pick closes the picker, and then the row runs.
// While the picker is open, React Aria keeps the rest of the page inert, and
// a press outside the picker closes it and reaches nothing else.
// As it closes, React Aria gives the focus back to the element that had it as
// the picker opened, as the button.
// A button that opens a picker says whether its picker is open.

import { Autocomplete, Menu, MenuItem, MenuSection, Popover, SearchField } from "@blockr/ui";
import { useId, useMemo } from "react";
import type { ReactElement, ReactNode } from "react";

export interface PickerRow {
  readonly key: string;
  readonly label: string;
  // What comes before the label, as the mark of a block's class.
  readonly lead?: ReactNode;
  // Muted text after the label, as the name of a block's class.
  readonly detail?: string;
  // A neutral badge at the row's end, as the package of a block class.
  readonly badge?: string;
  // Text by which the search finds the row besides its label and its detail,
  // as an id.
  readonly search?: string;
  // What the pick runs.
  readonly onPick: () => void;
}

export interface PickerGroup {
  readonly key: string;
  readonly title?: string;
  readonly rows: readonly PickerRow[];
}

export interface PickerProps {
  // The button that opened the picker.
  readonly button: HTMLElement;
  // Whether the picker lines up with the button's left edge, or with its right
  // edge.
  readonly align: "start" | "end";
  // The name of the picker's menu.
  readonly label: string;
  readonly caption: string;
  // The search, by its name and what the menu says where it keeps no row.
  readonly search: { readonly label: string; readonly empty: string };
  readonly groups: readonly PickerGroup[];
  // What the picker says where it has no row to list.
  readonly note?: string;
  // Closes the picker.
  readonly onClose: () => void;
}

// Whether a row holds every word of what the user searched, in its label, its
// detail or its search text, in any case.
export function matches(row: PickerRow, searched: string): boolean {
  const words = searched.toLowerCase().split(/\s+/).filter((word) => word !== "");
  const text = [row.label, row.detail ?? "", row.search ?? ""].join(" ").toLowerCase();
  return words.every((word) => text.includes(word));
}

// A picker, open, as the comment at the top of this file says.
export function Picker({ button, align, label, caption, search, groups, note, onClose }: PickerProps): ReactElement {
  const captionId = useId();
  const trigger = useMemo(() => ({ current: button }), [button]);
  const rows = useMemo(() => new Map(groups.flatMap((group) => group.rows.map((row) => [row.key, row]))), [groups]);

  return (
    <Popover
      triggerRef={trigger}
      isOpen
      onOpenChange={(open) => {
        if (!open) onClose();
      }}
      placement={align === "end" ? "bottom end" : "bottom start"}
      maxHeight={320}
      aria-labelledby={captionId}
      className="blockr-dock-picker"
    >
      <p id={captionId} className="blockr-dock-picker-caption">
        {caption}
      </p>
      {rows.size === 0 ? (
        note === undefined ? null : <p className="blockr-dock-picker-note">{note}</p>
      ) : (
        <Autocomplete
          filter={(_, searched, node) => {
            // A node that is no row, if React Aria hands one, stays.
            const row = rows.get(String(node.key));
            return row === undefined || matches(row, searched);
          }}
        >
          <SearchField autoFocus aria-label={search.label} placeholder={search.label} className="blockr-dock-picker-search" />
          <Menu
            aria-label={label}
            className="blockr-dock-picker-menu"
            renderEmptyState={() => <p className="blockr-dock-picker-note">{search.empty}</p>}
            onAction={(key) => {
              const row = rows.get(String(key));
              onClose();
              row?.onPick();
            }}
          >
            {groups
              .filter((group) => group.rows.length > 0)
              .map((group) => (
                <MenuSection key={group.key} id={group.key} title={group.title}>
                  {group.rows.map((row) => (
                    <Row key={row.key} row={row} />
                  ))}
                </MenuSection>
              ))}
          </Menu>
        </Autocomplete>
      )}
    </Popover>
  );
}

// A row of the picker, which assistive technology names by its label, its
// detail and its badge.
function Row({ row }: { readonly row: PickerRow }): ReactElement {
  const named = [row.label, row.detail, row.badge].filter((part) => part !== undefined);
  return (
    <MenuItem id={row.key} textValue={row.label} aria-label={named.join(", ")} className="blockr-dock-picker-row">
      <span className="blockr-dock-picker-name">
        {row.lead === undefined ? null : (
          <span className="blockr-dock-picker-lead" aria-hidden="true">
            {row.lead}
          </span>
        )}
        <span className="blockr-dock-picker-label">{row.label}</span>
        {row.detail === undefined ? null : <span className="blockr-dock-picker-detail">{row.detail}</span>}
      </span>
      {row.badge === undefined ? null : <span className="blockr-dock-picker-badge">{row.badge}</span>}
    </MenuItem>
  );
}
