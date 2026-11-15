// The dock's "+" menu, which Append block opens from the "…" button, as the
// design system's "Picking a block" has it
// (https://github.com/BristolMyersSquibb/blockr.ui/blob/e14e8310481e98b156871bf96669b41cb88c8435/vignettes/articles/design-system.Rmd#L1197-L1214):
// a caption, the search on top, and the block classes under the titles of
// their categories, one row each, with its 24px mark, its name and its
// package as a badge.
// The rows carry no description.
// It lists the classes that take a link, as a block appended to another takes
// the other's result.
// The categories come by name, but for the classes without one, which come
// last, and the classes by name within each, as core's picker orders them.
// The search keeps the classes whose name, id, category or keywords hold
// every word typed.
// It floats and closes as the block's menu does (./menu.tsx), while the focus
// stays in the search, whose active descendant is the row that the arrows
// move and that Enter picks.
// The first row is the active one whenever the search changes.
// The design system's tool "Open in the sidebar" is left out, since the dock
// has no block browser for it to open.

import type { RegistryEntry } from "@blockr/core";
import { useId, useLayoutEffect, useMemo, useRef, useState } from "react";
import type { KeyboardEvent, ReactElement } from "react";
import { createPortal } from "react-dom";
import { useFloating } from "./floating.ts";
import { categoryOf, Mark, packageOf } from "./mark.tsx";
import { stepped, useDismiss } from "./menu.tsx";
import type { Closing } from "./menu.tsx";
import { menuSpot } from "./place.ts";
import { useScope } from "./scope.ts";

interface Group {
  readonly category: string;
  readonly entries: readonly RegistryEntry[];
}

const UNCATEGORIZED = categoryOf(undefined);

// The classes that the search keeps, by category.
export function grouped(entries: readonly RegistryEntry[], search: string): Group[] {
  const words = search.toLowerCase().split(/\s+/).filter((word) => word !== "");
  const kept = entries.filter((entry) => {
    const text = [entry.name, entry.id, entry.category, ...entry.keywords].join(" ").toLowerCase();
    return words.every((word) => text.includes(word));
  });
  const groups = new Map<string, RegistryEntry[]>();
  for (const entry of kept) {
    const category = categoryOf(entry);
    groups.set(category, [...(groups.get(category) ?? []), entry]);
  }
  const last = (category: string) => (category === UNCATEGORIZED ? 1 : 0);
  return [...groups]
    .sort(([a], [b]) => last(a) - last(b) || a.localeCompare(b))
    .map(([category, list]) => ({ category, entries: list.sort((a, b) => a.name.localeCompare(b.name)) }));
}

export function BlockPicker({
  anchor,
  caption,
  entries,
  onPick,
  onClose,
}: {
  anchor: HTMLElement;
  caption: string;
  entries: readonly RegistryEntry[];
  onPick: (entry: RegistryEntry) => void;
  onClose: (how: Closing) => void;
}): ReactElement | null {
  const { layer } = useScope();
  const ref = useFloating<HTMLDivElement>(anchor, menuSpot);
  const field = useRef<HTMLInputElement>(null);
  const id = useId();
  const [search, setSearch] = useState("");
  const [active, setActive] = useState(0);
  useDismiss(ref, anchor, onClose);
  useLayoutEffect(() => {
    field.current?.focus({ preventScroll: true });
  }, []);

  const groups = useMemo(() => grouped(entries, search), [entries, search]);
  const shown = groups.flatMap((group) => group.entries);
  const optionId = (i: number) => `${id}-${String(i)}`;

  // The active row stays in view as the arrows move it.
  useLayoutEffect(() => {
    ref.current?.querySelector(`[id="${optionId(active)}"]`)?.scrollIntoView({ block: "nearest" });
  });

  const pick = (entry: RegistryEntry, pointer: boolean) => {
    onClose({ restore: true, pointer });
    onPick(entry);
  };

  const onKeyDown = (event: KeyboardEvent<HTMLInputElement>) => {
    const entry = shown[active];
    switch (event.key) {
      case "ArrowDown":
      case "ArrowUp":
        if (shown.length > 0) setActive(stepped(active, event.key === "ArrowDown" ? 1 : -1, shown.length));
        break;
      case "Enter":
        if (entry !== undefined) pick(entry, false);
        break;
      case "Escape":
        onClose({ restore: true, pointer: false });
        break;
      case "Tab":
        anchor.focus({ preventScroll: true });
        onClose({ restore: false, pointer: false });
        return;
      default:
        return;
    }
    event.preventDefault();
    event.stopPropagation();
  };

  if (layer === null) return null;
  // The index of the first row of each group among the rows shown.
  const starts = groups.map((_, g) => groups.slice(0, g).reduce((count, group) => count + group.entries.length, 0));
  return createPortal(
    <div ref={ref} className="blockr-card-menu" data-picker="">
      <div className="blockr-card-menu-caption">{caption}</div>
      <div className="blockr-card-menu-search">
        <input
          ref={field}
          type="text"
          role="combobox"
          aria-label="Search blocks"
          aria-expanded="true"
          aria-controls={`${id}-list`}
          aria-autocomplete="list"
          aria-activedescendant={shown.length === 0 ? undefined : optionId(active)}
          placeholder="Search blocks"
          autoComplete="off"
          spellCheck={false}
          value={search}
          onChange={(event) => {
            setSearch(event.target.value);
            setActive(0);
          }}
          onKeyDown={onKeyDown}
        />
      </div>
      <div role="listbox" id={`${id}-list`} aria-label="Block classes">
        {groups.map((group, g) => (
          <div key={group.category} role="group" aria-labelledby={`${id}-${group.category}`}>
            <div id={`${id}-${group.category}`} role="presentation" className="blockr-card-menu-title">
              {group.category}
            </div>
            {group.entries.map((entry, j) => {
              const i = (starts[g] ?? 0) + j;
              const pkg = packageOf(entry.id);
              return (
                <div
                  key={entry.id}
                  id={optionId(i)}
                  role="option"
                  aria-selected={i === active}
                  className="blockr-card-menu-row"
                  data-active={i === active ? "" : undefined}
                  onPointerDown={(event) => {
                    event.preventDefault();
                  }}
                  onPointerMove={() => {
                    if (active !== i) setActive(i);
                  }}
                  onClick={(event) => {
                    pick(entry, event.detail !== 0);
                  }}
                >
                  <Mark entry={entry} size={24} aria-hidden="true" />
                  <span className="blockr-card-menu-label">{entry.name}</span>
                  {pkg === null ? null : <span className="blockr-card-menu-badge">{pkg}</span>}
                </div>
              );
            })}
          </div>
        ))}
        {shown.length === 0 ? <div className="blockr-card-menu-empty">No matches</div> : null}
      </div>
    </div>,
    layer,
  );
}
