// The rows of the dock's "+" menu, which Append block opens from the "…"
// button, as the design system's "Picking a block" has it
// (https://github.com/BristolMyersSquibb/blockr.ui/blob/e14e8310481e98b156871bf96669b41cb88c8435/vignettes/articles/design-system.Rmd#L1197-L1214):
// one row per block class, with its 24px mark, its name and its package as a
// badge, under the title of its category.
// The rows carry no description.
// The menu lists the classes that take a link, as a block appended to another
// takes the other's result.
// The categories come by name, but for the classes without one, which come
// last, and the classes by name within each, as core's picker orders them.
// The search finds a class by its name, its id, its category and its
// keywords.
// The design system's tool "Open in the sidebar" is left out, since the dock
// has no block browser for it to open.

import type { RegistryEntry } from "@blockr/core";
import type { PickerGroup } from "../picker.tsx";
import { categoryOf, Mark, packageOf } from "./mark.tsx";

const UNCATEGORIZED = categoryOf(undefined);

// The rows of the classes given, by category, each of which picks its class.
export function classGroups(entries: readonly RegistryEntry[], pick: (entry: RegistryEntry) => void): PickerGroup[] {
  const groups = new Map<string, RegistryEntry[]>();
  for (const entry of entries) {
    const category = categoryOf(entry);
    groups.set(category, [...(groups.get(category) ?? []), entry]);
  }
  const last = (category: string) => (category === UNCATEGORIZED ? 1 : 0);
  return [...groups]
    .sort(([a], [b]) => last(a) - last(b) || a.localeCompare(b))
    .map(([category, list]) => ({
      key: category,
      title: category,
      rows: list
        .sort((a, b) => a.name.localeCompare(b.name))
        .map((entry) => {
          const pkg = packageOf(entry.id);
          return {
            key: entry.id,
            label: entry.name,
            lead: <Mark entry={entry} size={24} />,
            ...(pkg === null ? {} : { badge: pkg }),
            search: [entry.id, entry.category, ...entry.keywords].join(" "),
            onPick: () => {
              pick(entry);
            },
          };
        }),
    }));
}
