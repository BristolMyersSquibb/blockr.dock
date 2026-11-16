// The navbar items, which the bar across the top of the dock draws in one row,
// in their order
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.5#what-the-bar-holds).
// An app script gives them to serve() in `navbar`, and the method of R's
// frontend_args() for the dock plugin's class checks them as the app starts.
// R's reply to `hello` carries them, and core passes them to the frontend's
// render() in `args`.
// So no board and no file holds the bar.
//
// R writes each item as an object of its kind, its id and the fields of its
// kind:
// - A part, one of the dock's own parts, by its id.
// - A storage item, which draws one of the storage plugin's controls, by the
//   control's id.
// - A link, with its label and the URL that it opens in a new tab.
// - A menu, with its label and the links and further menus that it holds.
// - A module item, with the id of a module that a package offers, which draws
//   the item.
// The dock reads them as R wrote them.
// An item that it cannot read fails the frontend's render(), with a message
// that names the item, as a blockr option that the dock cannot read does
// (./settings.ts).
// Where `args` holds no navbar, the dock draws the default bar, as R's
// default_navbar() gives it.

import type { Json } from "@blockr/core";
import { isObject } from "../../state/index.ts";

// The dock's own parts of the bar, by id, in the order of the default bar.
export const PARTS = ["logo", "name", "storage", "undo_redo", "spacer", "views", "read_only", "options"] as const;

export type PartId = (typeof PARTS)[number];

export interface PartItem {
  readonly kind: "part";
  readonly id: PartId;
}

export interface StorageItem {
  readonly kind: "storage";
  readonly id: string;
}

export interface LinkItem {
  readonly kind: "link";
  readonly id: string;
  readonly label: string;
  readonly href: string;
}

export interface MenuItem {
  readonly kind: "menu";
  readonly id: string;
  readonly label: string;
  readonly items: readonly (LinkItem | MenuItem)[];
}

export interface ModuleItem {
  readonly kind: "module";
  readonly id: string;
  readonly module: string;
}

export type NavbarItem = PartItem | StorageItem | LinkItem | MenuItem | ModuleItem;

// The default bar, which holds the dock's own parts.
export const DEFAULT_NAVBAR: readonly NavbarItem[] = PARTS.map((id) => ({ kind: "part", id }));

// Whether the bar holds one of the dock's own parts, as the views menu, whose
// line of tabs takes room above the dock.
export function holdsPart(items: readonly NavbarItem[], id: PartId): boolean {
  return items.some((item) => item.kind === "part" && item.id === id);
}

// The navbar items that the frontend's `args` give.
export function readNavbar(args: Readonly<Record<string, Json>>): readonly NavbarItem[] {
  const sent = args.navbar;
  if (sent === undefined) return DEFAULT_NAVBAR;
  if (!Array.isArray(sent)) {
    throw new TypeError(`The navbar is ${JSON.stringify(sent)}, where the dock takes a list of navbar items.`);
  }
  return readList(sent, readItem, "on the bar");
}

// The items of a list, the bar's or a menu's, each read by `read`, whose ids
// differ.
function readList<T extends { readonly id: string }>(list: readonly Json[], read: (sent: Json) => T, where: string): T[] {
  const items = list.map(read);
  const ids = new Set<string>();
  for (const item of items) {
    if (ids.has(item.id)) throw new TypeError(`More than one navbar item ${where} has the id ${JSON.stringify(item.id)}.`);
    ids.add(item.id);
  }
  return items;
}

function readItem(sent: Json): NavbarItem {
  const { kind, id } = common(sent);
  const field = (name: string) => text(sent, id, name);
  switch (kind) {
    case "part":
      if (!isPart(id)) {
        throw new TypeError(`The dock has no part ${JSON.stringify(id)} to draw on the navbar. Its parts are ${PARTS.join(", ")}.`);
      }
      return { kind, id };
    case "storage":
      return { kind, id };
    case "module":
      return { kind, id, module: field("module") };
    case "link":
    case "menu":
      return readEntry(sent);
    default:
      throw new TypeError(`The navbar item ${JSON.stringify(id)} is of the kind ${JSON.stringify(kind)}, which the dock does not draw.`);
  }
}

// A link or a menu, which the bar and a menu hold.
function readEntry(sent: Json): LinkItem | MenuItem {
  const { kind, id } = common(sent);
  const field = (name: string) => text(sent, id, name);
  if (kind === "link") return { kind, id, label: field("label"), href: field("href") };
  if (kind !== "menu") {
    throw new TypeError(`The navbar menu holds the item ${JSON.stringify(id)}, which is neither a link nor a menu.`);
  }
  const items = (sent as Readonly<Record<string, Json>>).items;
  if (!Array.isArray(items)) {
    throw new TypeError(`The items of the navbar menu ${JSON.stringify(id)} are ${JSON.stringify(items)}, where the dock takes a list.`);
  }
  return { kind, id, label: field("label"), items: readList(items, readEntry, `in the menu ${JSON.stringify(id)}`) };
}

// The kind and the id of an item, each a string, the id a non-empty one.
function common(sent: Json): { readonly kind: string; readonly id: string } {
  if (!isObject(sent) || typeof sent.kind !== "string" || typeof sent.id !== "string" || sent.id === "") {
    throw new TypeError(`The navbar item ${JSON.stringify(sent)} has no kind and id that the dock reads.`);
  }
  return { kind: sent.kind, id: sent.id };
}

// The field of an item, a non-empty string.
function text(sent: Json, id: string, name: string): string {
  const value = isObject(sent) ? sent[name] : undefined;
  if (typeof value !== "string" || value === "") {
    throw new TypeError(`The ${name} of the navbar item ${JSON.stringify(id)} is ${JSON.stringify(value)}, where the dock takes a non-empty string.`);
  }
  return value;
}

function isPart(id: string): id is PartId {
  return (PARTS as readonly string[]).includes(id);
}
