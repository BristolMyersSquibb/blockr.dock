// The storage controls that the bar draws, as the storage of the app's
// storage plugin gives them
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.5#the-storage-plugins-controls).
// The dock reads them afresh as it draws the bar and as the user presses a
// key, so that it draws and runs what controls() gives at that moment:
// 1. It calls controls() with its CoreApi and the id of the storage plugin's
//    class.
// 2. Of the controls of one list that share an id, it keeps the first, since
//    it keys each control by its id, and logs the others.
// 3. It leaves out a control of a kind that it does not know.
// Where controls() throws, no control draws: the bar shows a warning in the
// place of the `storage` item, which names the plugin's package, and the dock
// logs why.
// The dock reads none of the hints that a control may carry, such as
// `placement`: an app script places a control on the bar through its navbar
// items alone.
// A storage plugin tells the user why one of its buttons failed, since it
// alone knows its backend, and core tells the user why it refused what the
// button asked of it, such as a board to open. So the dock logs a run() that
// rejects, and tells the user nothing more.

import type { CoreApi, StorageControl } from "@blockr/core";
import { asError, errorMessage } from "./errors.ts";
import type { NavbarItem } from "./navbar-items.ts";

export type Button = Extract<StorageControl, { kind: "button" }>;

// The storage controls as the bar draws them.
export type Drawn =
  // The controls, in the plugin's order, and which ids two controls of one
  // list share, if any.
  | { readonly controls: readonly StorageControl[]; readonly failed?: undefined; readonly problem?: string }
  // Why controls() failed, where it threw, with the package of the storage
  // plugin's class.
  | { readonly failed: string; readonly package: string; readonly problem: string };

const KINDS: ReadonlySet<string> = new Set(["button", "menu", "status"]);

// The storage controls of the app's storage plugin, or undefined where the app
// runs no storage plugin.
// What the dock finds wrong as it reads them comes back in `problem`, which
// the bar logs once for each problem, rather than as each drawing or key press
// finds it again.
export function drawnControls(core: CoreApi): Drawn | undefined {
  const storage = core.plugins.storage();
  if (storage === undefined) return undefined;
  // The app's storage plugin, whose class gives that storage.
  const plugin = Object.keys(core.plugins.appPlugins()).find((id) => core.plugins.pluginClass(id).storage === storage);
  if (plugin === undefined) return undefined;
  const twice = new Set<string>();
  let controls: StorageControl[];
  try {
    controls = distinct(storage.controls({ core, plugin }), twice);
  } catch (error) {
    const failed = errorMessage(error);
    return { failed, package: packageOf(plugin), problem: `The storage controls of ${plugin} failed: ${failed}` };
  }
  if (twice.size === 0) return { controls };
  const ids = [...twice].map((id) => JSON.stringify(id)).join(", ");
  return { controls, problem: `Storage controls of one list share the ids ${ids}, so only the first of each draws.` };
}

// The buttons among the controls, those inside menus too, in the order in which
// the bar draws them.
export function buttonsOf(controls: readonly StorageControl[]): Button[] {
  return controls.flatMap((control) => {
    if (control.kind === "button") return [control];
    if (control.kind === "menu") return buttonsOf(control.items);
    return [];
  });
}

// The buttons among the storage controls that the bar draws, those inside
// menus too, in the storage plugin's order: every control where the bar holds
// the `storage` item, and otherwise those that storage items place.
export function barButtons(core: CoreApi, items: readonly NavbarItem[]): Button[] {
  const drawn = drawnControls(core);
  if (drawn === undefined || drawn.failed !== undefined) return [];
  const all = items.some((item) => item.kind === "part" && item.id === "storage");
  const apart = new Set(items.flatMap((item) => (item.kind === "storage" ? [item.id] : [])));
  return buttonsOf(drawn.controls.filter((control) => all || apart.has(control.id)));
}

// Runs the button. It calls run() before it awaits anything, so that run()
// still acts within the user's click or key press, as a browser's file picker
// needs, and logs a run() that rejects through the dock's log.
export async function runButton(core: Pick<CoreApi, "log">, button: Button): Promise<void> {
  try {
    await button.run();
  } catch (error) {
    core.log("blockr.dock").error(asError(error));
  }
}

// The package of a class or a module, by its id of the form `{pkg}-{name}`,
// as "blockr.core" for "blockr.core-file_storage_plugin".
// A package's name holds no "-", so the package is all that comes before the
// first.
export function packageOf(id: string): string {
  const at = id.indexOf("-");
  return at < 0 ? id : id.slice(0, at);
}

// The controls of a list that the bar draws: the first of those that share an
// id, of the kinds that it knows, and each menu with the same of its items.
// It adds to `twice` each id that it finds again.
function distinct(list: readonly StorageControl[], twice: Set<string>): StorageControl[] {
  const seen = new Set<string>();
  const kept: StorageControl[] = [];
  for (const control of list) {
    if (!KINDS.has(control.kind)) continue;
    if (seen.has(control.id)) {
      twice.add(control.id);
      continue;
    }
    seen.add(control.id);
    kept.push(control.kind === "menu" ? { ...control, items: distinct(control.items, twice) } : control);
  }
  return kept;
}
