// How the dock makes an edit: it runs one of the commands that the command
// registry holds, as one step of the user, so that undo takes back each one.
// Where the store or the command registry refuses an edit, the dock tells the
// user why through a notification, an error of the code edit_refused, which
// concerns the dock plugin, whatever block the edit names, as core's plugins
// raise one for a refused edit
// (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.1.14#notifications).

import type { CoreApi, Json, PluginClassId } from "@blockr/core";

// Tells the user why the store or the command registry refused an edit of the
// dock plugin, whose class is `plugin`.
export function refused(core: Pick<CoreApi, "notifications">, plugin: PluginClassId, error: unknown): void {
  core.notifications.notify(plugin, {
    severity: "error",
    code: "edit_refused",
    message: error instanceof Error ? error.message : String(error),
  });
}

// Runs the command with the args as an edit of the dock plugin, and resolves
// whether it ran.
export function runEdit(
  core: Pick<CoreApi, "commands" | "notifications">,
  plugin: PluginClassId,
  id: string,
  args: Json,
): Promise<boolean> {
  return core.commands.run(id, args).then(
    () => true,
    (error: unknown) => {
      refused(core, plugin, error);
      return false;
    },
  );
}
