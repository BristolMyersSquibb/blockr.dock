// srcts/startup/index.ts
var api = 1;
var dockPlugin = {
  id: "blockr.dock-dock_plugin",
  frontend: "blockr.dock-dock"
};
var startup_default = {
  initialize({ core }) {
    core.plugins.register(dockPlugin);
  }
};
export {
  api,
  startup_default as default
};
