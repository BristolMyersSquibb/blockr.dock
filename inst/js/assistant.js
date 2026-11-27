// srcts/state/json.ts
function isList(value) {
  return Array.isArray(value);
}
function isObject(value) {
  return value !== null && typeof value === "object" && !Array.isArray(value);
}

// srcts/state/ids.ts
function isIdRecord(value) {
  return isObject(value) && typeof value.package === "string" && typeof value.id === "string";
}
function sameId(a, b) {
  return a.package === b.package && a.id === b.id;
}

// srcts/state/state.ts
function isDockState(value) {
  return isObject(value) && isRecordOf(value.views, isView) && isRecordOf(value.folders, isFolder) && (value.active === null || typeof value.active === "string") && isRecordOf(value.visible, (sections) => isListOf(sections, (s) => s === "inputs" || s === "outputs"));
}
function isView(value) {
  return isFolder(value) && isGrid(value.grid);
}
function isFolder(value) {
  return isObject(value) && typeof value.name === "string" && (value.folder === null || typeof value.folder === "string") && typeof value.index === "string";
}
function isGrid(value) {
  if (!isObject(value)) return false;
  const { orientation, rails: rails2 } = value;
  if (orientation !== "horizontal" && orientation !== "vertical") return false;
  const railsValid = rails2 === void 0 || isObject(rails2) && (rails2.left === void 0 || isRail(rails2.left)) && (rails2.right === void 0 || isRail(rails2.right));
  return railsValid && isSplit(value);
}
function isNode(value) {
  return isSplit(value) || isTabGroup(value);
}
function isSplit(value) {
  return isObject(value) && isListOf(value.children, isNode) && isListOf(value.sizes, (size) => typeof size === "number");
}
function isTabGroup(value) {
  return isObject(value) && isListOf(value.panels, isPanel) && isPanel(value.active);
}
function isRail(value) {
  if (!isObject(value)) return false;
  const { collapsed, size } = value;
  return typeof collapsed === "boolean" && typeof size === "number" && isTabGroup(value);
}
function isPanel(value) {
  if (!isObject(value)) return false;
  return typeof value.block === "string" !== isIdRecord(value.plugin);
}
function isRecordOf(value, item) {
  return isObject(value) && Object.values(value).every(item);
}
function isListOf(value, item) {
  return isList(value) && value.every(item);
}

// srcts/state/grid.ts
var EDGES = ["left", "right"];
function isTabGroup2(node) {
  return "panels" in node;
}
function gridPanels(grid) {
  return [...tabGroups(grid).flatMap((group) => group.panels), ...rails(grid).flatMap(([, rail]) => rail.panels)];
}
function tabGroups(grid) {
  const groups = [];
  const walk = (node) => {
    if (isTabGroup2(node)) groups.push(node);
    else node.children.forEach(walk);
  };
  grid.children.forEach(walk);
  return groups;
}
function rails(grid) {
  return EDGES.flatMap((edge) => {
    const rail = grid.rails?.[edge];
    return rail === void 0 ? [] : [[edge, rail]];
  });
}

// srcts/state/navigation.ts
import { generateKeyBetween, generateNKeysBetween } from "./vendor-a3ad4872.js";
function inFolder(state, folder) {
  const views = Object.entries(state.views).filter(([, view]) => holder(state, view.folder) === folder).map(([id, view]) => ({ entry: { view: id }, id, index: view.index }));
  const folders = Object.entries(state.folders).filter(([id, inner]) => holder(state, inner.folder, id) === folder).map(([id, inner]) => ({ entry: { folder: id }, id, index: inner.index }));
  return [...views, ...folders].sort((a, b) => compare(a.index, b.index) || compare(a.id, b.id)).map(({ entry }) => entry);
}
function viewOrder(state) {
  const order = [];
  const walked = /* @__PURE__ */ new Set();
  const walk = (folder) => {
    for (const entry of inFolder(state, folder)) {
      if ("view" in entry) order.push(entry.view);
      else if (!walked.has(entry.folder)) {
        walked.add(entry.folder);
        walk(entry.folder);
      }
    }
  };
  walk(null);
  return order;
}
function within(state, folder, outer) {
  const passed = /* @__PURE__ */ new Set();
  let at = folder;
  while (at !== null && !passed.has(at)) {
    if (at === outer) return true;
    passed.add(at);
    at = state.folders[at]?.folder ?? null;
  }
  return false;
}
function compare(a, b) {
  return a < b ? -1 : a > b ? 1 : 0;
}
function holder(state, folder, self) {
  if (folder === null || !Object.hasOwn(state.folders, folder)) return null;
  if (self !== void 0 && insideItself(state, self)) return null;
  return folder;
}
function insideItself(state, id) {
  const parent = state.folders[id]?.folder ?? null;
  return parent !== null && within(state, parent, id);
}

// srcts/modules/assistant/index.ts
var DOCK = { package: "blockr.dock", id: "dock_plugin" };
function describe(plugin, state) {
  if (!sameId(plugin, DOCK) || state === null) return [];
  if (!isDockState(state)) throw new Error("The dock's state does not have the shape that its schema gives.");
  const views = orderedViews(state);
  const active = views.find(([id]) => id === state.active);
  return [
    ...active === void 0 ? [] : [`The active view is ${active[1].name} (id: ${active[0]}).`],
    ...views.map(([id, view]) => `- ${view.name} (id: ${id}): ${placed(view)}`)
  ];
}
function orderedViews(state) {
  return viewOrder(state).flatMap((id) => {
    const view = state.views[id];
    return view === void 0 ? [] : [[id, view]];
  });
}
function placed(view) {
  const panels = gridPanels(view.grid).map(
    (panel) => "block" in panel ? panel.block : JSON.stringify({ package: panel.plugin.package, id: panel.plugin.id })
  );
  return panels.length === 0 ? "empty" : panels.join(", ");
}
var assistant_default = { describe };
export {
  assistant_default as default
};
