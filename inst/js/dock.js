// srcts/modules/dock/index.tsx
import { flushSync } from "react-dom";
import { createRoot } from "react-dom/client";

// inst/js/dock.schema.json
var dock_schema_default = {
  $schema: "http://json-schema.org/draft-07/schema#",
  type: "object",
  properties: {
    views: {
      type: "object",
      additionalProperties: {
        $ref: "#/definitions/View"
      },
      description: "Every view, by its id."
    },
    folders: {
      type: "object",
      additionalProperties: {
        $ref: "#/definitions/Folder"
      },
      description: "Every folder, by its id."
    },
    active: {
      anyOf: [
        {
          $ref: "#/definitions/ViewId"
        },
        {
          type: "null"
        }
      ],
      description: "The view that the board opens on, or null while the state holds no view."
    },
    visible: {
      type: "object",
      additionalProperties: {
        type: "array",
        items: {
          $ref: "#/definitions/Section"
        }
      },
      description: "The sections that a block's card shows, by the block's id, for each card that hides a section. A card without an entry shows both."
    }
  },
  required: [
    "views",
    "folders",
    "active",
    "visible"
  ],
  additionalProperties: false,
  description: "The dock's views, the folders that group them, the active view and the sections that each block's card shows.",
  definitions: {
    View: {
      type: "object",
      properties: {
        name: {
          type: "string",
          description: "The name that the user gives the view."
        },
        folder: {
          anyOf: [
            {
              $ref: "#/definitions/FolderId"
            },
            {
              type: "null"
            }
          ],
          description: "The folder that the view sits in, or null at the top."
        },
        index: {
          type: "string",
          description: "The key that orders the view among the views and folders of its folder, compared as a string."
        },
        grid: {
          $ref: "#/definitions/Grid",
          description: "Which panels the view shows, and where."
        }
      },
      required: [
        "name",
        "folder",
        "index",
        "grid"
      ],
      additionalProperties: false,
      description: "A view: a grid of panels, with a name, in a place in the navigation."
    },
    FolderId: {
      type: "string",
      description: "The id of a folder, which never changes."
    },
    Grid: {
      type: "object",
      properties: {
        orientation: {
          type: "string",
          enum: [
            "horizontal",
            "vertical"
          ],
          description: "How the top split divides. Each split inside divides the other way from the split that holds it."
        },
        children: {
          type: "array",
          items: {
            $ref: "#/definitions/Node"
          },
          description: "What the top split holds, in order."
        },
        sizes: {
          type: "array",
          items: {
            type: "number",
            description: "The share of a split that one of its children takes, a ratio above 0.",
            exclusiveMinimum: 0
          },
          description: "The share of each child, ratios that sum to 1."
        },
        rails: {
          type: "object",
          properties: {
            left: {
              $ref: "#/definitions/Rail"
            },
            right: {
              $ref: "#/definitions/Rail"
            }
          },
          additionalProperties: false,
          description: "The rails of the view, by the edge that each sits on."
        }
      },
      required: [
        "orientation",
        "children",
        "sizes"
      ],
      additionalProperties: false,
      description: "Where a view places its panels: nested splits of tab groups, and a rail on each side."
    },
    Node: {
      anyOf: [
        {
          $ref: "#/definitions/Split"
        },
        {
          $ref: "#/definitions/TabGroup"
        }
      ],
      description: "What a split holds: a split or a tab group."
    },
    Split: {
      type: "object",
      properties: {
        children: {
          type: "array",
          items: {
            $ref: "#/definitions/Node"
          },
          description: "What the split holds, in order."
        },
        sizes: {
          type: "array",
          items: {
            type: "number",
            description: "The share of a split that one of its children takes, a ratio above 0.",
            exclusiveMinimum: 0
          },
          description: "The share of each child, ratios that sum to 1."
        }
      },
      required: [
        "children",
        "sizes"
      ],
      additionalProperties: false,
      description: "A split, which divides the other way from the split that holds it."
    },
    TabGroup: {
      type: "object",
      properties: {
        panels: {
          type: "array",
          items: {
            $ref: "#/definitions/Panel"
          },
          description: "Its panels, in the order of their tabs."
        },
        active: {
          $ref: "#/definitions/Panel",
          description: "The panel that it shows."
        }
      },
      required: [
        "panels",
        "active"
      ],
      additionalProperties: false,
      description: "A tab group, with a tab for each of its panels."
    },
    Panel: {
      anyOf: [
        {
          type: "object",
          properties: {
            block: {
              $ref: "#/definitions/BlockId"
            }
          },
          required: [
            "block"
          ],
          additionalProperties: false
        },
        {
          type: "object",
          properties: {
            plugin: {
              $ref: "#/definitions/PluginClassId"
            }
          },
          required: [
            "plugin"
          ],
          additionalProperties: false
        }
      ],
      description: "A panel: the card of one of the board's blocks, by the block's id, or the UI of one of the app's plugins, by the id of the plugin's class."
    },
    BlockId: {
      type: "string"
    },
    PluginClassId: {
      type: "string"
    },
    Rail: {
      type: "object",
      properties: {
        panels: {
          type: "array",
          items: {
            $ref: "#/definitions/Panel"
          },
          description: "Its panels, in the order of their tabs."
        },
        active: {
          $ref: "#/definitions/Panel",
          description: "The panel that it shows."
        },
        collapsed: {
          type: "boolean",
          description: "Whether the rail shows its tabs alone."
        },
        size: {
          type: "number",
          description: "Its width in pixels while it is open, 500 by default.",
          minimum: 0
        }
      },
      required: [
        "panels",
        "active",
        "collapsed",
        "size"
      ],
      additionalProperties: false,
      description: "A rail on an edge of the view, with a tab for each of its panels."
    },
    Folder: {
      type: "object",
      properties: {
        name: {
          type: "string",
          description: "The name that the user gives the folder."
        },
        folder: {
          anyOf: [
            {
              $ref: "#/definitions/FolderId"
            },
            {
              type: "null"
            }
          ],
          description: "The folder that the folder sits in, or null at the top."
        },
        index: {
          type: "string",
          description: "The key that orders the folder among the views and folders of its folder, compared as a string."
        }
      },
      required: [
        "name",
        "folder",
        "index"
      ],
      additionalProperties: false,
      description: "A folder, which groups views and other folders in the navigation."
    },
    ViewId: {
      type: "string",
      description: "The id of a view, which never changes."
    },
    Section: {
      type: "string",
      enum: [
        "inputs",
        "outputs"
      ],
      description: "A section of a block's card."
    }
  }
};

// srcts/state/json.ts
function isList(value) {
  return Array.isArray(value);
}
function isObject(value) {
  return value !== null && typeof value === "object" && !Array.isArray(value);
}
function sameJson(a, b) {
  if (a === b) return true;
  if (isList(a) || isList(b)) {
    return isList(a) && isList(b) && a.length === b.length && a.every((value, i) => i in b && sameJson(value, b[i] ?? null));
  }
  if (!isObject(a) || !isObject(b)) return false;
  const names = Object.keys(a);
  return names.length === Object.keys(b).length && names.every((name2) => Object.hasOwn(b, name2) && sameJson(a[name2] ?? null, b[name2] ?? null));
}

// srcts/state/state.ts
var defaultState = { views: {}, folders: {}, active: null, visible: {} };
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
  return typeof value.block === "string" !== (typeof value.plugin === "string");
}
function isRecordOf(value, item) {
  return isObject(value) && Object.values(value).every(item);
}
function isListOf(value, item) {
  return isList(value) && value.every(item);
}

// srcts/state/grid.ts
var EDGES = ["left", "right"];
var SIDES = ["within", "left", "right", "above", "below"];
var RAIL_SIZE = 500;
var TOLERANCE = 15e-9;
function isTabGroup2(node) {
  return "panels" in node;
}
function samePanel(a, b) {
  return "block" in a ? "block" in b && a.block === b.block : "plugin" in b && a.plugin === b.plugin;
}
function gridPanels(grid2) {
  return [...tabGroups(grid2).flatMap((group) => group.panels), ...rails(grid2).flatMap(([, rail2]) => rail2.panels)];
}
function places(grid2, panel2) {
  return gridPanels(grid2).some((placed) => samePanel(placed, panel2));
}
function tabGroups(grid2) {
  const groups = [];
  const walk = (node) => {
    if (isTabGroup2(node)) groups.push(node);
    else node.children.forEach(walk);
  };
  grid2.children.forEach(walk);
  return groups;
}
function rails(grid2) {
  return EDGES.flatMap((edge2) => {
    const rail2 = grid2.rails?.[edge2];
    return rail2 === void 0 ? [] : [[edge2, rail2]];
  });
}
function prunePanels(grid2, keep2) {
  const top = prunedSplit(grid2, keep2) ?? { children: [], sizes: [] };
  const kept = rails(grid2).flatMap(([edge2, rail2]) => {
    const pruned = prunedGroup(rail2, keep2);
    return pruned === void 0 ? [] : [[edge2, { ...rail2, ...pruned }]];
  });
  return {
    orientation: grid2.orientation,
    ...top,
    ...kept.length === 0 ? {} : { rails: Object.fromEntries(kept) }
  };
}
function prunedNode(node, keep2) {
  return isTabGroup2(node) ? prunedGroup(node, keep2) : prunedSplit(node, keep2);
}
function prunedSplit(split, keep2) {
  const shares2 = fitted(split.sizes, split.children.length);
  const pruned = split.children.map((child) => prunedNode(child, keep2));
  const kept = pruned.flatMap((node, i) => node === void 0 ? [] : [{ node, share: shares2[i] ?? 0 }]);
  if (kept.length === 0) return void 0;
  const children2 = kept.map(({ node }) => node);
  const emptied = split.children.some((child, i) => pruned[i] === void 0 && holdsPanel(child));
  return { children: children2, sizes: emptied ? evenShares(children2.length) : normalized(kept.map(({ share }) => share)) };
}
function holdsPanel(node) {
  return isTabGroup2(node) ? node.panels.length > 0 : node.children.some(holdsPanel);
}
function prunedGroup(group, keep2) {
  const panels = group.panels.filter(keep2);
  const [first] = panels;
  if (first === void 0) return void 0;
  const active = panels.find((panel2) => samePanel(panel2, group.active)) ?? first;
  return { panels, active };
}
function fitted(sizes, n) {
  const fits2 = sizes.length === n && sizes.every((size) => Number.isFinite(size) && size > 0);
  return fits2 ? normalized(sizes) : evenShares(n);
}
function evenShares(n) {
  return Array.from({ length: n }, () => 1 / n);
}
function normalized(shares2) {
  const total = sum(shares2);
  return Math.abs(total - 1) <= TOLERANCE ? [...shares2] : shares2.map((share) => share / total);
}
function sum(values) {
  return values.reduce((total, value) => total + value, 0);
}
function gridFailures(grid2) {
  const failures = [];
  const split = (node, at2, top) => {
    if (!top && node.children.length === 0) failures.push(`The split at "${at2}" holds nothing.`);
    if (node.sizes.length !== node.children.length) {
      failures.push(`The split at "${at2 || "/"}" has ${count(node.children.length, "child", "children")} and ${count(node.sizes.length, "share", "shares")}.`);
    } else if (!node.sizes.every((size) => Number.isFinite(size) && size > 0)) {
      failures.push(`The split at "${at2 || "/"}" gives a child a share that is not above 0.`);
    } else if (node.children.length > 0 && Math.abs(sum(node.sizes) - 1) > TOLERANCE) {
      failures.push(`The shares of the split at "${at2 || "/"}" do not sum to 1.`);
    }
    node.children.forEach((child, i) => {
      if (isTabGroup2(child)) group(child, `${at2}/children/${String(i)}`, "tab group");
      else split(child, `${at2}/children/${String(i)}`, false);
    });
  };
  const group = (node, at2, what) => {
    if (node.panels.length === 0) failures.push(`The ${what} at "${at2}" holds no panel.`);
    else if (!node.panels.some((panel2) => samePanel(panel2, node.active))) {
      failures.push(`The ${what} at "${at2}" shows a panel that it does not hold.`);
    }
  };
  split(grid2, "", true);
  for (const [edge2, rail2] of rails(grid2)) group(rail2, `/rails/${edge2}`, "rail");
  const seen = [];
  const twice = [];
  for (const panel2 of gridPanels(grid2)) {
    if (seen.some((other) => samePanel(other, panel2))) {
      if (!twice.some((other) => samePanel(other, panel2))) twice.push(panel2);
    } else seen.push(panel2);
  }
  for (const panel2 of twice) failures.push(`The grid places ${panelName(panel2)} more than once.`);
  return failures;
}
function count(n, one, many) {
  return `${String(n)} ${n === 1 ? one : many}`;
}
function panelName(panel2) {
  return "block" in panel2 ? `the panel of the block "${panel2.block}"` : `the panel of the plugin "${panel2.plugin}"`;
}
function placePanel(grid2, panel2) {
  if (places(grid2, panel2)) return grid2;
  const target = tabGroups(grid2).reverse().find((group) => group.panels.every((placed) => "block" in placed));
  if (target !== void 0) {
    const add = (node) => {
      if (node === target) return { panels: [...target.panels, panel2], active: panel2 };
      return isTabGroup2(node) ? node : { ...node, children: node.children.map(add) };
    };
    return { ...grid2, children: grid2.children.map(add) };
  }
  return appendTabGroup(grid2, panel2);
}
function appendTabGroup(grid2, panel2) {
  return { ...grid2, ...evened(grid2, grid2.children.length, tabGroup(panel2)) };
}
function evened(split, at2, node) {
  const children2 = [...split.children.slice(0, at2), node, ...split.children.slice(at2)];
  return { children: children2, sizes: evenShares(children2.length) };
}
function halved(split, beside, before2, node) {
  const shares2 = fitted(split.sizes, split.children.length);
  const half = (shares2[beside] ?? 0) / 2;
  const sizes = shares2.map((share, i) => i === beside ? half : share);
  const at2 = before2 ? beside : beside + 1;
  return {
    children: [...split.children.slice(0, at2), node, ...split.children.slice(at2)],
    sizes: [...sizes.slice(0, at2), half, ...sizes.slice(at2)]
  };
}
function tabGroup(panel2) {
  return { panels: [panel2], active: panel2 };
}
function locate(grid2, panel2) {
  const walk = (node, path) => {
    if (isTabGroup2(node)) return node.panels.some((placed) => samePanel(placed, panel2)) ? path : void 0;
    for (const [i, child] of node.children.entries()) {
      const found = walk(child, [...path, i]);
      if (found !== void 0) return found;
    }
    return void 0;
  };
  for (const [i, child] of grid2.children.entries()) {
    const path = walk(child, [i]);
    if (path !== void 0) return { path };
  }
  const held = rails(grid2).find(([, rail2]) => rail2.panels.some((placed) => samePanel(placed, panel2)));
  return held === void 0 ? void 0 : { edge: held[0] };
}
function placeFailures(grid2, place) {
  if (place.rail !== void 0 || place.near === void 0) return [];
  const at2 = locate(grid2, place.near);
  if (at2 === void 0) return [`The view does not show ${panelName(place.near)}, which near names.`];
  if ("edge" in at2 && (place.side ?? "within") !== "within") {
    return [`The view shows ${panelName(place.near)}, which near names, on a rail, which takes a panel only within.`];
  }
  return [];
}
function placedAt(grid2, panel2, place, sizing) {
  if (place.rail !== void 0) return onRail(grid2, place.rail, panel2);
  const side2 = place.side ?? "within";
  if (place.near === void 0) return side2 === "within" ? placePanel(grid2, panel2) : atEdge(grid2, panel2, side2);
  const at2 = locate(grid2, place.near);
  if (at2 === void 0) return grid2;
  if ("edge" in at2) return onRail(grid2, at2.edge, panel2);
  if (side2 === "within") {
    return withNode(grid2, at2.path, (node) => isTabGroup2(node) ? { panels: [...node.panels, panel2], active: panel2 } : node);
  }
  return besideGroup(grid2, at2.path, panel2, side2, sizing);
}
function onRail(grid2, edge2, panel2) {
  const rail2 = grid2.rails?.[edge2];
  const next = rail2 === void 0 ? { ...tabGroup(panel2), collapsed: false, size: RAIL_SIZE } : { ...rail2, panels: [...rail2.panels, panel2], active: panel2 };
  return { ...grid2, rails: { ...grid2.rails, [edge2]: next } };
}
function sideOrientation(side2) {
  return side2 === "left" || side2 === "right" ? "horizontal" : "vertical";
}
function leads(side2) {
  return side2 === "left" || side2 === "above";
}
function besideGroup(grid2, path, panel2, side2, sizing) {
  const holder2 = path.slice(0, -1);
  const at2 = path.at(-1) ?? 0;
  const divides = holder2.length % 2 === 0 ? grid2.orientation : flipped(grid2.orientation);
  if (divides === sideOrientation(side2)) {
    return withSplit(
      grid2,
      holder2,
      (split) => sizing === "split" ? halved(split, at2, leads(side2), tabGroup(panel2)) : evened(split, leads(side2) ? at2 : at2 + 1, tabGroup(panel2))
    );
  }
  return withNode(grid2, path, (node) => ({
    children: leads(side2) ? [tabGroup(panel2), node] : [node, tabGroup(panel2)],
    sizes: [0.5, 0.5]
  }));
}
function atEdge(grid2, panel2, side2) {
  const orientation = sideOrientation(side2);
  if (grid2.children.length === 0) return { ...grid2, orientation, children: [tabGroup(panel2)], sizes: [1] };
  if (grid2.orientation === orientation) {
    return { ...grid2, ...evened(grid2, leads(side2) ? 0 : grid2.children.length, tabGroup(panel2)) };
  }
  const [only] = grid2.children;
  const top = grid2.children.length === 1 && only !== void 0 && isTabGroup2(only) ? only : { children: grid2.children, sizes: grid2.sizes };
  return {
    ...grid2,
    orientation,
    children: leads(side2) ? [tabGroup(panel2), top] : [top, tabGroup(panel2)],
    sizes: [0.5, 0.5]
  };
}
function flipped(orientation) {
  return orientation === "horizontal" ? "vertical" : "horizontal";
}
function withNode(grid2, path, fn) {
  const walk = (split, rest) => {
    const [i, ...deeper] = rest;
    return {
      ...split,
      children: split.children.map((child, j) => {
        if (j !== i) return child;
        if (deeper.length === 0) return fn(child);
        return isTabGroup2(child) ? child : walk(child, deeper);
      })
    };
  };
  return { ...grid2, ...walk(grid2, path) };
}
function withSplit(grid2, path, fn) {
  if (path.length === 0) return { ...grid2, ...fn({ children: grid2.children, sizes: grid2.sizes }) };
  return withNode(grid2, path, (node) => isTabGroup2(node) ? node : fn(node));
}
function splitAt(split, path) {
  const [i, ...deeper] = path;
  if (i === void 0) return split;
  const child = split.children[i];
  return child === void 0 || isTabGroup2(child) ? void 0 : splitAt(child, deeper);
}
function showing(grid2, panel2) {
  const at2 = locate(grid2, panel2);
  if (at2 === void 0) return grid2;
  if ("path" in at2) return withNode(grid2, at2.path, (node) => isTabGroup2(node) ? { ...node, active: panel2 } : node);
  const rail2 = grid2.rails?.[at2.edge];
  return rail2 === void 0 ? grid2 : { ...grid2, rails: { ...grid2.rails, [at2.edge]: { ...rail2, active: panel2 } } };
}
function collapsedRail(grid2, edge2, collapsed) {
  const rail2 = grid2.rails?.[edge2];
  return rail2 === void 0 ? grid2 : { ...grid2, rails: { ...grid2.rails, [edge2]: { ...rail2, collapsed } } };
}
function resizeFailures(grid2, panel2, size) {
  const at2 = locate(grid2, panel2);
  if (at2 === void 0 || "edge" in at2) return [];
  const failures = [];
  if (size >= 1) failures.push(`A tab group takes a share of its split below 1, which ${String(size)} is not.`);
  const split = splitAt(grid2, at2.path.slice(0, -1));
  if (split !== void 0 && split.children.length < 2) {
    failures.push(`The tab group that holds ${panelName(panel2)} fills its split alone, so its share stays 1.`);
  }
  return failures;
}
function resized(grid2, panel2, size) {
  const at2 = locate(grid2, panel2);
  if (at2 === void 0) return grid2;
  if ("edge" in at2) {
    const rail2 = grid2.rails?.[at2.edge];
    return rail2 === void 0 ? grid2 : { ...grid2, rails: { ...grid2.rails, [at2.edge]: { ...rail2, size } } };
  }
  const i = at2.path.at(-1) ?? 0;
  return withSplit(grid2, at2.path.slice(0, -1), (split) => {
    const shares2 = fitted(split.sizes, split.children.length);
    const rest = sum(shares2) - (shares2[i] ?? 0);
    return { ...split, sizes: shares2.map((share, j) => j === i ? size : share / rest * (1 - size)) };
  });
}
function withStoredShares(stored, grid2, tolerance) {
  if (!sameShape(stored, grid2)) return grid2;
  const kept = (before2, after2) => {
    const children2 = after2.children.map((child, i) => {
      const was = before2.children[i];
      return isTabGroup2(child) || was === void 0 || isTabGroup2(was) ? child : kept(was, child);
    });
    const shares2 = fitted(before2.sizes, before2.children.length);
    const steady = shares2.every((share, i) => Math.abs(share - (after2.sizes[i] ?? Number.NaN)) <= tolerance);
    return { children: children2, sizes: steady ? shares2 : after2.sizes };
  };
  return { ...grid2, ...kept(stored, grid2) };
}
function gridChanges(stored, grid2) {
  const before2 = gridPanels(stored);
  const after2 = gridPanels(grid2);
  return {
    panels: before2.length !== after2.length || !after2.every((panel2) => before2.some((other) => samePanel(other, panel2))),
    arrangement: JSON.stringify(arrangement(grid2)) !== JSON.stringify(arrangement(stored)),
    sizes: resizedFrom(stored, grid2)
  };
}
function arrangement(grid2) {
  const node = (child) => isTabGroup2(child) ? child.panels : child.children.map(node);
  const edges = rails(grid2).map(([edge2, rail2]) => [edge2, rail2.panels]);
  return { orientation: grid2.orientation, children: grid2.children.map(node), rails: Object.fromEntries(edges) };
}
function shape(grid2) {
  const node = (child) => isTabGroup2(child) ? "tab group" : child.children.map(node);
  return { orientation: grid2.orientation, children: grid2.children.map(node) };
}
function resizedFrom(stored, grid2) {
  const widths = EDGES.some((edge2) => {
    const [a, b] = [stored.rails?.[edge2], grid2.rails?.[edge2]];
    return a !== void 0 && b !== void 0 && a.size !== b.size;
  });
  if (widths) return true;
  if (!sameShape(stored, grid2)) return false;
  const given = shares(grid2);
  return shares(stored).some((share, i) => share !== given[i]);
}
function sameShape(a, b) {
  return JSON.stringify(shape(a)) === JSON.stringify(shape(b));
}
function shares(grid2) {
  const walk = (split) => [
    ...fitted(split.sizes, split.children.length),
    ...split.children.flatMap((child) => isTabGroup2(child) ? [] : walk(child))
  ];
  return walk(grid2);
}

// srcts/state/navigation.ts
import { generateKeyBetween, generateNKeysBetween } from "./vendor-a3ad4872.js";
function inFolder(state, folder2) {
  const views = Object.entries(state.views).filter(([, view2]) => holder(state, view2.folder) === folder2).map(([id, view2]) => ({ entry: { view: id }, id, index: view2.index }));
  const folders = Object.entries(state.folders).filter(([id, inner]) => holder(state, inner.folder, id) === folder2).map(([id, inner]) => ({ entry: { folder: id }, id, index: inner.index }));
  return [...views, ...folders].sort((a, b) => compare(a.index, b.index) || compare(a.id, b.id)).map(({ entry }) => entry);
}
function viewOrder(state) {
  const order = [];
  const walked = /* @__PURE__ */ new Set();
  const walk = (folder2) => {
    for (const entry of inFolder(state, folder2)) {
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
function firstView(state) {
  return viewOrder(state)[0] ?? null;
}
function sameEntry(a, b) {
  return "view" in a ? "view" in b && a.view === b.view : "folder" in b && a.folder === b.folder;
}
function mintKey(state, folder2, before2, moving) {
  const others = inFolder(state, folder2).filter((entry) => moving === void 0 || !sameEntry(entry, moving));
  const at2 = before2 === void 0 ? others.length : others.findIndex((entry) => sameEntry(entry, before2));
  const index = (entry) => entry === void 0 ? null : entryIndex(state, entry);
  const [key] = keysBetween(index(others[at2 - 1]), index(others[at2]), 1);
  return key ?? generateKeyBetween(null, null);
}
function sitsAt(state, entry, folder2, before2) {
  const listed = inFolder(state, folder2);
  const at2 = listed.findIndex((other) => sameEntry(other, entry));
  if (at2 === -1) return false;
  const next = listed[at2 + 1];
  return before2 === void 0 ? next === void 0 : next !== void 0 && sameEntry(next, before2);
}
function foldersHolding(state, entry) {
  const folders = [];
  let at2 = "view" in entry ? holder(state, state.views[entry.view]?.folder ?? null) : holder(state, state.folders[entry.folder]?.folder ?? null, entry.folder);
  while (at2 !== null && !folders.includes(at2)) {
    folders.push(at2);
    at2 = holder(state, state.folders[at2]?.folder ?? null, at2);
  }
  return folders;
}
function within(state, folder2, outer) {
  const passed = /* @__PURE__ */ new Set();
  let at2 = folder2;
  while (at2 !== null && !passed.has(at2)) {
    if (at2 === outer) return true;
    passed.add(at2);
    at2 = state.folders[at2]?.folder ?? null;
  }
  return false;
}
function keysBetween(a, b, n) {
  const bounds = [
    [a, b],
    [a, null],
    [null, b]
  ];
  for (const [low, high] of bounds) {
    try {
      return generateNKeysBetween(low, high, n);
    } catch {
    }
  }
  return generateNKeysBetween(null, null, n);
}
function entryIndex(state, entry) {
  return ("view" in entry ? state.views[entry.view]?.index : state.folders[entry.folder]?.index) ?? null;
}
function compare(a, b) {
  return a < b ? -1 : a > b ? 1 : 0;
}
function holder(state, folder2, self) {
  if (folder2 === null || !Object.hasOwn(state.folders, folder2)) return null;
  if (self !== void 0 && insideItself(state, self)) return null;
  return folder2;
}
function insideItself(state, id) {
  const parent = state.folders[id]?.folder ?? null;
  return parent !== null && within(state, parent, id);
}

// srcts/state/patch.ts
function pointer(segments) {
  return segments.map((segment) => `/${segment.replaceAll("~", "~0").replaceAll("/", "~1")}`).join("");
}
function segmentsFromPath(path) {
  if (!path.startsWith("/")) return void 0;
  const segments = path.slice(1).split("/");
  if (segments.some((segment) => /~(?![01])/.test(segment))) return void 0;
  return segments.map((segment) => segment.replaceAll("~1", "/").replaceAll("~0", "~"));
}
function patched(state, patches, plugin) {
  let at2 = state;
  for (const patch of patches) {
    const segments = segmentsFromPath(patch.path);
    if (segments?.[0] !== "plugins" || segments[1] !== plugin) continue;
    at2 = at2 === void 0 ? void 0 : applied(at2, segments.slice(2), patch);
  }
  return at2;
}
function applied(at2, segments, patch) {
  const [segment, ...rest] = segments;
  if (segment === void 0) return patch.op === "replace" ? patch.value : void 0;
  if (isList(at2)) {
    const items = at2;
    const last = rest.length === 0;
    const index = last && segment === "-" && patch.op === "add" ? items.length : arrayIndex(segment);
    const bound = last && patch.op === "add" ? items.length : items.length - 1;
    if (index === void 0 || index > bound) return void 0;
    const copy = [...items];
    if (!last) {
      const inner = applied(items[index], rest, patch);
      if (inner === void 0) return void 0;
      copy.splice(index, 1, inner);
      return copy;
    }
    if (patch.op === "remove") copy.splice(index, 1);
    else if (patch.value === void 0) return void 0;
    else copy.splice(index, patch.op === "add" ? 0 : 1, patch.value);
    return copy;
  }
  if (!isObject(at2)) return void 0;
  const record = at2;
  const has = Object.hasOwn(record, segment);
  if (rest.length > 0) {
    if (!has) return void 0;
    const inner = applied(record[segment], rest, patch);
    return inner === void 0 ? void 0 : { ...record, [segment]: inner };
  }
  if (patch.op === "remove") {
    if (!has) return void 0;
    return Object.fromEntries(Object.entries(record).filter(([name2]) => name2 !== segment));
  }
  if (patch.value === void 0 || patch.op === "replace" && !has) return void 0;
  return { ...record, [segment]: patch.value };
}
function arrayIndex(segment) {
  return /^(0|[1-9]\d*)$/.test(segment) ? Number(segment) : void 0;
}

// srcts/state/update.ts
function update(delta, board, plugin) {
  const stored = board.plugins[plugin];
  const given = stored === void 0 ? void 0 : patched(stored, delta.patches ?? [], plugin);
  const removed = new Set(delta.blocks?.rm ?? []);
  const added = Object.keys(delta.blocks?.add ?? {});
  if (removed.size === 0 && added.length === 0 && given === stored) return [];
  if (!isDockState(given)) return [];
  const before2 = isDockState(stored) ? stored : given;
  const cleared = removeBlocks(given, removed);
  const { state: moved, moves } = rehoused(cleared, before2);
  const active = moved.active !== null && Object.hasOwn(moved.views, moved.active) ? moved.active : firstView(moved);
  const state = placeBlocks({ ...moved, active }, added);
  const at2 = (...segments) => pointer(["plugins", plugin, ...segments]);
  const patches = [];
  for (const [id, view2] of Object.entries(state.views)) {
    if (view2.grid !== given.views[id]?.grid) {
      patches.push({ op: "replace", path: at2("views", id, "grid"), value: view2.grid });
    }
  }
  for (const block of removed) {
    if (Object.hasOwn(given.visible, block)) patches.push({ op: "remove", path: at2("visible", block) });
  }
  for (const move2 of moves) {
    patches.push({ op: "replace", path: at2(move2.kind, move2.id, "folder"), value: move2.folder });
    patches.push({ op: "replace", path: at2(move2.kind, move2.id, "index"), value: move2.index });
  }
  if (state.active !== given.active) patches.push({ op: "replace", path: at2("active"), value: state.active });
  return patches;
}
function removeBlocks(state, removed) {
  if (removed.size === 0) return state;
  const kept = (panel2) => !("block" in panel2 && removed.has(panel2.block));
  const views = Object.fromEntries(
    Object.entries(state.views).map(([id, view2]) => [
      id,
      gridPanels(view2.grid).every(kept) ? view2 : { ...view2, grid: prunePanels(view2.grid, kept) }
    ])
  );
  const visible = Object.fromEntries(Object.entries(state.visible).filter(([block]) => !removed.has(block)));
  return { ...state, views, visible };
}
function rehoused(state, before2) {
  const lacks = (folder2) => folder2 !== null && !Object.hasOwn(state.folders, folder2);
  const entries = (kind, record) => Object.entries(record).map(([id, entry]) => ({ kind, id, folder: entry.folder, index: entry.index }));
  const all = [...entries("views", state.views), ...entries("folders", state.folders)];
  const stray = all.flatMap((entry) => lacks(entry.folder) ? [{ ...entry, folder: entry.folder }] : []);
  if (stray.length === 0) return { state, moves: [] };
  const placed = stray.map((entry) => {
    const path = [entry.index];
    let outer = entry.folder;
    const passed = /* @__PURE__ */ new Set([outer]);
    let parent = before2.folders[outer]?.folder ?? null;
    while (lacks(parent) && Object.hasOwn(before2.folders, parent) && !passed.has(parent)) {
      path.unshift(before2.folders[outer]?.index ?? "");
      passed.add(parent);
      outer = parent;
      parent = before2.folders[outer]?.folder ?? null;
    }
    const place = before2.folders[outer]?.index ?? null;
    return { ...entry, target: lacks(parent) ? null : parent, place, path };
  });
  const lists = /* @__PURE__ */ new Map();
  const folderKeys = (target) => {
    let list = lists.get(target);
    if (list === void 0) {
      list = all.filter((entry) => entry.folder === target).sort((a, b) => compare(a.index, b.index) || compare(a.id, b.id)).map((entry) => ({ index: entry.index, added: false }));
      lists.set(target, list);
    }
    return list;
  };
  const groups = /* @__PURE__ */ new Map();
  for (const entry of placed) {
    const key = JSON.stringify([entry.target, entry.place]);
    groups.set(key, [...groups.get(key) ?? [], entry]);
  }
  const order = [...groups.values()].sort((a, b) => placeOrder(a[0]?.place ?? null, b[0]?.place ?? null));
  const moves = [];
  for (const group of order) {
    const [first] = group;
    if (first === void 0) continue;
    group.sort((a, b) => pathOrder(a.path, b.path) || compare(a.kind, b.kind) || compare(a.id, b.id));
    const list = folderKeys(first.target);
    const place = first.place;
    const after2 = place === null ? list.length : list.filter((held) => held.added || compare(held.index, place) <= 0).length;
    const keys = keysBetween(list[after2 - 1]?.index ?? null, list[after2]?.index ?? null, group.length);
    group.forEach((entry, i) => {
      const index = keys[i] ?? entry.index;
      moves.push({ kind: entry.kind, id: entry.id, folder: first.target, index });
      list.splice(after2 + i, 0, { index, added: true });
    });
  }
  let next = state;
  for (const { kind, id, folder: folder2, index } of moves) {
    if (kind === "views") {
      const view2 = next.views[id];
      if (view2 !== void 0) next = { ...next, views: { ...next.views, [id]: { ...view2, folder: folder2, index } } };
    } else {
      const inner = next.folders[id];
      if (inner !== void 0) next = { ...next, folders: { ...next.folders, [id]: { ...inner, folder: folder2, index } } };
    }
  }
  return { state: next, moves };
}
function placeOrder(a, b) {
  if (a === b) return 0;
  if (a === null) return 1;
  if (b === null) return -1;
  return compare(a, b);
}
function pathOrder(a, b) {
  for (let i = 0; i < Math.min(a.length, b.length); i++) {
    const order = compare(a[i] ?? "", b[i] ?? "");
    if (order !== 0) return order;
  }
  return a.length - b.length;
}
function placeBlocks(state, added) {
  const active = state.active;
  if (active === null) return state;
  let next = state;
  for (const block of added) {
    const panel2 = { block };
    const view2 = next.views[active];
    if (view2 === void 0) return next;
    if (Object.values(next.views).some((other) => places(other.grid, panel2))) continue;
    next = { ...next, views: { ...next.views, [active]: { ...view2, grid: placePanel(view2.grid, panel2) } } };
  }
  return next;
}

// srcts/options/schemas.ts
var optionSchemas = {
  compact: {
    type: "boolean",
    description: "Whether each block card shows a compact header over the block's content"
  },
  view_tabs: {
    type: "boolean",
    description: "Whether the views show as a line of tabs under the navbar"
  },
  sync_rails: {
    type: "boolean",
    description: "Whether collapsing or opening a view's rail does so on every view at once"
  }
};

// srcts/options/ui.tsx
import { Checkbox, reactModule, useValue } from "@blockr/core/react";
import { jsx } from "react/jsx-runtime";
function checkboxUi(label) {
  function CheckboxUi({ host }) {
    return /* @__PURE__ */ jsx(Checkbox, { bound: useValue(host), label });
  }
  return reactModule(CheckboxUi);
}

// srcts/options/options.ts
var compact = {
  label: "Compact",
  category: "Theme options",
  schema: optionSchemas.compact,
  default: logicalDefault("compact", false),
  ui: checkboxUi("Compact")
};
var viewTabs = {
  label: "Views as tabs",
  category: "Theme options",
  schema: optionSchemas.view_tabs,
  default: logicalDefault("view_tabs", false),
  ui: checkboxUi("Views as tabs")
};
var syncRails = {
  label: "Sync rails across views",
  category: "Board options",
  schema: optionSchemas.sync_rails,
  default: logicalDefault("sync_rails", true),
  ui: checkboxUi("Sync rails across views")
};
function logicalDefault(id, fallback) {
  return (core) => logicalOption(core, id, fallback, `which the board option ${id} does not take`);
}
async function logicalOption(core, name2, fallback, refused) {
  return await readOption(core, name2, fallback, refused, (sent) => {
    const value = typeof sent === "string" ? asLogical(sent) : sent;
    return typeof value === "boolean" ? value : void 0;
  });
}
async function numericOption(core, name2, fallback, refused) {
  return await readOption(core, name2, fallback, refused, (sent) => {
    const value = typeof sent === "string" ? asNumber(sent) : sent;
    return typeof value === "number" && Number.isFinite(value) && value >= 0 ? value : void 0;
  });
}
async function readOption(core, name2, fallback, refused, read) {
  const sent = await core.blockrOption(name2);
  if (sent === void 0) return fallback;
  const value = read(sent);
  if (value !== void 0) return value;
  throw new TypeError(`The blockr option ${name2} is ${JSON.stringify(sent)}, ${refused}.`);
}
function asNumber(text2) {
  const number = text2.trim() === "" ? Number.NaN : Number(text2);
  return Number.isFinite(number) ? number : void 0;
}
function asLogical(text2) {
  const trimmed = text2.trim();
  if (["TRUE", "true", "True", "T"].includes(trimmed)) return true;
  if (["FALSE", "false", "False", "F"].includes(trimmed)) return false;
  return void 0;
}

// srcts/commands/common.ts
var DOCK = "blockr.dock-dock_plugin";
function dockState(board) {
  const state = board.plugins[DOCK];
  return isDockState(state) ? state : void 0;
}
function activeView(board) {
  const state = dockState(board);
  const active = state?.active ?? null;
  return active === null ? void 0 : state?.views[active];
}
function at(...segments) {
  return pointer(["plugins", DOCK, ...segments]);
}
function command(definition) {
  return definition;
}
function fits(state, folder2, before2, moving) {
  if (before2 === void 0) return true;
  if (moving !== void 0 && sameEntry(before2, moving)) return false;
  return inFolder(state, folder2).some((entry) => sameEntry(entry, before2));
}
function folderOrTop(state, folder2) {
  return folder2 === null || Object.hasOwn(state.folders, folder2);
}
function givenGridFailures(board, grid2) {
  const lacking = gridPanels(grid2).flatMap(
    (panel2) => "block" in panel2 && !Object.hasOwn(board.blocks, panel2.block) ? [panel2.block] : []
  );
  return [...gridFailures(grid2), ...lacking.map((block) => `The grid places the panel of the block "${block}", which the board does not hold.`)];
}
function refuse(context, failures) {
  if (failures.length > 0) throw context.refusal(failures);
}
function holdsBlock(board, panel2) {
  return !("block" in panel2) || Object.hasOwn(board.blocks, panel2.block);
}
async function layoutLocks(context) {
  if (!context.lock.board) return { rearrange: false, resize: false };
  const read = (option) => logicalOption(context, option, true, "where it takes TRUE or FALSE");
  const [rearrange, resize] = await Promise.all([read("lock_rearrange"), read("lock_resize")]);
  return { rearrange, resize };
}
async function gridSizeTolerance(context) {
  return await numericOption(context, "dock_grid_size_tol", 5e-3, "where it takes a number of 0 or more");
}
function nextName(word, taken2) {
  let n = taken2.length + 1;
  while (taken2.includes(`${word} ${String(n)}`)) n += 1;
  return `${word} ${String(n)}`;
}

// srcts/commands/schemas.ts
function object(properties, required = []) {
  const defs = definitionsFor(properties);
  return {
    type: "object",
    properties,
    required,
    additionalProperties: false,
    ...Object.keys(defs).length === 0 ? {} : { $defs: defs }
  };
}
var DEFINITIONS = JSON.parse(
  JSON.stringify(dock_schema_default.definitions).replaceAll('"#/definitions/', '"#/$defs/')
);
function ref(name2, description) {
  return { $ref: `#/$defs/${name2}`, description };
}
function definitionsFor(value) {
  const picked = /* @__PURE__ */ new Map();
  const queue = referenced(value);
  for (let name2 = queue.pop(); name2 !== void 0; name2 = queue.pop()) {
    if (picked.has(name2)) continue;
    const definition = DEFINITIONS[name2];
    if (definition === void 0) throw new Error(`The schema of the dock's state defines no "${name2}".`);
    picked.set(name2, definition);
    queue.push(...referenced(definition));
  }
  return Object.fromEntries([...picked].sort(([a], [b]) => a < b ? -1 : 1));
}
function referenced(value) {
  return [...JSON.stringify(value).matchAll(/"#\/\$defs\/([^"]+)"/g)].flatMap((match) => match.slice(1, 2));
}
var view = { type: "string", description: "The id of a view." };
var folder = { type: "string", description: "The id of a folder." };
function into(what, optional = false) {
  const top = optional ? " It goes to the top where this is left out." : "";
  return {
    type: ["string", "null"],
    description: `The id of the folder that the ${what} goes into, or null for the top of the navigation.${top}`
  };
}
function name(what, description) {
  return {
    type: "string",
    pattern: "\\S",
    description: `${description} It holds a character other than a space, and may be the name of another ${what}.`
  };
}
var before = {
  description: 'The view or folder that it goes before, as { "view": id } or { "folder": id }, which sits in the folder that it goes into. It goes at the end of that folder where this is left out.',
  anyOf: [object({ view }, ["view"]), object({ folder }, ["folder"])]
};
var panel = ref("Panel", `A panel, as a grid names it: { "block": id } for a block's card, or { "plugin": id } for the UI of a plugin, by the id of its class.`);
function near(what) {
  return ref(
    "Panel",
    `A panel of the view, by whose tab group the panel ${what}: in it, or beside it on the side that \`side\` names. Where this panel sits on a rail, the panel ${what} onto that rail, and \`side\` is within.`
  );
}
var side = {
  type: "string",
  enum: SIDES,
  description: "Where the panel goes by the tab group of the panel that `near` names: within it, as a new tab, or in a new tab group to its left or right, or above or below it. It is within unless given. Without `near`, a side other than within places the panel at that edge of the whole grid."
};
var rail = {
  type: "string",
  enum: EDGES,
  description: "The edge of the view whose rail takes the panel, as a new tab, which opens a rail there where the view has none. It takes precedence over `near` and `side`."
};
var edge = { type: "string", enum: EDGES, description: "The edge of the view that the rail sits on." };
var grid = ref("Grid", "A view's grid, which says which panels it shows and where.");

// srcts/commands/panels.ts
function boardView(board, id) {
  return dockState(board)?.views[id];
}
function shows(board, view2, panel2) {
  const held = boardView(board, view2);
  return held !== void 0 && places(held.grid, panel2);
}
function gridPatch(view2, grid2) {
  return { op: "replace", path: at("views", view2, "grid"), value: grid2 };
}
function withoutPanel(grid2, panel2) {
  return prunePanels(grid2, (placed) => !samePanel(placed, panel2));
}
function placeArgs(what) {
  return object(
    {
      view,
      panel,
      near: near(what),
      side,
      rail
    },
    ["view", "panel"]
  );
}
var KEPT = {
  panels: "The board is locked, which keeps the panels that each view shows.",
  arrangement: "The board is locked, and the blockr option lock_rearrange keeps where each panel sits.",
  sizes: "The board is locked, and the blockr option lock_resize keeps the sizes of the splits and rails."
};
function keep(changes, locks, context) {
  if (!context.lock.board) return;
  refuse(context, [
    ...changes.panels === true ? [KEPT.panels] : [],
    ...changes.arrangement === true && locks.rearrange ? [KEPT.arrangement] : [],
    ...changes.sizes === true && locks.resize ? [KEPT.sizes] : []
  ]);
}
var panelCommands = [
  command({
    id: "blockr.dock-add_panel",
    label: "Add panel",
    description: "Places a panel in a view that does not show it, as the tab that its tab group or rail shows: in or beside the tab group of the panel that `near` names, on the side that `side` names, on the rail that `rail` names, or at the default place, a new tab in the last tab group that holds no plugin's panel.",
    args: placeArgs("goes"),
    preflight(board, { view: view2, panel: panel2 }, { lock }) {
      return !lock.board && boardView(board, view2) !== void 0 && !shows(board, view2, panel2) && holdsBlock(board, panel2);
    },
    run(board, { view: view2, panel: panel2, near: near2, side: side2, rail: rail2 }, context) {
      const held = boardView(board, view2);
      if (held === void 0) return [];
      const place = { near: near2, side: side2, rail: rail2 };
      refuse(context, placeFailures(held.grid, place));
      return { patches: [gridPatch(view2, placedAt(held.grid, panel2, place, "distribute"))] };
    }
  }),
  command({
    id: "blockr.dock-remove_panel",
    label: "Remove panel",
    description: "Takes a panel out of a view, as closing its tab does. It removes no block, and the panel stays in the other views that show it.",
    args: object({ view, panel }, ["view", "panel"]),
    preflight(board, { view: view2, panel: panel2 }, { lock }) {
      return !lock.board && shows(board, view2, panel2);
    },
    run(board, { view: view2, panel: panel2 }) {
      const held = boardView(board, view2);
      if (held === void 0) return [];
      return { patches: [gridPatch(view2, withoutPanel(held.grid, panel2))] };
    }
  }),
  command({
    id: "blockr.dock-move_panel",
    label: "Move panel",
    description: "Moves a panel within a view that shows it, placed as blockr.dock-add_panel places one: in or beside the tab group of the panel that `near` names, on the side that `side` names, on the rail that `rail` names, or at the default place.",
    args: placeArgs("moves"),
    preflight(board, { view: view2, panel: panel2, near: near2 }) {
      return shows(board, view2, panel2) && (near2 === void 0 || !samePanel(near2, panel2));
    },
    async prepare(_board, args, context) {
      return { ...args, locks: await layoutLocks(context) };
    },
    run(board, { view: view2, panel: panel2, near: near2, side: side2, rail: rail2, locks }, context) {
      keep({ arrangement: true }, locks, context);
      const held = boardView(board, view2);
      if (held === void 0) return [];
      const from = locate(held.grid, panel2);
      if (rail2 !== void 0 && from !== void 0 && "edge" in from && from.edge === rail2) {
        if (held.grid.rails?.[rail2]?.panels.length === 1) return [];
      }
      const place = { near: near2, side: side2, rail: rail2 };
      const rest = withoutPanel(held.grid, panel2);
      refuse(context, placeFailures(rest, place));
      return { patches: [gridPatch(view2, placedAt(rest, panel2, place, "split"))] };
    }
  }),
  command({
    id: "blockr.dock-resize_panel",
    label: "Resize panel",
    description: "Sets the size of the tab group that holds a panel: its share of the split that holds it, a ratio between 0 and 1, while the other children of the split share the rest in proportion. For a panel on a rail, it sets the rail's width in pixels while it is open.",
    args: object(
      {
        view,
        panel,
        size: {
          type: "number",
          exclusiveMinimum: 0,
          description: "The share of its split that the tab group takes, above 0 and below 1, or the width in pixels of the rail."
        }
      },
      ["view", "panel", "size"]
    ),
    preflight(board, { view: view2, panel: panel2 }) {
      return shows(board, view2, panel2);
    },
    async prepare(_board, args, context) {
      return { ...args, locks: await layoutLocks(context) };
    },
    run(board, { view: view2, panel: panel2, size, locks }, context) {
      keep({ sizes: true }, locks, context);
      const held = boardView(board, view2);
      if (held === void 0) return [];
      refuse(context, resizeFailures(held.grid, panel2, size));
      return { patches: [gridPatch(view2, resized(held.grid, panel2, size))] };
    }
  }),
  command({
    id: "blockr.dock-activate_panel",
    label: "Activate panel",
    description: "Has the tab group or rail that holds a panel show it, as clicking its tab does. It takes no undo step, since it changes only what the user looks at.",
    args: object({ view, panel }, ["view", "panel"]),
    undo: false,
    preflight(board, { view: view2, panel: panel2 }) {
      return shows(board, view2, panel2);
    },
    run(board, { view: view2, panel: panel2 }) {
      const held = boardView(board, view2);
      if (held === void 0) return [];
      return { patches: [gridPatch(view2, showing(held.grid, panel2))] };
    }
  }),
  command({
    id: "blockr.dock-show_panel",
    label: "Show panel",
    description: "Shows a panel in the active view, as the dock carries out core's show(): has the tab group or rail that holds it show it, opening the rail where it is collapsed, where the view shows it, and places it at the default place where the view does not, a new tab in the last tab group that holds no plugin's panel. While the board option sync_rails is on, a rail that it opens opens on every view that has a rail on that edge. It never switches views, and takes no undo step, since showing takes none. On a locked board, it only has a tab group or rail show a panel that the view shows.",
    args: object({ panel }, ["panel"]),
    undo: false,
    preflight(board, { panel: panel2 }, { lock }) {
      const active = dockState(board)?.active ?? null;
      if (active === null || !holdsBlock(board, panel2)) return false;
      return !lock.board || shows(board, active, panel2);
    },
    run(board, { panel: panel2 }) {
      const state = dockState(board);
      const active = state?.active ?? null;
      const held = active === null ? void 0 : state?.views[active];
      if (state === void 0 || active === null || held === void 0) return [];
      if (!places(held.grid, panel2)) return { patches: [gridPatch(active, placePanel(held.grid, panel2))] };
      const grid2 = showing(held.grid, panel2);
      const at2 = locate(grid2, panel2);
      if (at2 === void 0 || !("edge" in at2) || grid2.rails?.[at2.edge]?.collapsed !== true) {
        return { patches: [gridPatch(active, grid2)] };
      }
      const shown = { ...state, views: { ...state.views, [active]: { ...held, grid: grid2 } } };
      return { patches: railPatches(board, shown, active, at2.edge, false) };
    }
  }),
  command({
    id: "blockr.dock-set_grid",
    label: "Set grid",
    description: "Sets a view's whole grid, which says which panels the view shows and where, as the dock does after the user drags a panel or a border. A grid places each panel once, and the shares of each split are above 0 and sum to 1. Where the grid has the same splits and tab groups as the view's, a split keeps its shares unless the grid moves one by more than the blockr option dock_grid_size_tol, 0.005 unless set.",
    args: object({ view, grid }, ["view", "grid"]),
    preflight(board, { view: view2 }) {
      return boardView(board, view2) !== void 0;
    },
    async prepare(_board, args, context) {
      const [locks, tolerance] = await Promise.all([layoutLocks(context), gridSizeTolerance(context)]);
      return { ...args, locks, tolerance };
    },
    run(board, { view: view2, grid: grid2, locks, tolerance }, context) {
      const held = boardView(board, view2);
      if (held === void 0) return [];
      refuse(context, givenGridFailures(board, grid2));
      const kept = withStoredShares(held.grid, grid2, tolerance);
      keep(gridChanges(held.grid, kept), locks, context);
      return { patches: [gridPatch(view2, kept)] };
    }
  }),
  command({
    id: "blockr.dock-collapse_rail",
    label: "Collapse rail",
    description: "Collapses the rail on an edge of a view, which then shows its tabs alone, or opens it. While the board option sync_rails is on, it does so on every view that has a rail on that edge. It takes no undo step, since it changes only what the user looks at.",
    args: object(
      {
        view,
        edge,
        collapsed: { type: "boolean", description: "Whether the rail is collapsed, or open." }
      },
      ["view", "edge", "collapsed"]
    ),
    undo: false,
    preflight(board, { view: view2, edge: edge2 }) {
      return boardView(board, view2)?.grid.rails?.[edge2] !== void 0;
    },
    run(board, { view: view2, edge: edge2, collapsed }) {
      const state = dockState(board);
      if (state === void 0) return [];
      return { patches: railPatches(board, state, view2, edge2, collapsed) };
    }
  })
];
function railPatches(board, state, view2, edge2, collapsed) {
  const synced = board.options.sync_rails !== false;
  return railViews(state, synced ? void 0 : view2, edge2).map(([id, grid2]) => gridPatch(id, collapsedRail(grid2, edge2, collapsed)));
}
function railViews(state, view2, edge2) {
  return Object.entries(state.views).flatMap(
    ([id, held]) => (view2 === void 0 || id === view2) && held.grid.rails?.[edge2] !== void 0 ? [[id, held.grid]] : []
  );
}

// srcts/commands/sections.ts
var SECTIONS = ["inputs", "outputs"];
var sectionCommands = [
  command({
    id: "blockr.dock-set_sections",
    label: "Set card sections",
    description: "Sets the sections of a block's card that show: its inputs, its outputs, both, or neither. A card shows both unless this sets it otherwise.",
    args: object(
      {
        block: { type: "string", description: "The id of a block of the board." },
        sections: {
          type: "array",
          items: ref("Section", "A section of a block's card."),
          uniqueItems: true,
          description: 'The sections that the card shows, of "inputs" and "outputs".'
        }
      },
      ["block", "sections"]
    ),
    preflight(board, { block }, { lock }) {
      return !lock.board && dockState(board) !== void 0 && Object.hasOwn(board.blocks, block);
    },
    run(board, { block, sections }) {
      const state = dockState(board);
      if (state === void 0) return [];
      const shown = SECTIONS.filter((section) => sections.includes(section));
      if (shown.length === SECTIONS.length) {
        return Object.hasOwn(state.visible, block) ? { patches: [{ op: "remove", path: at("visible", block) }] } : [];
      }
      return { patches: [{ op: "add", path: at("visible", block), value: shown }] };
    }
  })
];

// srcts/commands/views.ts
var EMPTY = { orientation: "horizontal", children: [], sizes: [] };
function editable(state, locked) {
  return state !== void 0 && !locked;
}
function taken(state) {
  return (id) => Object.hasOwn(state.views, id) || Object.hasOwn(state.folders, id);
}
var viewCommands = [
  command({
    id: "blockr.dock-add_view",
    label: "Add view",
    description: 'Adds a view, under a new id, and makes it the active view. It goes into a folder, or to the top, before the view or folder that `before` names, or at the end. Where no name is given, it is named "View N", with N one more than the views there are, or the first N after it that no view\'s name has. Where no grid is given, it shows nothing.',
    args: object({
      name: name("view", "The name of the new view."),
      folder: into("view", true),
      before,
      grid: ref("Grid", "The grid of the new view, which says which panels it shows and where.")
    }),
    preflight(board, args, { lock }) {
      const state = dockState(board);
      const folder2 = args.folder ?? null;
      return editable(state, lock.board) && folderOrTop(state, folder2) && fits(state, folder2, args.before);
    },
    run(board, args, context) {
      const state = dockState(board);
      if (state === void 0) return [];
      const grid2 = args.grid ?? EMPTY;
      refuse(context, givenGridFailures(board, grid2));
      const folder2 = args.folder ?? null;
      const id = context.mintId(taken(state));
      const view2 = {
        name: args.name ?? nextName("View", Object.values(state.views).map((other) => other.name)),
        folder: folder2,
        index: mintKey(state, folder2, args.before),
        grid: grid2
      };
      return {
        patches: [
          { op: "add", path: at("views", id), value: view2 },
          { op: "replace", path: at("active"), value: id }
        ]
      };
    }
  }),
  command({
    id: "blockr.dock-remove_view",
    label: "Remove view",
    description: "Removes a view, and makes the first view of the navigation active where the view it removes was. It removes no block, and refuses to remove the last view.",
    args: object({ view }, ["view"]),
    preflight(board, { view: view2 }, { lock }) {
      const state = dockState(board);
      return editable(state, lock.board) && Object.hasOwn(state.views, view2) && Object.keys(state.views).length > 1;
    },
    run(board, { view: view2 }) {
      const state = dockState(board);
      if (state === void 0) return [];
      const views = Object.fromEntries(Object.entries(state.views).filter(([id]) => id !== view2));
      const patches = [{ op: "remove", path: at("views", view2) }];
      if (state.active === view2) patches.push({ op: "replace", path: at("active"), value: firstView({ ...state, views }) });
      return { patches };
    }
  }),
  command({
    id: "blockr.dock-rename_view",
    label: "Rename view",
    description: "Gives a view a new name.",
    args: object({ view, name: name("view", "The view's new name.") }, ["view", "name"]),
    preflight(board, { view: view2 }, { lock }) {
      const state = dockState(board);
      return editable(state, lock.board) && Object.hasOwn(state.views, view2);
    },
    run: (_board, { view: view2, name: name2 }) => ({ patches: [{ op: "replace", path: at("views", view2, "name"), value: name2 }] })
  }),
  command({
    id: "blockr.dock-move_view",
    label: "Move view",
    description: "Moves a view into a folder, or to the top, before the view or folder that `before` names, or at the end of that folder.",
    args: object({ view, folder: into("view"), before }, ["view", "folder"]),
    preflight(board, { view: view2, folder: folder2, before: before2 }, { lock }) {
      const state = dockState(board);
      return editable(state, lock.board) && Object.hasOwn(state.views, view2) && folderOrTop(state, folder2) && fits(state, folder2, before2, { view: view2 });
    },
    run(board, { view: view2, folder: folder2, before: before2 }) {
      const state = dockState(board);
      const moving = state?.views[view2];
      if (state === void 0 || moving === void 0) return [];
      if (moving.folder === folder2 && sitsAt(state, { view: view2 }, folder2, before2)) return [];
      return {
        patches: [
          { op: "replace", path: at("views", view2, "folder"), value: folder2 },
          { op: "replace", path: at("views", view2, "index"), value: mintKey(state, folder2, before2, { view: view2 }) }
        ]
      };
    }
  }),
  command({
    id: "blockr.dock-activate_view",
    label: "Activate view",
    description: "Makes a view the active one, which the dock shows. It takes no undo step, since it changes only what the user looks at.",
    args: object({ view }, ["view"]),
    undo: false,
    preflight(board, { view: view2 }) {
      const state = dockState(board);
      return state !== void 0 && Object.hasOwn(state.views, view2);
    },
    run: (_board, { view: view2 }) => ({ patches: [{ op: "replace", path: at("active"), value: view2 }] })
  }),
  command({
    id: "blockr.dock-add_folder",
    label: "Add folder",
    description: 'Adds a folder, under a new id, which groups views and other folders in the navigation. It goes into a folder, or to the top, before the view or folder that `before` names, or at the end. Where no name is given, it is named "Folder N", with N one more than the folders there are, or the first N after it that no folder\'s name has.',
    args: object({
      name: name("folder", "The name of the new folder."),
      folder: into("folder", true),
      before
    }),
    preflight(board, args, { lock }) {
      const state = dockState(board);
      const folder2 = args.folder ?? null;
      return editable(state, lock.board) && folderOrTop(state, folder2) && fits(state, folder2, args.before);
    },
    run(board, args, { mintId }) {
      const state = dockState(board);
      if (state === void 0) return [];
      const folder2 = args.folder ?? null;
      const value = {
        name: args.name ?? nextName("Folder", Object.values(state.folders).map((other) => other.name)),
        folder: folder2,
        index: mintKey(state, folder2, args.before)
      };
      return { patches: [{ op: "add", path: at("folders", mintId(taken(state))), value }] };
    }
  }),
  command({
    id: "blockr.dock-remove_folder",
    label: "Remove folder",
    description: "Removes a folder, and moves the views and folders that it holds up into the folder that held it, in its place and in their order. It removes no view.",
    args: object({ folder }, ["folder"]),
    preflight(board, { folder: folder2 }, { lock }) {
      const state = dockState(board);
      return editable(state, lock.board) && Object.hasOwn(state.folders, folder2);
    },
    run(board, { folder: folder2 }) {
      const state = dockState(board);
      if (state === void 0) return [];
      const folders = Object.fromEntries(Object.entries(state.folders).filter(([id]) => id !== folder2));
      const { moves } = rehoused({ ...state, folders }, state);
      return {
        patches: [
          { op: "remove", path: at("folders", folder2) },
          ...moves.flatMap((move2) => [
            { op: "replace", path: at(move2.kind, move2.id, "folder"), value: move2.folder },
            { op: "replace", path: at(move2.kind, move2.id, "index"), value: move2.index }
          ])
        ]
      };
    }
  }),
  command({
    id: "blockr.dock-rename_folder",
    label: "Rename folder",
    description: "Gives a folder a new name.",
    args: object({ folder, name: name("folder", "The folder's new name.") }, ["folder", "name"]),
    preflight(board, { folder: folder2 }, { lock }) {
      const state = dockState(board);
      return editable(state, lock.board) && Object.hasOwn(state.folders, folder2);
    },
    run: (_board, { folder: folder2, name: name2 }) => ({
      patches: [{ op: "replace", path: at("folders", folder2, "name"), value: name2 }]
    })
  }),
  command({
    id: "blockr.dock-move_folder",
    label: "Move folder",
    description: "Moves a folder, with what it holds, into another folder, or to the top, before the view or folder that `before` names, or at the end of that folder. It refuses to move a folder into itself or into a folder inside it.",
    args: object({ folder, into: into("folder"), before }, ["folder", "into"]),
    preflight(board, { folder: folder2, into: into2, before: before2 }, { lock }) {
      const state = dockState(board);
      return editable(state, lock.board) && Object.hasOwn(state.folders, folder2) && folderOrTop(state, into2) && (into2 === null || !within(state, into2, folder2)) && fits(state, into2, before2, { folder: folder2 });
    },
    run(board, { folder: folder2, into: into2, before: before2 }) {
      const state = dockState(board);
      const moving = state?.folders[folder2];
      if (state === void 0 || moving === void 0) return [];
      if (moving.folder === into2 && sitsAt(state, { folder: folder2 }, into2, before2)) return [];
      return {
        patches: [
          { op: "replace", path: at("folders", folder2, "folder"), value: into2 },
          { op: "replace", path: at("folders", folder2, "index"), value: mintKey(state, into2, before2, { folder: folder2 }) }
        ]
      };
    }
  })
];

// srcts/commands/index.ts
var dockCommands = [...viewCommands, ...panelCommands, ...sectionCommands];

// srcts/modules/dock/api.ts
var API = 1;

// srcts/layout/gestures.ts
function gestureRuns(view2, shown, read, stored, reading) {
  const before2 = withStoredRails(shown, stored);
  const after2 = withStoredRails(
    withStoredShares(shown, read, reading.shares ? reading.tolerance : Infinity),
    stored,
    reading.widths ? shown : void 0
  );
  const changes = gridChanges(before2, after2);
  if (changes.panels || changes.arrangement || changes.sizes) {
    return [{ id: "blockr.dock-set_grid", args: { view: view2, grid: after2 } }];
  }
  const runs = [];
  const activate2 = (panel2) => {
    runs.push({ id: "blockr.dock-activate_panel", args: { view: view2, panel: panel2 } });
  };
  const groups = tabGroups(before2);
  tabGroups(after2).forEach((group, i) => {
    const was = groups[i];
    if (was !== void 0 && !samePanel(was.active, group.active)) activate2(group.active);
  });
  for (const edge2 of EDGES) {
    const [was, rail2] = [before2.rails?.[edge2], after2.rails?.[edge2]];
    if (was === void 0 || rail2 === void 0) continue;
    if (!samePanel(was.active, rail2.active)) activate2(rail2.active);
    if (was.collapsed !== rail2.collapsed) {
      runs.push({ id: "blockr.dock-collapse_rail", args: { view: view2, edge: edge2, collapsed: rail2.collapsed } });
    }
  }
  return runs;
}
function columnRuns(view2, shown, read) {
  const held = gridPanels(read);
  const removed = gridPanels(shown).filter((panel2) => !held.some((other) => samePanel(other, panel2)));
  const showing2 = tabGroups(shown).map((group) => group.active);
  const shows2 = tabGroups(read).map((group) => group.active).filter((panel2) => !showing2.some((other) => samePanel(other, panel2)));
  return [
    ...removed.map((panel2) => ({ id: "blockr.dock-remove_panel", args: { view: view2, panel: panel2 } })),
    ...shows2.map((panel2) => ({ id: "blockr.dock-activate_panel", args: { view: view2, panel: panel2 } }))
  ];
}
function withStoredRails(grid2, stored, moved) {
  if (grid2.rails === void 0) return grid2;
  const rails2 = {};
  for (const edge2 of EDGES) {
    const rail2 = grid2.rails[edge2];
    if (rail2 === void 0) continue;
    const kept = stored.rails?.[edge2];
    const was = moved?.rails?.[edge2];
    const dragged = was !== void 0 && was.size !== rail2.size;
    rails2[edge2] = kept === void 0 ? rail2 : { ...rail2, size: dragged ? rail2.size : kept.size };
  }
  return { ...grid2, rails: rails2 };
}

// srcts/layout/ids.ts
function panelId(panel2) {
  return "block" in panel2 ? JSON.stringify({ block: panel2.block }) : JSON.stringify({ plugin: panel2.plugin });
}
function panelFromId(id) {
  let value;
  try {
    value = JSON.parse(id);
  } catch {
    return void 0;
  }
  if (typeof value !== "object" || value === null || Array.isArray(value)) return void 0;
  const keys = Object.keys(value);
  if (keys.length !== 1) return void 0;
  const { block, plugin } = value;
  if (typeof block === "string") return { block };
  if (typeof plugin === "string") return { plugin };
  return void 0;
}
function railId(edge2) {
  return `rail-${edge2}`;
}

// srcts/layout/layout.ts
import { Orientation } from "./vendor-a3ad4872.js";
var PANEL_COMPONENT = "panel";
function gridToLayout(grid2, title, size) {
  const panels = {};
  let groups = 0;
  const take = (list) => list.flatMap((panel2) => {
    const id = panelId(panel2);
    if (Object.hasOwn(panels, id)) return [];
    const name2 = title(panel2);
    if (name2 === void 0) return [];
    const params = { panel: panelFromId(id) ?? panel2 };
    panels[id] = { id, contentComponent: PANEL_COMPONENT, title: name2, params };
    return [id];
  });
  const shown = (group) => {
    const views = take(group.panels);
    const active = panelId(group.active);
    const activeView2 = views.includes(active) ? active : views[0];
    return activeView2 === void 0 ? { views } : { views, activeView: activeView2 };
  };
  const node = (child, share) => {
    if (isTabGroup2(child)) {
      const state = shown(child);
      if (state.views.length === 0) return void 0;
      groups += 1;
      return { type: "leaf", size: share, data: { id: `group-${String(groups)}`, ...state } };
    }
    const data = split(child);
    return data.length === 0 ? void 0 : { type: "branch", size: share, data };
  };
  const split = (parent) => {
    const shares2 = fitted(parent.sizes, parent.children.length);
    const kept = parent.children.flatMap((child, i) => node(child, shares2[i] ?? 0) ?? []);
    const rescaled = fitted(
      kept.map((object2) => object2.size ?? 0),
      kept.length
    );
    return kept.map((object2, i) => ({ ...object2, size: rescaled[i] ?? 0 }));
  };
  const root = split(grid2);
  const edgeGroup = (edge2, rail2) => {
    const state = rail2 === void 0 ? { views: [] } : shown(rail2);
    const holds = state.views.length > 0;
    return {
      size: rail2?.size ?? RAIL_SIZE,
      visible: holds,
      collapsed: holds && rail2?.collapsed === true,
      group: { id: railId(edge2), ...state }
    };
  };
  const edgeGroups = {};
  for (const edge2 of EDGES) edgeGroups[edge2] = edgeGroup(edge2, grid2.rails?.[edge2]);
  return {
    grid: {
      root: { type: "branch", size: 1, data: root },
      width: size.width,
      height: size.height,
      orientation: grid2.orientation === "vertical" ? Orientation.VERTICAL : Orientation.HORIZONTAL
    },
    panels,
    edgeGroups
  };
}
function layoutToGrid(layout) {
  const { root, orientation } = layout.grid;
  const top = Array.isArray(root.data) ? children(root.data) : children([root]);
  const rails2 = {};
  for (const edge2 of EDGES) {
    const rail2 = asRail(layout.edgeGroups?.[edge2]);
    if (rail2 !== void 0) rails2[edge2] = rail2;
  }
  return {
    orientation: orientation === Orientation.VERTICAL ? "vertical" : "horizontal",
    ...top,
    ...Object.keys(rails2).length === 0 ? {} : { rails: rails2 }
  };
}
function children(objects) {
  const kept = objects.flatMap((object2) => {
    const node = asNode(object2);
    return node === void 0 ? [] : [{ node, size: object2.size ?? 0 }];
  });
  return {
    children: kept.map(({ node }) => node),
    sizes: fitted(
      kept.map(({ size }) => size),
      kept.length
    )
  };
}
function asNode(object2) {
  if (!Array.isArray(object2.data)) return asTabGroup(object2.data);
  const split = children(object2.data);
  return split.children.length === 0 ? void 0 : split;
}
function asTabGroup(state) {
  const panels = state.views.flatMap((id) => panelFromId(id) ?? []);
  const [first] = panels;
  if (first === void 0) return void 0;
  const shown = state.activeView === void 0 ? void 0 : panelFromId(state.activeView);
  const active = shown === void 0 ? void 0 : panels.find((panel2) => samePanel(panel2, shown));
  return { panels, active: active ?? first };
}
function asRail(edgeGroup) {
  if (edgeGroup === void 0 || !isGroupState(edgeGroup.group)) return void 0;
  const group = asTabGroup(edgeGroup.group);
  if (group === void 0) return void 0;
  return {
    ...group,
    collapsed: edgeGroup.collapsed === true,
    size: Math.max(0, Math.round(edgeGroup.size))
  };
}
function isGroupState(value) {
  if (typeof value !== "object" || value === null) return false;
  const { views, activeView: activeView2 } = value;
  return Array.isArray(views) && views.every((view2) => typeof view2 === "string") && (activeView2 === void 0 || typeof activeView2 === "string");
}

// srcts/layout/stack.ts
function stackGrid(grid2, cap) {
  const rows = [];
  const walk = (children2, sizes, vertical, height) => {
    const shares2 = fitted(sizes, children2.length);
    children2.forEach((child, i) => {
      const share = vertical ? height * (shares2[i] ?? 0) : height;
      if (isTabGroup2(child)) rows.push({ group: child, height: Math.min(share, cap) });
      else walk(child.children, child.sizes, !vertical, share);
    });
  };
  walk(grid2.children, grid2.sizes, grid2.orientation === "vertical", 1);
  for (const edge2 of EDGES) {
    const rail2 = grid2.rails?.[edge2];
    if (rail2 !== void 0 && rail2.panels.length > 0) {
      rows.push({ group: { panels: rail2.panels, active: rail2.active }, height: cap });
    }
  }
  if (rows.length === 0) return { grid: { orientation: "vertical", children: [], sizes: [] }, height: cap };
  const heights = rows.map((row) => row.height);
  return {
    grid: { orientation: "vertical", children: rows.map((row) => row.group), sizes: fitted(heights, heights.length) },
    height: heights.reduce((total, height) => total + height, 0)
  };
}

// srcts/modules/dock/demand.ts
var OWNER = "blockr.dock";
var SHOWN = 1;
function onScreen(board, drawn, narrow) {
  const state = dockState(board);
  const active = state?.active ?? null;
  const view2 = active === null ? void 0 : state?.views[active];
  if (view2 === void 0) return { blocks: [], plugins: [] };
  const title = (panel2) => "block" in panel2 ? board.blocks[panel2.block]?.name : drawn.get(panel2.plugin);
  const grid2 = layoutToGrid(gridToLayout(view2.grid, title, { width: 0, height: 0 }));
  const shown = [
    ...tabGroups(grid2).map((group) => group.active),
    ...EDGES.flatMap((edge2) => {
      const rail2 = grid2.rails?.[edge2];
      return rail2 === void 0 || rail2.collapsed && !narrow ? [] : [rail2.active];
    })
  ];
  return {
    blocks: shown.flatMap((panel2) => "block" in panel2 ? [panel2.block] : []),
    plugins: shown.flatMap((panel2) => "plugin" in panel2 ? [panel2.plugin] : [])
  };
}
function demandShown(core, drawn, narrow) {
  const blocks = () => (board) => onScreen(board, drawn, narrow.get()).blocks;
  const demand = core.scheduler.demand(OWNER, blocks(), { priority: SHOWN });
  const tell = (board) => {
    core.onScreen.set(onScreen(board, drawn, narrow.get()));
  };
  tell(core.store.board());
  const offBoard = core.store.subscribe((_change, board) => {
    tell(board);
  });
  const offNarrow = narrow.subscribe(() => {
    demand.set(blocks(), { priority: SHOWN });
    tell(core.store.board());
  });
  return () => {
    offNarrow();
    offBoard();
    demand.release();
  };
}

// srcts/modules/dock/busy.ts
function trackBusy(core, drawn, narrow, delay) {
  let turning = false;
  let timer;
  const listeners = /* @__PURE__ */ new Set();
  const evaluating = () => onScreen(core.store.board(), drawn, narrow.get()).blocks.some((block) => {
    const status = core.scheduler.status(block)?.status;
    return status === "stale" || status === "unevaluated";
  });
  function turn(on) {
    if (on === turning) return;
    turning = on;
    for (const fn of [...listeners]) fn();
  }
  function check() {
    if (!evaluating()) {
      clearTimeout(timer);
      timer = void 0;
      turn(false);
    } else if (!turning && timer === void 0) {
      timer = setTimeout(() => {
        timer = void 0;
        turn(true);
      }, delay);
    }
  }
  const offs = [
    core.scheduler.subscribe(check),
    core.store.subscribe(() => {
      check();
    }),
    narrow.subscribe(check)
  ];
  check();
  return {
    get: () => turning,
    subscribe(fn) {
      listeners.add(fn);
      return () => {
        listeners.delete(fn);
      };
    },
    stop() {
      for (const off of offs) off();
      clearTimeout(timer);
      timer = void 0;
      listeners.clear();
    }
  };
}

// srcts/modules/dock/card/card.tsx
import { useBoard, useNotified, useStatus } from "@blockr/core/react";
import { useCallback as useCallback4, useMemo as useMemo2, useState as useState4 } from "react";

// srcts/modules/dock/context.ts
import { createContext, useContext } from "react";
var DockContext = createContext(null);
function useDock() {
  const value = useContext(DockContext);
  if (value === null) throw new Error("A component of the dock is drawn outside the dock.");
  return value;
}
function runEdit(core, id, args) {
  core.commands.run(id, args).catch(() => {
  });
}
function giveFocus(core, block) {
  if (core.focus.get() !== block) core.focus.set(block);
}

// srcts/modules/dock/mounted.tsx
import { useLayoutEffect, useRef } from "react";
import { jsx as jsx2 } from "react/jsx-runtime";
function Mounted({ mount, label, className, data }) {
  const ref2 = useRef(null);
  useLayoutEffect(() => {
    const el = ref2.current;
    if (el === null) return void 0;
    const end = mount(el);
    return () => {
      queueMicrotask(end);
    };
  }, [mount]);
  return /* @__PURE__ */ jsx2("div", { ref: ref2, role: label === void 0 ? void 0 : "region", "aria-label": label, className, ...data });
}

// srcts/modules/dock/notifications.ts
function onCard(notification) {
  const { about, source } = notification;
  if (about === void 0 || !("block" in about)) return false;
  return source.kind !== "module" || !("plugin" in source.module.object);
}
var ANSI = /(?:\u001b\[|\u009b)[0-?]*[ -/]*[@-~]|\u001b\][^\u0007\u001b]*(?:\u0007|\u001b\\)/g;
function plainText(message) {
  return message.replace(ANSI, "");
}

// srcts/modules/dock/card/header.tsx
import { useCallback as useCallback3, useId as useId2, useLayoutEffect as useLayoutEffect4, useRef as useRef4, useState as useState3 } from "react";

// srcts/modules/dock/menu.tsx
import { Fragment, useCallback, useEffect, useId, useLayoutEffect as useLayoutEffect3, useMemo, useRef as useRef2, useState } from "react";

// srcts/modules/dock/floating.ts
import { useLayoutEffect as useLayoutEffect2 } from "react";
function usePopover(ref2, source) {
  useLayoutEffect2(() => {
    const el = ref2.current;
    if (el === null) return;
    el.showPopover({ source });
    return () => {
      try {
        el.hidePopover();
      } catch {
      }
    };
  }, [ref2, source]);
}
function useFloating(ref2, anchor, placement) {
  useLayoutEffect2(() => {
    const el = ref2.current;
    if (el === null || placement === null) return;
    const doc = el.ownerDocument;
    const win = doc.defaultView ?? window;
    const place = () => {
      const view2 = { width: win.innerWidth, height: win.innerHeight };
      const spot = placement(anchor.getBoundingClientRect(), el.getBoundingClientRect(), view2);
      el.style.left = `${String(Math.round(spot.left))}px`;
      el.style.top = `${String(Math.round(spot.top))}px`;
    };
    place();
    const resizes = typeof win.ResizeObserver === "function" ? new win.ResizeObserver(place) : void 0;
    resizes?.observe(el);
    win.addEventListener("resize", place);
    doc.addEventListener("scroll", place, true);
    return () => {
      resizes?.disconnect();
      win.removeEventListener("resize", place);
      doc.removeEventListener("scroll", place, true);
    };
  }, [ref2, anchor, placement]);
}

// srcts/modules/dock/icons.tsx
import { jsx as jsx3, jsxs } from "react/jsx-runtime";
function Chevron({ right = false }) {
  return /* @__PURE__ */ jsx3(
    "svg",
    {
      className: "blockr-dock-chevron",
      width: "12",
      height: "12",
      viewBox: "0 0 12 12",
      fill: "none",
      stroke: "currentColor",
      strokeWidth: "1.4",
      strokeLinecap: "round",
      strokeLinejoin: "round",
      "aria-hidden": "true",
      focusable: "false",
      children: /* @__PURE__ */ jsx3("polyline", { points: right ? "4.5 3 7.5 6 4.5 9" : "3 4.5 6 7.5 9 4.5", vectorEffect: "non-scaling-stroke" })
    }
  );
}
function WarningIcon() {
  return /* @__PURE__ */ jsxs(
    "svg",
    {
      width: "14",
      height: "14",
      viewBox: "0 0 16 16",
      fill: "none",
      stroke: "currentColor",
      strokeWidth: "1.25",
      strokeLinecap: "round",
      strokeLinejoin: "round",
      "aria-hidden": "true",
      focusable: "false",
      children: [
        /* @__PURE__ */ jsx3("path", { d: "M8 2.25l6.25 11H1.75z" }),
        /* @__PURE__ */ jsx3("path", { d: "M8 5.75v2.5" }),
        /* @__PURE__ */ jsx3("circle", { cx: "8", cy: "10.75", r: "0.75", fill: "currentColor", stroke: "none" })
      ]
    }
  );
}
function LockIcon({ size = 12 }) {
  return /* @__PURE__ */ jsx3("svg", { width: size, height: size, viewBox: "0 0 16 16", fill: "currentColor", "aria-hidden": "true", focusable: "false", children: /* @__PURE__ */ jsx3("path", { d: "M8 1a3 3 0 0 0-3 3v3H4.5A1.5 1.5 0 0 0 3 8.5v5A1.5 1.5 0 0 0 4.5 15h7a1.5 1.5 0 0 0 1.5-1.5v-5A1.5 1.5 0 0 0 11.5 7H11V4a3 3 0 0 0-3-3Zm-1.75 6V4a1.75 1.75 0 1 1 3.5 0v3h-3.5Z" }) });
}
function CheckIcon() {
  return /* @__PURE__ */ jsx3(
    "svg",
    {
      className: "blockr-dock-check",
      width: "14",
      height: "14",
      viewBox: "0 0 16 16",
      fill: "none",
      stroke: "currentColor",
      strokeWidth: "1.5",
      strokeLinecap: "round",
      strokeLinejoin: "round",
      "aria-hidden": "true",
      focusable: "false",
      children: /* @__PURE__ */ jsx3("path", { d: "M3.5 8.5l3 3 6-7" })
    }
  );
}
function GripIcon() {
  return /* @__PURE__ */ jsxs("svg", { width: "8", height: "12", viewBox: "0 0 8 12", fill: "currentColor", "aria-hidden": "true", focusable: "false", children: [
    /* @__PURE__ */ jsx3("circle", { cx: "2", cy: "2", r: "1" }),
    /* @__PURE__ */ jsx3("circle", { cx: "6", cy: "2", r: "1" }),
    /* @__PURE__ */ jsx3("circle", { cx: "2", cy: "6", r: "1" }),
    /* @__PURE__ */ jsx3("circle", { cx: "6", cy: "6", r: "1" }),
    /* @__PURE__ */ jsx3("circle", { cx: "2", cy: "10", r: "1" }),
    /* @__PURE__ */ jsx3("circle", { cx: "6", cy: "10", r: "1" })
  ] });
}
function PlusIcon() {
  return /* @__PURE__ */ jsx3(
    "svg",
    {
      width: "14",
      height: "14",
      viewBox: "0 0 16 16",
      fill: "none",
      stroke: "currentColor",
      strokeWidth: "1.25",
      strokeLinecap: "round",
      "aria-hidden": "true",
      focusable: "false",
      children: /* @__PURE__ */ jsx3("path", { d: "M8 3v10M3 8h10" })
    }
  );
}
function RemoveIcon() {
  return /* @__PURE__ */ jsx3("svg", { width: "10", height: "10", viewBox: "0 0 10 10", fill: "none", stroke: "currentColor", strokeWidth: "1", strokeLinecap: "round", "aria-hidden": "true", focusable: "false", children: /* @__PURE__ */ jsx3("path", { d: "M2.5 2.5l5 5M7.5 2.5l-5 5" }) });
}
function SlidersIcon() {
  return /* @__PURE__ */ jsxs(
    "svg",
    {
      width: "14",
      height: "14",
      viewBox: "0 0 16 16",
      fill: "none",
      stroke: "currentColor",
      strokeWidth: "1.25",
      strokeLinecap: "round",
      "aria-hidden": "true",
      focusable: "false",
      children: [
        /* @__PURE__ */ jsx3("path", { d: "M2 4h12M2 8h12M2 12h12" }),
        /* @__PURE__ */ jsx3("circle", { cx: "5", cy: "4", r: "1.6", fill: "var(--blockr-dock-bg-raised)" }),
        /* @__PURE__ */ jsx3("circle", { cx: "10", cy: "8", r: "1.6", fill: "var(--blockr-dock-bg-raised)" }),
        /* @__PURE__ */ jsx3("circle", { cx: "6", cy: "12", r: "1.6", fill: "var(--blockr-dock-bg-raised)" })
      ]
    }
  );
}
function PinIcon() {
  return /* @__PURE__ */ jsx3(
    "svg",
    {
      width: "12",
      height: "12",
      viewBox: "0 0 16 16",
      fill: "none",
      stroke: "currentColor",
      strokeWidth: "1.4",
      strokeLinecap: "round",
      strokeLinejoin: "round",
      "aria-hidden": "true",
      focusable: "false",
      children: /* @__PURE__ */ jsx3("path", { d: "M5.5 2h5M6.5 2v4.25L4.5 9h7l-2-2.75V2M8 9v5", vectorEffect: "non-scaling-stroke" })
    }
  );
}

// srcts/modules/dock/place.ts
var EDGE = 8;
var MENU_GAP = 4;
var TOOLTIP_GAP = 6;
function clamp(value, low, high) {
  return Math.max(low, Math.min(value, high));
}
function menuSpot(trigger, menu, view2, align = "start") {
  const below = trigger.bottom + MENU_GAP;
  const above = trigger.top - MENU_GAP - menu.height;
  const top = below + menu.height > view2.height - EDGE && above >= EDGE ? above : below;
  const left = align === "end" ? trigger.right - menu.width : trigger.left;
  return {
    left: clamp(left, EDGE, view2.width - EDGE - menu.width),
    top: clamp(top, EDGE, view2.height - EDGE - menu.height)
  };
}
function besideSpot(row, menu, view2) {
  const right = row.right + MENU_GAP;
  const left = row.left - MENU_GAP - menu.width;
  const x = right + menu.width > view2.width - EDGE && left >= EDGE ? left : right;
  return {
    left: clamp(x, EDGE, view2.width - EDGE - menu.width),
    top: clamp(row.top, EDGE, view2.height - EDGE - menu.height)
  };
}
function tooltipSpot(trigger, tooltip, view2) {
  const above = trigger.top - TOOLTIP_GAP - tooltip.height;
  return {
    left: clamp(trigger.left + trigger.width / 2 - tooltip.width / 2, EDGE, view2.width - EDGE - tooltip.width),
    top: above < EDGE ? trigger.bottom + TOOLTIP_GAP : above
  };
}

// srcts/modules/dock/errors.ts
function errorMessage(error) {
  return error instanceof Error ? error.message : String(error);
}
function asError(error) {
  return error instanceof Error ? error : new Error(errorMessage(error));
}

// srcts/modules/dock/storage.ts
var KINDS = /* @__PURE__ */ new Set(["button", "menu", "status"]);
function drawnControls(core) {
  const storage = core.plugins.storage();
  if (storage === void 0) return void 0;
  const plugin = Object.keys(core.plugins.appPlugins()).find((id) => core.plugins.pluginClass(id).storage === storage);
  if (plugin === void 0) return void 0;
  const twice = /* @__PURE__ */ new Set();
  let controls;
  try {
    controls = distinct(storage.controls({ core, plugin }), twice);
  } catch (error) {
    const failed = errorMessage(error);
    return { failed, package: packageFromId(plugin), problem: `The storage controls of ${plugin} failed: ${failed}` };
  }
  if (twice.size === 0) return { controls };
  const ids = [...twice].map((id) => JSON.stringify(id)).join(", ");
  return { controls, problem: `Storage controls of one list share the ids ${ids}, so only the first of each draws.` };
}
function allButtons(controls) {
  return controls.flatMap((control) => {
    if (control.kind === "button") return [control];
    if (control.kind === "menu") return allButtons(control.items);
    return [];
  });
}
function barButtons(core, items) {
  const drawn = drawnControls(core);
  if (drawn === void 0 || drawn.failed !== void 0) return [];
  const all = items.some((item) => item.kind === "part" && item.id === "storage");
  const apart = new Set(items.flatMap((item) => item.kind === "storage" ? [item.id] : []));
  return allButtons(drawn.controls.filter((control) => all || apart.has(control.id)));
}
async function runButton(core, button) {
  try {
    await button.run();
  } catch (error) {
    core.log("blockr.dock").error(asError(error));
  }
}
function packageFromId(id) {
  const at2 = id.indexOf("-");
  return at2 < 0 ? id : id.slice(0, at2);
}
function distinct(list, twice) {
  const seen = /* @__PURE__ */ new Set();
  const kept = [];
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

// srcts/modules/dock/shortcuts.ts
function bindShortcuts(doc, core, buttons) {
  function onKey(event) {
    if (event.defaultPrevented || editable2(event.target)) return;
    const step = undoOrRedo(event);
    if (step !== void 0) {
      event.preventDefault();
      try {
        if (step === "undo") core.store.undo();
        else core.store.redo();
      } catch {
      }
      return;
    }
    const button = buttons().find(
      (b) => b.disabled !== true && b.shortcut !== void 0 && matchesShortcut(b.shortcut, event)
    );
    if (button === void 0) return;
    event.preventDefault();
    if (!event.repeat) void runButton(core, button);
  }
  doc.addEventListener("keydown", onKey);
  return () => {
    doc.removeEventListener("keydown", onKey);
  };
}
function matchesShortcut(text2, event) {
  const shortcut = readShortcut(text2);
  const key = event.key;
  if (shortcut === void 0 || typeof key !== "string" || key.toLowerCase() !== shortcut.key.toLowerCase()) {
    return false;
  }
  if (event.altKey !== shortcut.alt || event.shiftKey !== shortcut.shift) return false;
  if (shortcut.mod) return event.ctrlKey || event.metaKey;
  return event.ctrlKey === shortcut.ctrl && event.metaKey === shortcut.meta;
}
function ariaShortcuts(text2) {
  const shortcut = text2 === void 0 ? void 0 : readShortcut(text2);
  if (shortcut === void 0) return void 0;
  const rest = [...shortcut.alt ? ["Alt"] : [], ...shortcut.shift ? ["Shift"] : [], shortcut.key];
  const firsts = shortcut.mod ? [["Control"], ["Meta"]] : [[...shortcut.ctrl ? ["Control"] : [], ...shortcut.meta ? ["Meta"] : []]];
  return firsts.map((first) => [...first, ...rest].join("+")).join(" ");
}
function shortcutLabel(text2, mac) {
  const shortcut = readShortcut(text2);
  if (shortcut === void 0) return text2;
  const key = shortcut.key.length === 1 ? shortcut.key.toUpperCase() : shortcut.key;
  if (mac) {
    const glyphs = [
      ...shortcut.ctrl ? ["\u2303"] : [],
      ...shortcut.alt ? ["\u2325"] : [],
      ...shortcut.mod || shortcut.meta ? ["\u2318"] : [],
      ...shortcut.shift ? ["\u21E7"] : []
    ];
    return [...glyphs, key].join("");
  }
  const words = [
    ...shortcut.mod || shortcut.ctrl ? ["Ctrl"] : [],
    ...shortcut.alt ? ["Alt"] : [],
    ...shortcut.shift ? ["Shift"] : [],
    ...shortcut.meta ? ["Meta"] : []
  ];
  return [...words, key].join("+");
}
function onMac(win) {
  const nav = win.navigator;
  return /mac|iphone|ipad/i.test(nav.userAgentData?.platform ?? nav.platform);
}
var MODIFIERS = ["mod", "ctrl", "alt", "shift", "meta"];
function readShortcut(text2) {
  const at2 = text2.length < 2 ? -1 : text2.lastIndexOf("+", text2.length - 2);
  const key = text2.slice(at2 + 1);
  const names = at2 < 0 ? [] : text2.slice(0, at2).toLowerCase().split("+");
  if (key === "" || new Set(names).size < names.length || !names.every((name2) => MODIFIERS.includes(name2))) {
    return void 0;
  }
  const has = (name2) => names.includes(name2);
  if (has("mod") && (has("ctrl") || has("meta"))) return void 0;
  return { key, mod: has("mod"), ctrl: has("ctrl"), alt: has("alt"), shift: has("shift"), meta: has("meta") };
}
function undoOrRedo(event) {
  if (!(event.ctrlKey || event.metaKey) || event.altKey) return void 0;
  const key = event.key.toLowerCase();
  if (key === "z") return event.shiftKey ? "redo" : "undo";
  if (key === "y" && !event.shiftKey) return "redo";
  return void 0;
}
function editable2(target) {
  if (!(target instanceof HTMLElement)) return false;
  return target.isContentEditable || ["INPUT", "TEXTAREA", "SELECT"].includes(target.tagName);
}

// srcts/modules/dock/menu.tsx
import { Fragment as Fragment2, jsx as jsx4, jsxs as jsxs2 } from "react/jsx-runtime";
function matches(row, searched) {
  const words = searched.toLowerCase().split(/\s+/).filter((word) => word !== "");
  const text2 = [row.label, row.detail ?? "", row.search ?? ""].join(" ").toLowerCase();
  return words.every((word) => text2.includes(word));
}
function stepped(at2, step, count2) {
  if (at2 === null) return step === 1 ? 0 : count2 - 1;
  return (at2 + step + count2) % count2;
}
function foldOf(rows, at2) {
  const depth = rows[at2]?.depth ?? 0;
  for (let i = at2 - 1; i >= 0 && depth > 0; i -= 1) {
    if ((rows[i]?.depth ?? 0) < depth) return i;
  }
  return -1;
}
function anchored(doc) {
  return doc.defaultView?.CSS.supports("position-area", "block-end") === true;
}
function anchorName(depth) {
  return `--blockr-dock-menu-${String(depth)}`;
}
var reachable = (row) => row.status !== true;
function useDismiss(menu, anchor, close) {
  useEffect(() => {
    const doc = anchor.ownerDocument;
    const outside = (target) => menu.current !== null && !(target instanceof Node && (anchor.contains(target) || menu.current.contains(target)));
    const onPointerDown = (event) => {
      if (outside(event.target)) close({ restore: false, pointer: true });
    };
    const onFocusIn = (event) => {
      if (outside(event.target)) close({ restore: false, pointer: false });
    };
    doc.addEventListener("pointerdown", onPointerDown, true);
    doc.addEventListener("focusin", onFocusIn);
    return () => {
      doc.removeEventListener("pointerdown", onPointerDown, true);
      doc.removeEventListener("focusin", onFocusIn);
    };
  }, [menu, anchor, close]);
}
function useMenuSurface(ref2, anchor, { depth, align, keyboard, onClose }) {
  const [placement] = useState(() => {
    if (anchored(anchor.ownerDocument)) return null;
    return depth === 0 ? (trigger, size, view2) => menuSpot(trigger, size, view2, align) : besideSpot;
  });
  useLayoutEffect3(() => {
    const name2 = anchorName(depth);
    if (depth === 0 && !keyboard) anchor.focus({ preventScroll: true, focusVisible: false });
    anchor.style.setProperty("anchor-name", name2);
    ref2.current?.style.setProperty("position-anchor", name2);
    return () => {
      anchor.style.removeProperty("anchor-name");
    };
  }, [ref2, anchor, keyboard, depth]);
  usePopover(ref2, anchor);
  useFloating(ref2, anchor, placement);
  useDismiss(ref2, anchor, onClose);
}
function Menu({
  anchor,
  label,
  align = "start",
  caption,
  search,
  groups,
  note,
  keyboard,
  depth = 0,
  onClose,
  onCloseAll
}) {
  const ref2 = useRef2(null);
  const field = useRef2(null);
  const id = useId();
  const closeAll = onCloseAll ?? onClose;
  const [searched, setSearched] = useState("");
  const [active, setActive] = useState(() => {
    if (search !== void 0) return 0;
    if (!keyboard) return null;
    return Math.max(
      0,
      groups.flatMap((group) => group.rows).filter(reachable).findIndex((row) => row.current === true)
    );
  });
  const [sub, setSub] = useState(null);
  const listed = groups.some((group) => group.rows.length > 0);
  const shown = useMemo(
    () => groups.map((group) => ({ ...group, rows: group.rows.filter((row) => matches(row, searched)) })).filter((group) => group.rows.length > 0),
    [groups, searched]
  );
  const rows = shown.flatMap((group) => group.rows).filter(reachable);
  const rowId = (i) => `${id}-row-${String(i)}`;
  const activeId = active !== null && active < rows.length ? rowId(active) : void 0;
  useMenuSurface(ref2, anchor, { depth, align, keyboard, onClose });
  useLayoutEffect3(() => {
    (field.current ?? ref2.current)?.focus({ preventScroll: true });
  }, []);
  useLayoutEffect3(() => {
    if (activeId !== void 0) ref2.current?.querySelector(`[id="${activeId}"]`)?.scrollIntoView({ block: "nearest" });
  }, [activeId]);
  const closeSub = useCallback((how) => {
    setSub(null);
    if (how.restore) ref2.current?.focus({ preventScroll: true });
  }, []);
  const rowElement = (i) => ref2.current?.querySelector(`[id="${rowId(i)}"]`) ?? null;
  const pick = (row, i, pointer2) => {
    if (row.disabled === true) return;
    if (row.fold !== void 0) {
      setActive(i);
      row.fold.onToggle(!row.fold.open);
      return;
    }
    if (row.rows !== void 0) {
      const element = rowElement(i);
      if (element !== null) {
        setActive(i);
        setSub({ key: row.key, row: element, keyboard: !pointer2 });
      }
      return;
    }
    if (row.href !== void 0) {
      rowElement(i)?.click();
      return;
    }
    closeAll({ restore: true, pointer: pointer2 });
    row.onSelect?.();
  };
  const onKeyDown = (event) => {
    if (event.nativeEvent.isComposing) return;
    const row = active === null ? void 0 : rows[active];
    switch (event.key) {
      case "ArrowDown":
      case "ArrowUp":
        if (rows.length > 0) setActive(stepped(active, event.key === "ArrowDown" ? 1 : -1, rows.length));
        break;
      case "Home":
      case "End":
        if (search !== void 0) return;
        setActive(event.key === "Home" ? 0 : rows.length - 1);
        break;
      case "ArrowRight":
        if (search !== void 0 || row === void 0 || active === null) return;
        if (row.fold !== void 0) {
          if (!row.fold.open) row.fold.onToggle(true);
          else if ((rows[active + 1]?.depth ?? 0) > (row.depth ?? 0)) setActive(active + 1);
          break;
        }
        if (row.rows === void 0) return;
        pick(row, active, false);
        break;
      case "ArrowLeft": {
        if (search !== void 0) return;
        const fold2 = active === null ? -1 : foldOf(rows, active);
        if (row?.fold?.open === true) row.fold.onToggle(false);
        else if (fold2 >= 0) setActive(fold2);
        else if (depth === 0) return;
        else onClose({ restore: true, pointer: false });
        break;
      }
      case " ":
        if (search !== void 0) return;
        if (row !== void 0 && active !== null) pick(row, active, false);
        break;
      case "Enter":
        if (row !== void 0 && active !== null) pick(row, active, false);
        break;
      case "Escape":
        onClose({ restore: true, pointer: false });
        break;
      case "Tab":
        closeAll({ restore: true, pointer: false });
        return;
      default:
        return;
    }
    event.preventDefault();
    event.stopPropagation();
  };
  const starts = shown.map(
    (_, g) => shown.slice(0, g).reduce((count2, group) => count2 + group.rows.filter(reachable).length, 0)
  );
  const drawRow = (row, i, folds) => {
    const common2 = {
      id: rowId(i),
      className: "blockr-dock-menu-row",
      style: row.depth === void 0 ? void 0 : { "--depth": row.depth },
      "data-active": i === active ? "" : void 0,
      "data-danger": row.danger === true ? "" : void 0,
      title: row.tooltip,
      // The focus stays in the menu.
      onPointerDown: (event) => {
        event.preventDefault();
      },
      onPointerMove: () => {
        if (active !== i) setActive(i);
      }
    };
    const content = /* @__PURE__ */ jsxs2(Fragment2, { children: [
      row.fold !== void 0 ? /* @__PURE__ */ jsx4("span", { className: "blockr-dock-menu-fold", "aria-hidden": "true", children: /* @__PURE__ */ jsx4(Chevron, {}) }) : folds ? /* @__PURE__ */ jsx4("span", { className: "blockr-dock-menu-fold", "aria-hidden": "true" }) : null,
      row.lead === void 0 ? null : /* @__PURE__ */ jsx4("span", { className: "blockr-dock-menu-lead", "aria-hidden": "true", children: row.lead }),
      /* @__PURE__ */ jsx4("span", { className: "blockr-dock-menu-label", children: row.label }),
      row.detail === void 0 ? null : /* @__PURE__ */ jsx4("span", { className: "blockr-dock-menu-detail", children: row.detail }),
      row.badge === void 0 ? null : /* @__PURE__ */ jsx4("span", { className: "blockr-dock-menu-badge", children: row.badge }),
      row.meta === void 0 ? null : /* @__PURE__ */ jsx4("span", { className: "blockr-dock-menu-meta", children: row.meta }),
      row.checked === true ? /* @__PURE__ */ jsx4(CheckIcon, {}) : null
    ] });
    if (search !== void 0) {
      return /* @__PURE__ */ jsx4(
        "div",
        {
          ...common2,
          role: "option",
          "aria-selected": i === active,
          onClick: (event) => {
            pick(row, i, event.detail !== 0);
          },
          children: content
        }
      );
    }
    if (row.href !== void 0) {
      return /* @__PURE__ */ jsx4(
        "a",
        {
          ...common2,
          role: "menuitem",
          tabIndex: -1,
          href: row.href,
          target: "_blank",
          rel: "noopener noreferrer",
          onClick: (event) => {
            const pointer2 = event.detail !== 0;
            setTimeout(() => {
              closeAll({ restore: true, pointer: pointer2 });
            }, 0);
          },
          children: content
        }
      );
    }
    if (row.rows !== void 0) {
      const open = sub?.key === row.key;
      return /* @__PURE__ */ jsxs2("div", { role: "none", children: [
        /* @__PURE__ */ jsxs2(
          "button",
          {
            ...common2,
            type: "button",
            role: "menuitem",
            tabIndex: -1,
            "aria-haspopup": "menu",
            "aria-expanded": open,
            onClick: (event) => {
              if (open) closeSub({ restore: false, pointer: true });
              else pick(row, i, event.detail !== 0);
            },
            children: [
              content,
              /* @__PURE__ */ jsx4(Chevron, { right: true })
            ]
          }
        ),
        open ? /* @__PURE__ */ jsx4(
          Menu,
          {
            anchor: sub.row,
            label: row.label,
            groups: [{ key: row.key, rows: row.rows }],
            keyboard: sub.keyboard,
            depth: depth + 1,
            onClose: closeSub,
            onCloseAll: closeAll
          }
        ) : null
      ] });
    }
    return /* @__PURE__ */ jsx4(
      "button",
      {
        ...common2,
        type: "button",
        role: row.current !== void 0 ? "menuitemradio" : row.checked !== void 0 ? "menuitemcheckbox" : "menuitem",
        tabIndex: -1,
        "aria-checked": row.current ?? row.checked,
        "aria-expanded": row.fold?.open,
        "aria-disabled": row.disabled === true ? true : void 0,
        "aria-keyshortcuts": ariaShortcuts(row.shortcut),
        onClick: (event) => {
          pick(row, i, event.detail !== 0);
        },
        children: content
      }
    );
  };
  const drawGroup = (group, g) => {
    const folds = group.rows.some((row) => row.fold !== void 0);
    const drawn = group.rows.map((row, r) => {
      const element = row.status === true ? /* @__PURE__ */ jsx4("div", { role: "none", className: "blockr-dock-menu-status", title: row.tooltip, children: row.label }) : drawRow(row, (starts[g] ?? 0) + group.rows.slice(0, r).filter(reachable).length, folds);
      return /* @__PURE__ */ jsxs2(Fragment, { children: [
        row.divided === true && search === void 0 ? /* @__PURE__ */ jsx4("hr", { className: "blockr-dock-menu-divider" }) : null,
        element
      ] }, row.key);
    });
    return group.title === void 0 ? /* @__PURE__ */ jsx4(Fragment, { children: drawn }, group.key) : /* @__PURE__ */ jsxs2("div", { role: "group", "aria-labelledby": `${id}-${group.key}`, children: [
      /* @__PURE__ */ jsx4("div", { id: `${id}-${group.key}`, role: "presentation", className: "blockr-dock-menu-title", children: group.title }),
      drawn
    ] }, group.key);
  };
  return /* @__PURE__ */ jsxs2(
    "div",
    {
      ref: ref2,
      popover: "auto",
      className: "blockr-dock-menu",
      "data-align": align,
      "data-depth": depth,
      tabIndex: -1,
      onKeyDown,
      onToggle: (event) => {
        if (event.newState === "closed") onClose({ restore: false, pointer: false });
      },
      ...search === void 0 && listed ? { role: "menu", "aria-label": label, "aria-activedescendant": activeId } : {},
      children: [
        caption === void 0 ? null : /* @__PURE__ */ jsx4("p", { className: "blockr-dock-menu-caption", children: caption }),
        !listed ? /* @__PURE__ */ jsx4("p", { className: "blockr-dock-menu-note", children: note }) : search === void 0 ? shown.map(drawGroup) : /* @__PURE__ */ jsxs2(Fragment2, { children: [
          /* @__PURE__ */ jsx4("div", { className: "blockr-dock-menu-search", children: /* @__PURE__ */ jsx4(
            "input",
            {
              ref: field,
              type: "search",
              role: "combobox",
              "aria-label": search.label,
              "aria-expanded": "true",
              "aria-controls": `${id}-list`,
              "aria-autocomplete": "list",
              "aria-activedescendant": activeId,
              placeholder: search.label,
              autoComplete: "off",
              spellCheck: false,
              value: searched,
              onChange: (event) => {
                setSearched(event.target.value);
                setActive(0);
              }
            }
          ) }),
          /* @__PURE__ */ jsx4("div", { id: `${id}-list`, role: "listbox", "aria-label": label, children: shown.map(drawGroup) }),
          rows.length === 0 ? /* @__PURE__ */ jsx4("p", { className: "blockr-dock-menu-note", children: search.empty }) : null
        ] })
      ]
    }
  );
}
function useMenuButton(open, show, close) {
  const pressedOpen = useRef2(false);
  return {
    "aria-haspopup": "menu",
    "aria-expanded": open,
    onPointerDown: () => {
      pressedOpen.current = open;
    },
    onClick: (event) => {
      const keyboard = event.detail === 0;
      const wasOpen = open || !keyboard && pressedOpen.current;
      pressedOpen.current = false;
      if (wasOpen) close();
      else show(event.currentTarget, keyboard);
    },
    onKeyDown: (event) => {
      if (event.key !== "ArrowDown" || open) return;
      event.preventDefault();
      show(event.currentTarget, true);
    }
  };
}
function entryRows(entries) {
  return entries.map((entry) => {
    switch (entry.kind) {
      case "link":
        return { key: entry.id, label: entry.label, href: entry.href };
      case "button":
        return {
          key: entry.id,
          label: entry.label,
          ...entry.tooltip === void 0 ? {} : { tooltip: entry.tooltip },
          ...entry.shortcut === void 0 ? {} : { shortcut: entry.shortcut },
          ...entry.disabled === true ? { disabled: true } : {},
          onSelect: entry.run
        };
      case "status":
        return { key: entry.id, label: entry.text, status: true, ...entry.tooltip === void 0 ? {} : { tooltip: entry.tooltip } };
      case "menu":
        return { key: entry.id, label: entry.label, rows: entryRows(entry.entries) };
    }
  });
}
function MenuButton({ label, entries, className, onTip }) {
  const [open, setOpen] = useState(null);
  const button = useRef2(null);
  const props = useMenuButton(
    open !== null,
    (anchor, keyboard) => {
      setOpen({ anchor, keyboard });
    },
    () => {
      setOpen(null);
    }
  );
  const close = useCallback((how) => {
    setOpen(null);
    if (how.restore) button.current?.focus({ preventScroll: true, focusVisible: !how.pointer });
  }, []);
  const tip = (event) => {
    onTip?.(event.currentTarget);
  };
  return /* @__PURE__ */ jsxs2(Fragment2, { children: [
    /* @__PURE__ */ jsxs2("button", { ref: button, type: "button", className, ...props, onPointerEnter: tip, onFocus: tip, children: [
      /* @__PURE__ */ jsx4("span", { className: "blockr-dock-bar-label", children: label }),
      /* @__PURE__ */ jsx4(Chevron, {})
    ] }),
    open === null ? null : /* @__PURE__ */ jsx4(
      Menu,
      {
        anchor: open.anchor,
        label,
        groups: [{ key: "entries", rows: entryRows(entries) }],
        keyboard: open.keyboard,
        onClose: close
      }
    )
  ] });
}

// srcts/modules/dock/card/mark.tsx
import { jsx as jsx5 } from "react/jsx-runtime";
var UNCATEGORIZED = "uncategorized";
function categoryOf(entry) {
  return entry === void 0 || entry.category === "" ? UNCATEGORIZED : entry.category;
}
function packageOf(id) {
  const at2 = id.indexOf("-");
  return at2 > 0 ? id.slice(0, at2) : null;
}
function Mark({
  entry,
  size,
  ...rest
}) {
  return /* @__PURE__ */ jsx5(
    "span",
    {
      ...rest,
      className: "blockr-card-mark",
      "data-size": size,
      "data-category": categoryOf(entry),
      dangerouslySetInnerHTML: { __html: entry?.icon ?? "" }
    }
  );
}

// srcts/modules/dock/card/classes.tsx
import { jsx as jsx6 } from "react/jsx-runtime";
var UNCATEGORIZED2 = categoryOf(void 0);
function classGroups(entries, pick) {
  const groups = /* @__PURE__ */ new Map();
  for (const entry of entries) {
    const category = categoryOf(entry);
    groups.set(category, [...groups.get(category) ?? [], entry]);
  }
  const last = (category) => category === UNCATEGORIZED2 ? 1 : 0;
  return [...groups].sort(([a], [b]) => last(a) - last(b) || a.localeCompare(b)).map(([category, list]) => ({
    key: category,
    title: category,
    rows: list.sort((a, b) => a.name.localeCompare(b.name)).map((entry) => {
      const pkg = packageOf(entry.id);
      return {
        key: entry.id,
        label: entry.name,
        lead: /* @__PURE__ */ jsx6(Mark, { entry, size: 24 }),
        ...pkg === null ? {} : { badge: pkg },
        search: [entry.id, entry.category, ...entry.keywords].join(" "),
        onSelect: () => {
          pick(entry);
        }
      };
    })
  }));
}

// srcts/modules/dock/card/clipboard.ts
async function copyText(doc, text2) {
  const { clipboard } = doc.defaultView?.navigator ?? {};
  if (clipboard === void 0) return copyCommand(doc, text2);
  try {
    await clipboard.writeText(text2);
    return true;
  } catch {
    return copyCommand(doc, text2);
  }
}
function copyCommand(doc, text2) {
  const before2 = doc.activeElement;
  const field = doc.createElement("textarea");
  field.value = text2;
  field.setAttribute("readonly", "");
  field.style.cssText = "position: fixed; inset-block-start: 0; opacity: 0;";
  doc.body.append(field);
  field.select();
  try {
    return doc.execCommand("copy");
  } catch {
    return false;
  } finally {
    field.remove();
    if (before2 instanceof HTMLElement) before2.focus({ preventScroll: true });
  }
}

// srcts/modules/dock/card/icons.tsx
import { jsx as jsx7, jsxs as jsxs3 } from "react/jsx-runtime";
function Icon({ width = 1.25, children: children2 }) {
  return /* @__PURE__ */ jsx7(
    "svg",
    {
      width: "14",
      height: "14",
      viewBox: "0 0 16 16",
      fill: "none",
      stroke: "currentColor",
      strokeWidth: width,
      strokeLinecap: "round",
      strokeLinejoin: "round",
      "aria-hidden": "true",
      children: children2
    }
  );
}
function SlidersIcon2() {
  return /* @__PURE__ */ jsxs3(Icon, { children: [
    /* @__PURE__ */ jsx7("path", { d: "M2 4h12M2 8h12M2 12h12" }),
    /* @__PURE__ */ jsx7("circle", { className: "blockr-card-icon-knob", cx: "5", cy: "4", r: "1.6" }),
    /* @__PURE__ */ jsx7("circle", { className: "blockr-card-icon-knob", cx: "10", cy: "8", r: "1.6" }),
    /* @__PURE__ */ jsx7("circle", { className: "blockr-card-icon-knob", cx: "6", cy: "12", r: "1.6" })
  ] });
}
function EyeIcon() {
  return /* @__PURE__ */ jsxs3(Icon, { width: 1.4, children: [
    /* @__PURE__ */ jsx7("path", { d: "M1.5 8S4 3.5 8 3.5 14.5 8 14.5 8 12 12.5 8 12.5 1.5 8 1.5 8z" }),
    /* @__PURE__ */ jsx7("circle", { cx: "8", cy: "8", r: "2" })
  ] });
}
function LockIcon2({ open }) {
  return /* @__PURE__ */ jsxs3(Icon, { children: [
    /* @__PURE__ */ jsx7("rect", { x: "3.5", y: "7", width: "9", height: "6.5", rx: "1.5" }),
    /* @__PURE__ */ jsx7("path", { d: open ? "M5.5 7V5a2.5 2.5 0 0 1 4.67-1.25" : "M5.5 7V5a2.5 2.5 0 0 1 5 0v2" })
  ] });
}
function DotsIcon() {
  return /* @__PURE__ */ jsxs3("svg", { width: "14", height: "14", viewBox: "0 0 16 16", fill: "currentColor", "aria-hidden": "true", children: [
    /* @__PURE__ */ jsx7("circle", { cx: "3.5", cy: "8", r: "1.2" }),
    /* @__PURE__ */ jsx7("circle", { cx: "8", cy: "8", r: "1.2" }),
    /* @__PURE__ */ jsx7("circle", { cx: "12.5", cy: "8", r: "1.2" })
  ] });
}
function BinIcon() {
  return /* @__PURE__ */ jsx7(Icon, { children: /* @__PURE__ */ jsx7("path", { d: "M2.5 4.5h11M6.5 4.5V3h3v1.5M4 4.5l.7 9h6.6l.7-9" }) });
}

// srcts/modules/dock/card/tooltip.tsx
import { useCallback as useCallback2, useEffect as useEffect2, useRef as useRef3, useState as useState2 } from "react";
import { Fragment as Fragment3, jsx as jsx8, jsxs as jsxs4 } from "react/jsx-runtime";
var DELAY = 300;
var WARM = 400;
function createTooltips() {
  let current;
  let warmUntil = 0;
  return {
    showing(hide) {
      if (current !== void 0 && current !== hide) current();
      current = hide;
    },
    left() {
      warmUntil = Date.now() + WARM;
    },
    warm: () => Date.now() < warmUntil
  };
}
function useTooltip(source) {
  const { tooltips } = useDock();
  const [shown, setShown] = useState2(null);
  const timer = useRef3(void 0);
  const showing2 = useRef3(false);
  const hide = useCallback2(() => {
    clearTimeout(timer.current);
    timer.current = void 0;
    if (!showing2.current) return;
    showing2.current = false;
    tooltips.left();
    setShown(null);
  }, [tooltips]);
  const show = (el) => {
    const tip = typeof source === "function" ? source(el) : source;
    if (tip === null || tip.length === 0 || !el.isConnected) return;
    tooltips.showing(hide);
    showing2.current = true;
    setShown({ el, tip });
  };
  useEffect2(() => {
    if (shown === null) return;
    const doc = shown.el.ownerDocument;
    const onKey = (event) => {
      if (event.key === "Escape") hide();
    };
    doc.addEventListener("keydown", onKey, true);
    doc.addEventListener("scroll", hide, true);
    return () => {
      doc.removeEventListener("keydown", onKey, true);
      doc.removeEventListener("scroll", hide, true);
    };
  }, [shown, hide]);
  useEffect2(() => hide, [hide]);
  const props = {
    onPointerEnter(event) {
      if (event.pointerType === "touch" || showing2.current) return;
      const el = event.currentTarget;
      clearTimeout(timer.current);
      if (tooltips.warm()) show(el);
      else
        timer.current = setTimeout(() => {
          show(el);
        }, DELAY);
    },
    onPointerLeave: hide,
    onPointerDown: hide,
    onFocus(event) {
      if (event.currentTarget.matches(":focus-visible")) show(event.currentTarget);
    },
    onBlur: hide
  };
  const tooltip = shown === null ? null : /* @__PURE__ */ jsx8(TooltipCard, { anchor: shown.el, tip: shown.tip });
  return { props, tooltip };
}
function TooltipCard({ anchor, tip }) {
  const ref2 = useRef3(null);
  usePopover(ref2, anchor);
  useFloating(ref2, anchor, tooltipSpot);
  const lines = typeof tip === "string" ? [{ text: tip }] : tip;
  return /* @__PURE__ */ jsx8("div", { ref: ref2, popover: "manual", role: "tooltip", className: "blockr-card-tooltip", children: lines.map((line, i) => /* @__PURE__ */ jsxs4("div", { className: "blockr-card-tooltip-line", "data-muted": line.muted === true ? "" : void 0, children: [
    line.text,
    line.badge === void 0 ? null : /* @__PURE__ */ jsxs4(Fragment3, { children: [
      " ",
      /* @__PURE__ */ jsx8("span", { className: "blockr-card-tooltip-badge", children: line.badge })
    ] })
  ] }, i)) });
}

// srcts/modules/dock/card/header.tsx
import { Fragment as Fragment4, jsx as jsx9, jsxs as jsxs5 } from "react/jsx-runtime";
var GESTURE = "Double-click to edit";
function Header({
  id,
  block,
  entry,
  sections,
  drawn,
  dot
}) {
  const { core } = useDock();
  const locked = core.lock.board;
  const [renaming, setRenaming] = useState3(false);
  const menuButton = useRef4(null);
  const open = block.locked === false;
  const toggle = (section) => {
    const shown = sections.includes(section) ? sections.filter((each) => each !== section) : [...sections, section];
    runEdit(core, "blockr.dock-set_sections", {
      block: id,
      sections: SECTIONS.filter((each) => shown.includes(each))
    });
  };
  const rename2 = () => {
    setRenaming(true);
  };
  return /* @__PURE__ */ jsxs5("header", { className: "blockr-card-header", children: [
    /* @__PURE__ */ jsx9(HeaderMark, { entry, block, dot }),
    /* @__PURE__ */ jsx9("div", { className: "blockr-card-title", children: renaming ? /* @__PURE__ */ jsx9(
      NameField,
      {
        name: block.name,
        onCommit: (name2) => {
          runEdit(core, "blockr.core-rename_block", { id, name: name2 });
        },
        onDone: (restore) => {
          setRenaming(false);
          if (restore) menuButton.current?.focus({ preventScroll: true });
        }
      }
    ) : /* @__PURE__ */ jsx9(Name, { name: block.name, editable: !locked, onRename: rename2 }) }),
    /* @__PURE__ */ jsxs5("div", { className: "blockr-card-tools", children: [
      locked ? null : /* @__PURE__ */ jsxs5(Fragment4, { children: [
        drawn ? /* @__PURE__ */ jsx9(
          Tool,
          {
            label: "Controls",
            pressed: sections.includes("inputs"),
            onClick: () => {
              toggle("inputs");
            },
            children: /* @__PURE__ */ jsx9(SlidersIcon2, {})
          }
        ) : null,
        /* @__PURE__ */ jsx9(
          Tool,
          {
            label: "Preview",
            pressed: sections.includes("outputs"),
            onClick: () => {
              toggle("outputs");
            },
            children: /* @__PURE__ */ jsx9(EyeIcon, {})
          }
        ),
        /* @__PURE__ */ jsx9(
          Tool,
          {
            label: open ? "Open to readers" : "Locked for readers",
            pressed: open,
            onClick: () => {
              runEdit(core, "blockr.core-set_block_lock", { id, locked: open ? null : false });
            },
            children: /* @__PURE__ */ jsx9(LockIcon2, { open })
          }
        )
      ] }),
      /* @__PURE__ */ jsx9(MenuButton2, { ref: menuButton, id, block, onRename: rename2 })
    ] })
  ] });
}
function HeaderMark({
  entry,
  block,
  dot
}) {
  const pkg = packageOf(block.registry_id);
  const name2 = entry?.name ?? block.registry_id;
  const { props, tooltip } = useTooltip([pkg === null ? { text: name2 } : { text: name2, badge: pkg }]);
  return /* @__PURE__ */ jsxs5("span", { className: "blockr-card-mark-frame", children: [
    /* @__PURE__ */ jsx9(Mark, { entry, size: 32, role: "img", "aria-label": pkg === null ? name2 : `${name2}, ${pkg}`, ...props }),
    tooltip,
    dot === null ? null : /* @__PURE__ */ jsx9(Dot, { dot })
  ] });
}
function Dot({ dot }) {
  const { props, tooltip } = useTooltip(dot.tooltip);
  return /* @__PURE__ */ jsxs5(Fragment4, { children: [
    /* @__PURE__ */ jsx9(
      "span",
      {
        className: "blockr-card-dot",
        role: "img",
        "aria-label": dot.tooltip,
        "data-status": dot.status,
        "data-ring": dot.ring ? "" : void 0,
        ...props
      }
    ),
    tooltip
  ] });
}
function cutOff(el) {
  return el.scrollHeight > el.clientHeight + 1 || el.scrollWidth > el.clientWidth + 1;
}
function Name({ name: name2, editable: editable3, onRename }) {
  const { props, tooltip } = useTooltip((el) => {
    const cut = cutOff(el);
    if (!editable3) return cut ? name2 : null;
    return cut ? [{ text: name2 }, { text: GESTURE, muted: true }] : GESTURE;
  });
  return /* @__PURE__ */ jsxs5(Fragment4, { children: [
    /* @__PURE__ */ jsx9("h3", { className: "blockr-card-name", "data-editable": editable3 ? "" : void 0, onDoubleClick: editable3 ? onRename : void 0, ...props, children: name2 }),
    tooltip
  ] });
}
function NameField({
  name: name2,
  onCommit,
  onDone
}) {
  const [value, setValue] = useState3(name2);
  const [refused, setRefused] = useState3(false);
  const field = useRef4(null);
  const ended = useRef4(false);
  const error = useId2();
  useLayoutEffect4(() => {
    field.current?.focus({ preventScroll: true });
    field.current?.select();
  }, []);
  const end = (commit, restore) => {
    if (ended.current) return;
    ended.current = true;
    const given = value.trim();
    if (commit && given !== name2) onCommit(given);
    onDone(restore);
  };
  return /* @__PURE__ */ jsxs5("div", { className: "blockr-card-name-edit", children: [
    /* @__PURE__ */ jsx9(
      "input",
      {
        ref: field,
        className: "blockr-card-name-field",
        "aria-label": "Block name",
        "aria-invalid": refused,
        "aria-describedby": refused ? error : void 0,
        value,
        onChange: (event) => {
          setValue(event.target.value);
          if (event.target.value.trim() !== "") setRefused(false);
        },
        onKeyDown: (event) => {
          if (event.key === "Enter") {
            event.preventDefault();
            if (value.trim() === "") setRefused(true);
            else end(true, true);
          } else if (event.key === "Escape") {
            event.preventDefault();
            event.stopPropagation();
            end(false, true);
          }
        },
        onBlur: () => {
          end(value.trim() !== "", false);
        }
      }
    ),
    refused ? /* @__PURE__ */ jsx9("div", { id: error, className: "blockr-card-name-error", children: "A block needs a name" }) : null
  ] });
}
function Tool({
  label,
  pressed,
  onClick,
  children: children2
}) {
  const { props, tooltip } = useTooltip(label);
  return /* @__PURE__ */ jsxs5(Fragment4, { children: [
    /* @__PURE__ */ jsx9("button", { type: "button", className: "blockr-card-tool", "aria-label": label, "aria-pressed": pressed, onClick, ...props, children: children2 }),
    tooltip
  ] });
}
function MenuButton2({
  ref: ref2,
  id,
  block,
  onRename
}) {
  const { core, plugin } = useDock();
  const [open, setOpen] = useState3(null);
  const { props: tip, tooltip } = useTooltip(open === null ? "More actions" : null);
  const button = useMenuButton(
    open !== null,
    (anchor, keyboard) => {
      setOpen({ menu: "actions", anchor, keyboard });
    },
    () => {
      setOpen(null);
    }
  );
  const close = useCallback3(
    (how) => {
      setOpen(null);
      if (how.restore) ref2.current?.focus({ preventScroll: true, focusVisible: !how.pointer });
    },
    [ref2]
  );
  const actions = (anchor) => {
    const copy = {
      key: "copy",
      label: "Copy block ID",
      meta: id,
      onSelect() {
        void copyText(anchor.ownerDocument, id).then((copied) => {
          if (!copied) {
            core.notifications.notify(plugin, {
              severity: "error",
              code: "copy_failed",
              message: `The browser did not let the dock copy the block ID "${id}".`
            });
          }
        });
      }
    };
    if (core.lock.board) return [copy];
    return [
      { key: "rename", label: "Rename", onSelect: onRename },
      {
        key: "append",
        label: "Append block",
        onSelect() {
          setOpen({ menu: "append", anchor, keyboard: false });
        }
      },
      copy,
      {
        key: "remove",
        label: "Remove block",
        lead: /* @__PURE__ */ jsx9(BinIcon, {}),
        danger: true,
        divided: true,
        onSelect() {
          runEdit(core, "blockr.core-remove_block", { id });
        }
      }
    ];
  };
  const append = (entry) => {
    const [first] = entry.inputs;
    const input = entry.inputs.length > 1 && !entry.inputs.includes("...") && first !== void 0 ? { input: first } : {};
    runEdit(core, "blockr.core-append_block", { from: id, registry_id: entry.id, ...input });
  };
  return /* @__PURE__ */ jsxs5(Fragment4, { children: [
    /* @__PURE__ */ jsx9(
      "button",
      {
        ref: ref2,
        type: "button",
        className: "blockr-card-tool",
        "aria-label": "More actions",
        ...tip,
        ...button,
        onPointerDown: () => {
          button.onPointerDown();
          tip.onPointerDown();
        },
        children: /* @__PURE__ */ jsx9(DotsIcon, {})
      }
    ),
    tooltip,
    open?.menu === "actions" ? /* @__PURE__ */ jsx9(
      Menu,
      {
        anchor: open.anchor,
        label: "More actions",
        align: "end",
        groups: [{ key: "actions", rows: actions(open.anchor) }],
        keyboard: open.keyboard,
        onClose: close
      }
    ) : null,
    open?.menu === "append" ? /* @__PURE__ */ jsx9(
      Menu,
      {
        anchor: open.anchor,
        label: "Block classes",
        align: "end",
        caption: `Append to ${block.name}`,
        search: { label: "Search blocks", empty: "No matches" },
        groups: classGroups(
          Object.values(core.blockRegistry()).filter((entry) => entry.inputs.length > 0),
          append
        ),
        keyboard: open.keyboard,
        onClose: close
      }
    ) : null
  ] });
}

// srcts/modules/dock/card/notifications.tsx
import { jsx as jsx10, jsxs as jsxs6 } from "react/jsx-runtime";
function Errors({ notifications }) {
  if (notifications.length === 0) return null;
  return /* @__PURE__ */ jsx10(Listed, { notifications, label: "Errors" });
}
function WarningsAndMessages({ notifications }) {
  const warnings = notifications.filter((notification) => notification.severity === "warning");
  const messages = notifications.filter((notification) => notification.severity === "message");
  if (warnings.length + messages.length === 0) return null;
  const count2 = [counted(warnings.length, "warning"), counted(messages.length, "message")].filter((part) => part !== null);
  return /* @__PURE__ */ jsxs6("details", { className: "blockr-card-messages", children: [
    /* @__PURE__ */ jsxs6("summary", { className: "blockr-card-messages-count", children: [
      count2.join(", "),
      /* @__PURE__ */ jsx10(Chevron, {})
    ] }),
    /* @__PURE__ */ jsx10(Listed, { notifications: [...warnings, ...messages] })
  ] });
}
function counted(n, severity) {
  if (n === 0) return null;
  return `${String(n)} ${severity}${n === 1 ? "" : "s"}`;
}
function Listed({ notifications, label }) {
  return /* @__PURE__ */ jsx10("ul", { className: "blockr-card-notifications", "aria-label": label, children: notifications.map(({ id, severity, message }) => /* @__PURE__ */ jsx10("li", { className: "blockr-card-notification", "data-severity": severity, children: plainText(message) }, id)) });
}

// srcts/modules/dock/card/section.tsx
import { useLayoutEffect as useLayoutEffect5, useRef as useRef5 } from "react";
import { jsx as jsx11 } from "react/jsx-runtime";
function Section({
  name: name2,
  open,
  empty = false,
  children: children2
}) {
  const ref2 = useRef5(null);
  const was = useRef5(null);
  useLayoutEffect5(() => {
    const el = ref2.current;
    if (el === null) return;
    const before2 = was.current;
    was.current = open;
    if (before2 === null || before2 === open || empty) {
      settle(el, open && !empty);
      return;
    }
    return fold(el, open);
  }, [open, empty]);
  return /* @__PURE__ */ jsx11("section", { ref: ref2, className: "blockr-card-section", "data-section": name2, children: /* @__PURE__ */ jsx11("div", { className: "blockr-card-section-body", children: children2 }) });
}
function settle(el, shows2) {
  delete el.dataset.folding;
  el.dataset.open = String(shows2);
  el.hidden = !shows2;
}
function fold(el, open) {
  let current = true;
  el.dataset.folding = "";
  if (open) {
    el.hidden = false;
    el.getBoundingClientRect();
  }
  el.dataset.open = String(open);
  const transitions = el.getAnimations().filter((animation) => "transitionProperty" in animation);
  void Promise.allSettled(transitions.map((transition) => transition.finished)).then(() => {
    if (!current) return;
    delete el.dataset.folding;
    if (!open) el.hidden = true;
  });
  return () => {
    current = false;
  };
}

// srcts/modules/dock/card/status-dot.ts
var DOTS = {
  stale: { ring: false, tooltip: "Inputs changed since this block last ran" },
  waiting: { ring: true, tooltip: "Waiting for a data input" },
  invalid: { ring: false, tooltip: "Set this block's inputs" },
  failed: { ring: false, tooltip: "Evaluation failed" },
  ready: { ring: false, tooltip: "Up to date" },
  unevaluated: { ring: true, tooltip: "Not evaluated yet" }
};
function statusDot(status, notifications) {
  if (status === void 0) return null;
  const reads = status.status !== "stale" && notifications.some(isOutputError) ? "failed" : status.status;
  return { status: reads, ...DOTS[reads] };
}
function isOutputError({ severity, source }) {
  if (severity !== "error") return false;
  if (source.kind === "r") return source.request.type === "output";
  return source.kind === "core" && source.code === "output_unanswered";
}

// srcts/modules/dock/card/card.tsx
import { jsx as jsx12, jsxs as jsxs7 } from "react/jsx-runtime";
var WITHOUT_OUTPUT = /* @__PURE__ */ new Set(["failed", "waiting", "invalid"]);
var WITH_NOTE = /* @__PURE__ */ new Set(["waiting", "invalid"]);
function BlockCard({ id }) {
  const { core } = useDock();
  const board = useBoard(core);
  const status = useStatus(core, id);
  const notified = useNotified(core, id);
  const [drawn, setDrawn] = useState4(false);
  const severities = core.options.value("show_conditions");
  const shown = useMemo2(
    () => notified.filter(
      (notification) => onCard(notification) && Array.isArray(severities) && severities.includes(notification.severity)
    ),
    [notified, severities]
  );
  const focus = useCallback4(() => {
    giveFocus(core, id);
  }, [core, id]);
  const mountUi = useCallback4(
    (el) => {
      const cleanup = core.mountBlock(el, id);
      const seen = () => {
        setDrawn(el.hasChildNodes());
      };
      seen();
      const watch = new MutationObserver(seen);
      watch.observe(el, { childList: true });
      return () => {
        watch.disconnect();
        cleanup();
      };
    },
    [core, id]
  );
  const mountOutput = useCallback4((el) => core.mountOutput(el, id), [core, id]);
  const block = board.blocks[id];
  if (block === void 0) return null;
  const sections = dockState(board)?.visible[id] ?? SECTIONS;
  const compact2 = core.options.value("compact") === true;
  const entry = core.blockRegistry()[block.registry_id];
  const inputs = !core.store.locked(id);
  const hidden = status !== void 0 && WITHOUT_OUTPUT.has(status.status);
  const note = status !== void 0 && WITH_NOTE.has(status.status) ? status.reason : null;
  const folded2 = shown.filter((notification) => notification.severity !== "error");
  return /* @__PURE__ */ jsxs7(
    "article",
    {
      className: "blockr-card",
      "aria-label": block.name,
      "data-compact": compact2 ? "" : void 0,
      onPointerDown: focus,
      onFocus: focus,
      children: [
        /* @__PURE__ */ jsx12(
          Header,
          {
            id,
            block,
            entry,
            sections,
            drawn: inputs && drawn,
            dot: statusDot(status, notified)
          }
        ),
        /* @__PURE__ */ jsx12(Errors, { notifications: shown.filter((notification) => notification.severity === "error") }),
        /* @__PURE__ */ jsxs7("div", { className: "blockr-card-sections", children: [
          inputs ? /* @__PURE__ */ jsx12(Section, { name: "inputs", open: sections.includes("inputs"), empty: !drawn, children: /* @__PURE__ */ jsx12(Mounted, { mount: mountUi, label: "Inputs" }) }) : null,
          /* @__PURE__ */ jsxs7(Section, { name: "outputs", open: sections.includes("outputs"), empty: hidden && note === null && folded2.length === 0, children: [
            /* @__PURE__ */ jsx12("div", { className: "blockr-card-output-frame", hidden, "data-faded": status?.status === "stale" ? "" : void 0, children: /* @__PURE__ */ jsx12(Mounted, { mount: mountOutput, label: "Output", className: "blockr-card-output" }) }),
            note === null ? null : /* @__PURE__ */ jsx12("p", { className: "blockr-card-status-note", children: plainText(note) }),
            /* @__PURE__ */ jsx12(WarningsAndMessages, { notifications: folded2 })
          ] })
        ] })
      ]
    }
  );
}

// srcts/modules/dock/dock.tsx
import { useBoard as useBoard4 } from "@blockr/core/react";
import { DockviewDefaultTab, DockviewReact, themeLightSpaced } from "./vendor-a3ad4872.js";
import { useCallback as useCallback7, useEffect as useEffect3, useMemo as useMemo4, useRef as useRef8, useState as useState8, useSyncExternalStore as useSyncExternalStore2 } from "react";

// srcts/modules/dock/drawing.ts
var EMPTY2 = { orientation: "horizontal", children: [], sizes: [] };
var DWELL_MS = 250;
var REVEAL_BAND = 35;
function drawDock(options) {
  const { core, plugin, api: api2, el, settings, narrow, drawn } = options;
  const doc = el.ownerDocument;
  const win = doc.defaultView ?? window;
  let view2;
  let column = narrow.get();
  let shown;
  let pending = 0;
  let press;
  let laidOut = api2.width > 0 && api2.height > 0;
  let dragging = false;
  const revealed = /* @__PURE__ */ new Set();
  let dwell;
  let stopped = false;
  const active = () => {
    const state = dockState(core.store.board());
    const id = state?.active ?? null;
    const held = id === null ? void 0 : state?.views[id];
    return id === null || held === void 0 ? void 0 : { id, grid: held.grid };
  };
  const title = (panel2) => "block" in panel2 ? core.store.board().blocks[panel2.block]?.name : drawn.get(panel2.plugin);
  const size = () => ({ width: api2.width, height: api2.height });
  const read = () => layoutToGrid(api2.toJSON());
  const target = (grid2) => column ? stackGrid(grid2, settings.fraction).grid : grid2;
  function draw() {
    const current = active();
    view2 = current?.id ?? null;
    const was = column;
    column = narrow.get();
    if (was !== column) lockDockview();
    const grid2 = current?.grid ?? EMPTY2;
    el.dataset.layout = column ? "narrow" : "wide";
    el.style.height = column ? `${String(Math.round(stackGrid(grid2, settings.fraction).height * 1e4) / 100)}vh` : "";
    api2.fromJSON(gridToLayout(target(grid2), title, size()), { reuseExistingPanels: true });
    syncRails2();
    shown = read();
  }
  function lockDockview() {
    const locked = core.lock.board;
    api2.updateOptions({
      locked: column || locked && settings.lockResize,
      disableDnd: column || locked && settings.lockRearrange
    });
  }
  function reconcile() {
    if (stopped || pending > 0) return;
    const current = active();
    if (view2 === void 0 || (current?.id ?? null) !== view2 || narrow.get() !== column) {
      draw();
      return;
    }
    if (current === void 0) return;
    const want = layoutToGrid(gridToLayout(target(current.grid), title, size()));
    const now = read();
    const changes = gridChanges(want, now);
    const sized = api2.width > 0 && api2.height > 0;
    if (changes.panels || changes.arrangement || sized && !sharesNear(want, now, settings.tolerance)) {
      draw();
      return;
    }
    const wantedGroups = tabGroups(want);
    tabGroups(now).forEach((group, i) => {
      const wanted = wantedGroups[i];
      if (wanted !== void 0 && !samePanel(wanted.active, group.active)) showPanel(wanted.active);
    });
    for (const edge2 of EDGES) {
      const [wanted, rail2] = [want.rails?.[edge2], now.rails?.[edge2]];
      const group = api2.getEdgeGroup(edge2);
      if (wanted === void 0 || rail2 === void 0 || group === void 0) continue;
      if (!samePanel(wanted.active, rail2.active)) showPanel(wanted.active);
      if (wanted.collapsed !== rail2.collapsed) {
        if (wanted.collapsed) group.collapse();
        else group.expand();
      }
      if (wanted.size !== rail2.size) group.setSize({ width: wanted.size });
    }
    retitle();
    shown = read();
  }
  function showPanel(panel2) {
    api2.getPanel(panelId(panel2))?.api.setActive();
  }
  function retitle() {
    for (const held of api2.panels) {
      const panel2 = panelFromId(held.id);
      const name2 = panel2 === void 0 ? void 0 : title(panel2);
      if (name2 !== void 0 && held.api.title !== name2) held.api.setTitle(name2);
    }
  }
  const dockWidth = () => el.getBoundingClientRect().width;
  function settle2(pointer2) {
    if (stopped) return;
    syncRails2();
    const current = active();
    const before2 = shown;
    if (before2 === void 0 || current === void 0 || current.id !== view2) return;
    const now = read();
    shown = now;
    const runs = column ? columnRuns(current.id, before2, now) : gestureRuns(current.id, before2, now, current.grid, {
      shares: pointer2 !== void 0,
      widths: pointer2?.width === dockWidth(),
      tolerance: settings.tolerance
    });
    for (const run of runs) send(run);
  }
  function send(run) {
    pending += 1;
    void core.commands.run(run.id, run.args).catch(() => {
    }).finally(() => {
      pending -= 1;
      reconcile();
    });
  }
  function syncRails2() {
    for (const edge2 of EDGES) {
      const group = api2.getGroup(railId(edge2));
      if (group === void 0) continue;
      const holds = group.panels.length > 0;
      if (holds && revealed.has(edge2) && group.api.isCollapsed()) group.api.expand();
      if (!dragging) revealed.delete(edge2);
      const wanted = !column && (holds || revealed.has(edge2));
      if (api2.isEdgeGroupVisible(edge2) !== wanted) api2.setEdgeGroupVisible(edge2, wanted);
    }
  }
  function restoreRails() {
    if (column) return;
    const current = active();
    const sizes = api2.toJSON().edgeGroups;
    for (const edge2 of EDGES) {
      const rail2 = current?.grid.rails?.[edge2];
      const group = api2.getEdgeGroup(edge2);
      const now = sizes?.[edge2]?.size;
      if (rail2 === void 0 || group === void 0 || rail2.collapsed || now === void 0) continue;
      if (Math.round(now) !== rail2.size) group.setSize({ width: rail2.size });
    }
  }
  function inBand(edge2, event) {
    const rect = el.getBoundingClientRect();
    const { clientX: x, clientY: y } = event;
    if (x < rect.left || x > rect.right || y < rect.top || y > rect.bottom) return false;
    return edge2 === "left" ? x - rect.left <= REVEAL_BAND : rect.right - x <= REVEAL_BAND;
  }
  function clearDwell() {
    if (dwell !== void 0) clearTimeout(dwell.timer);
    dwell = void 0;
  }
  function reveal2(edge2) {
    if (api2.isEdgeGroupVisible(edge2)) return;
    revealed.add(edge2);
    api2.setEdgeGroupVisible(edge2, true);
    const group = api2.getEdgeGroup(edge2);
    if (group !== void 0 && !group.isCollapsed()) group.collapse();
  }
  const onDragOver = (event) => {
    if (!dragging || column) return;
    const edge2 = EDGES.find((candidate) => inBand(candidate, event));
    if (edge2 === void 0) {
      clearDwell();
      return;
    }
    if (dwell?.edge === edge2) return;
    clearDwell();
    dwell = {
      edge: edge2,
      timer: setTimeout(() => {
        dwell = void 0;
        if (dragging) reveal2(edge2);
      }, DWELL_MS)
    };
  };
  const endDrag = () => {
    if (!dragging) return;
    dragging = false;
    clearDwell();
    syncRails2();
  };
  const onKey = (event) => {
    if (event.key === "Escape") endDrag();
  };
  const endPress = () => {
    if (press === void 0) return;
    press = void 0;
    queueMicrotask(() => {
      settle2();
    });
  };
  const onDragDone = () => {
    endDrag();
    endPress();
  };
  const onPointerDown = () => {
    press = { width: dockWidth() };
  };
  const onPointerUp = () => {
    const pressed = press;
    if (pressed === void 0) return;
    press = void 0;
    settle2(pressed);
  };
  const onResize = () => {
    if (stopped) return;
    if (press === void 0) restoreRails();
    const was = laidOut;
    laidOut = api2.width > 0 && api2.height > 0;
    if (!was && laidOut) reconcile();
  };
  const resizes = typeof win.ResizeObserver === "function" ? new win.ResizeObserver(() => {
    win.requestAnimationFrame(onResize);
  }) : void 0;
  const landed = () => {
    const drag = dragging;
    dragging = false;
    clearDwell();
    if (drag) endPress();
  };
  const disposables = [
    api2.onDidLayoutChange(() => {
      if (press === void 0) settle2();
    }),
    api2.onWillDragPanel(() => {
      dragging = true;
    }),
    api2.onWillDragGroup(() => {
      dragging = true;
    }),
    api2.onDidMovePanel(landed),
    api2.onDidAddPanel(landed),
    api2.onDidRemovePanel(landed)
  ];
  resizes?.observe(el);
  el.addEventListener("pointerdown", onPointerDown, true);
  win.addEventListener("pointerup", onPointerUp, true);
  win.addEventListener("pointercancel", onPointerUp, true);
  el.addEventListener("dragover", onDragOver);
  doc.addEventListener("drop", onDragDone);
  doc.addEventListener("dragend", onDragDone);
  doc.addEventListener("keydown", onKey);
  let lastState = core.store.board().plugins[plugin];
  const offBoard = core.store.subscribe((_change, board) => {
    if (board.plugins[plugin] === lastState) {
      retitle();
      return;
    }
    lastState = board.plugins[plugin];
    reconcile();
  });
  const offNarrow = narrow.subscribe(() => {
    reconcile();
  });
  lockDockview();
  draw();
  for (const edge2 of EDGES) {
    const group = api2.getEdgeGroup(edge2);
    if (group === void 0) continue;
    disposables.push(
      group.onDidCollapsedChange(() => {
        queueMicrotask(() => {
          if (press === void 0) settle2();
        });
      })
    );
  }
  return () => {
    stopped = true;
    clearDwell();
    resizes?.disconnect();
    offBoard();
    offNarrow();
    for (const disposable of disposables) disposable.dispose();
    el.removeEventListener("pointerdown", onPointerDown, true);
    win.removeEventListener("pointerup", onPointerUp, true);
    win.removeEventListener("pointercancel", onPointerUp, true);
    el.removeEventListener("dragover", onDragOver);
    doc.removeEventListener("drop", onDragDone);
    doc.removeEventListener("dragend", onDragDone);
    doc.removeEventListener("keydown", onKey);
  };
}
function sharesNear(a, b, tolerance) {
  const bare = (grid2) => ({ orientation: grid2.orientation, children: grid2.children, sizes: grid2.sizes });
  return !gridChanges(bare(a), bare(withStoredShares(a, b, tolerance))).sizes;
}

// srcts/modules/dock/picker.tsx
import { useBoard as useBoard2 } from "@blockr/core/react";
import { createContext as createContext2, useCallback as useCallback5, useContext as useContext2, useId as useId3, useMemo as useMemo3, useState as useState5 } from "react";
import { jsx as jsx13, jsxs as jsxs8 } from "react/jsx-runtime";
var PickerContext = createContext2(null);
function usePicker() {
  const value = useContext2(PickerContext);
  if (value === null) throw new Error("A button of the panel picker is drawn outside the picker.");
  return value;
}
function PanelPicker({ children: children2 }) {
  const [picking, setPicking] = useState5(void 0);
  const state = useMemo3(() => ({ picking, setPicking }), [picking]);
  const close = useCallback5(
    (how) => {
      setPicking(void 0);
      if (how.restore) picking?.button.focus({ preventScroll: true, focusVisible: !how.pointer });
    },
    [picking]
  );
  return /* @__PURE__ */ jsxs8(PickerContext, { value: state, children: [
    children2,
    picking === void 0 ? null : /* @__PURE__ */ jsx13(PickerMenu, { picking, onClose: close })
  ] });
}
function usePickerButton(near2) {
  const { picking, setPicking } = usePicker();
  const id = useId3();
  return useMenuButton(
    picking?.id === id,
    (button, keyboard) => {
      setPicking({ id, button, near: near2(), keyboard });
    },
    () => {
      setPicking(void 0);
    }
  );
}
function pickerGroups(board, registry, drawn, pick) {
  const view2 = activeView(board);
  if (view2 === void 0) return [];
  const blocks = Object.entries(board.blocks).flatMap(([block, held]) => {
    if (places(view2.grid, { block })) return [];
    const entry = registry[held.registry_id];
    return [
      {
        key: block,
        label: held.name,
        lead: /* @__PURE__ */ jsx13(Mark, { entry, size: 24 }),
        ...entry === void 0 ? {} : { detail: entry.name },
        search: block,
        onSelect: () => {
          pick({ block });
        }
      }
    ];
  });
  const plugins = [...drawn].flatMap(
    ([plugin, name2]) => places(view2.grid, { plugin }) ? [] : [
      {
        key: plugin,
        label: name2,
        search: plugin,
        onSelect: () => {
          pick({ plugin });
        }
      }
    ]
  );
  return [
    { key: "blocks", rows: blocks },
    { key: "plugins", title: "Plugins", rows: plugins }
  ];
}
function PickerMenu({ picking, onClose }) {
  const { core, drawn } = useDock();
  const board = useBoard2(core);
  const { near: near2 } = picking;
  const groups = useMemo3(() => {
    const pick = (panel2) => {
      const view2 = dockState(core.store.board())?.active ?? null;
      if (view2 === null) return;
      const args = near2 === void 0 ? { view: view2, panel: panel2 } : { view: view2, panel: panel2, near: near2, side: "within" };
      core.commands.run("blockr.dock-add_panel", args).catch(() => {
      });
    };
    return pickerGroups(board, core.blockRegistry(), drawn, pick);
  }, [board, core, drawn, near2]);
  return /* @__PURE__ */ jsx13(
    Menu,
    {
      anchor: picking.button,
      label: "Panels",
      caption: "Show in this view",
      search: { label: "Search", empty: "No panel matches." },
      groups,
      note: nothingLeft(board, drawn),
      keyboard: picking.keyboard,
      onClose
    }
  );
}
function nothingLeft(board, drawn) {
  return Object.keys(board.blocks).length === 0 && drawn.size === 0 ? "The board has no blocks yet. Add a new block to get started." : "All blocks and plugins are already in this view. Add a new block to the board first.";
}

// srcts/modules/dock/views-menu.tsx
import { useBoard as useBoard3 } from "@blockr/core/react";
import { useCallback as useCallback6, useLayoutEffect as useLayoutEffect7, useRef as useRef7, useState as useState7, useSyncExternalStore } from "react";

// srcts/modules/dock/name-field.tsx
import { useLayoutEffect as useLayoutEffect6, useRef as useRef6, useState as useState6 } from "react";
import { jsx as jsx14 } from "react/jsx-runtime";
function NameField2({ name: name2, label, className, onSubmit, onDone }) {
  const [text2, setText] = useState6(name2);
  const ref2 = useRef6(null);
  const done = useRef6(false);
  useLayoutEffect6(() => {
    ref2.current?.focus();
    ref2.current?.select();
  }, []);
  function finish(submit) {
    if (done.current) return;
    done.current = true;
    const trimmed = text2.trim();
    if (submit && trimmed !== "" && trimmed !== name2) onSubmit(trimmed);
    onDone();
  }
  return /* @__PURE__ */ jsx14(
    "input",
    {
      ref: ref2,
      type: "text",
      "aria-label": label,
      className,
      value: text2,
      spellCheck: false,
      onChange: (event) => {
        setText(event.target.value);
      },
      onKeyDown: (event) => {
        if (event.key === "Enter" && !event.nativeEvent.isComposing) {
          event.preventDefault();
          finish(true);
        } else if (event.key === "Escape") {
          event.preventDefault();
          event.stopPropagation();
          finish(false);
        }
      },
      onBlur: () => {
        finish(true);
      }
    }
  );
}

// srcts/modules/dock/tips.ts
function cutTip(text2) {
  const tip = (event) => {
    setTip(event.currentTarget, text2);
  };
  return { onPointerEnter: tip, onFocus: tip };
}
function setTip(el, text2) {
  const label = el.querySelector(".blockr-dock-bar-label") ?? el;
  const tooltip = text2(label.scrollWidth > label.clientWidth);
  if (tooltip === void 0) el.removeAttribute("title");
  else el.setAttribute("title", tooltip);
}

// srcts/modules/dock/views-list.ts
function rowsOf(state, root, open) {
  const rows = [];
  const walked = new Set(root === null ? [] : [root]);
  const walk = (folder2, depth) => {
    for (const entry of inFolder(state, folder2)) {
      const opens = "folder" in entry && open.has(entry.folder) && !walked.has(entry.folder);
      rows.push({ entry, depth, folder: folder2, open: opens });
      if (opens) {
        walked.add(entry.folder);
        walk(entry.folder, depth + 1);
      }
    }
  };
  walk(root, 0);
  return rows;
}
function dropAt(state, rows, spans, y, moving) {
  const drop = placeAt(state, rows, spans, y);
  if (drop === void 0) return void 0;
  if ("folder" in moving && drop.folder !== null && within(state, drop.folder, moving.folder)) return void 0;
  if (drop.before !== void 0 && sameEntry(drop.before, moving)) return void 0;
  const folder2 = "view" in moving ? state.views[moving.view]?.folder : state.folders[moving.folder]?.folder;
  if (folder2 === drop.folder && sitsAt(state, moving, drop.folder, drop.before)) return void 0;
  return drop;
}
function placeAt(state, rows, spans, y) {
  const last = rows.length - 1;
  const lastSpan = spans[last];
  if (last < 0 || lastSpan === void 0) return void 0;
  if (y >= lastSpan.bottom) return { folder: null, before: void 0, mark: { row: last, at: "below", depth: 0 } };
  const at2 = spans.findIndex((span2) => y < span2.bottom);
  const row = rows[at2];
  const span = spans[at2];
  if (row === void 0 || span === void 0) return void 0;
  const share = (y - span.top) / Math.max(1, span.bottom - span.top);
  const { entry } = row;
  const isFolder2 = "folder" in entry;
  if (share < (isFolder2 ? 0.25 : 0.5)) return { folder: row.folder, before: entry, mark: { row: at2, at: "above", depth: row.depth } };
  if (!isFolder2) return { folder: row.folder, before: after(state, row), mark: { row: at2, at: "below", depth: row.depth } };
  if (share <= 0.75) return { folder: entry.folder, before: void 0, mark: { row: at2, at: "into", depth: row.depth + 1 } };
  if (row.open) return { folder: entry.folder, before: inFolder(state, entry.folder)[0], mark: { row: at2, at: "below", depth: row.depth + 1 } };
  return { folder: row.folder, before: after(state, row), mark: { row: at2, at: "below", depth: row.depth } };
}
function after(state, row) {
  const listed = inFolder(state, row.folder);
  return listed[listed.findIndex((entry) => sameEntry(entry, row.entry)) + 1];
}

// srcts/modules/dock/views-menu.tsx
import { Fragment as Fragment5, jsx as jsx15, jsxs as jsxs9 } from "react/jsx-runtime";
var ACTIVATE_VIEW = "blockr.dock-activate_view";
var ADD_VIEW = "blockr.dock-add_view";
var ADD_FOLDER = "blockr.dock-add_folder";
var RENAME_VIEW = "blockr.dock-rename_view";
var RENAME_FOLDER = "blockr.dock-rename_folder";
var REMOVE_VIEW = "blockr.dock-remove_view";
var REMOVE_FOLDER = "blockr.dock-remove_folder";
var MOVE_VIEW = "blockr.dock-move_view";
var MOVE_FOLDER = "blockr.dock-move_folder";
var SET_OPTION = "blockr.core-set_option";
var EDGE2 = 24;
var STEP = 8;
function useViewTabs(core) {
  const subscribe = useCallback6(
    (changed) => core.store.subscribe(() => {
      changed();
    }),
    [core]
  );
  return useSyncExternalStore(subscribe, () => core.store.board().options.view_tabs === true);
}
function Views({ openFolders }) {
  const { core } = useDock();
  const board = useBoard3(core);
  const state = dockState(board);
  if (state === void 0) return null;
  const tabs = board.options.view_tabs === true;
  return /* @__PURE__ */ jsxs9("div", { className: "blockr-dock-bar-views", "data-tabs": tabs ? "" : void 0, children: [
    tabs ? /* @__PURE__ */ jsx15(TabLine, { state, openFolders }) : null,
    /* @__PURE__ */ jsx15(ViewsMenu, { state, openFolders, tabs }),
    tabs ? null : /* @__PURE__ */ jsx15("span", { className: "blockr-dock-bar-rule", "aria-hidden": "true" })
  ] });
}
function ViewsMenu({ state, openFolders, tabs }) {
  const { core } = useDock();
  const open = useOpenFolders(openFolders);
  const [opened, setOpened] = useState7(null);
  const button = useRef7(null);
  const props = useMenuButton(
    opened !== null,
    (anchor, keyboard) => {
      openFolders.open(activeHolders(state));
      setOpened({ anchor, keyboard, managing: false });
    },
    () => {
      setOpened(null);
    }
  );
  const close = useCallback6((how) => {
    setOpened(null);
    if (how.restore) button.current?.focus({ preventScroll: true, focusVisible: !how.pointer });
  }, []);
  const active = state.active === null ? void 0 : state.views[state.active];
  const name2 = active?.name ?? "Views";
  const actions = (at2) => core.lock.board ? [] : [
    {
      key: "manage",
      label: "Manage views",
      lead: /* @__PURE__ */ jsx15(SlidersIcon, {}),
      divided: true,
      onSelect: () => {
        setOpened({ ...at2, managing: true });
      }
    },
    {
      key: "tabs",
      label: "Show views as tabs",
      checked: tabs,
      onSelect: () => {
        runEdit(core, SET_OPTION, { id: "view_tabs", value: !tabs });
      }
    }
  ];
  return /* @__PURE__ */ jsxs9(Fragment5, { children: [
    /* @__PURE__ */ jsxs9(
      "button",
      {
        ref: button,
        type: "button",
        className: tabs ? "blockr-dock-bar-button blockr-dock-views-chevron" : "blockr-dock-bar-button blockr-dock-views-button",
        "aria-label": tabs ? "Views" : `Views: ${name2}`,
        ...tabs ? { title: "Views" } : cutTip((cut) => cut ? name2 : void 0),
        ...props,
        children: [
          tabs ? null : /* @__PURE__ */ jsx15("span", { className: "blockr-dock-bar-label", children: name2 }),
          /* @__PURE__ */ jsx15(Chevron, {})
        ]
      }
    ),
    opened === null ? null : opened.managing ? /* @__PURE__ */ jsx15(
      Manage,
      {
        anchor: opened.anchor,
        keyboard: opened.keyboard,
        state,
        openFolders,
        onClose: close,
        onDone: () => {
          setOpened({ ...opened, managing: false });
        }
      }
    ) : /* @__PURE__ */ jsx15(
      Menu,
      {
        anchor: opened.anchor,
        label: "Views",
        align: "end",
        groups: [
          { key: "views", rows: viewRows(core, state, null, open, openFolders) },
          { key: "actions", rows: actions(opened) }
        ],
        keyboard: opened.keyboard,
        onClose: close
      }
    )
  ] });
}
function viewRows(core, state, root, open, openFolders) {
  return rowsOf(state, root, open).map(({ entry, depth, open: unfolded }) => {
    const common2 = { key: keyOf(entry), label: nameOf(state, entry), depth };
    if ("view" in entry) {
      return {
        ...common2,
        current: entry.view === state.active,
        onSelect: () => {
          activate(core, state, entry.view);
        }
      };
    }
    return {
      ...common2,
      fold: {
        open: unfolded,
        onToggle: (on) => {
          openFolders.set(entry.folder, on);
        }
      }
    };
  });
}
function Manage({ anchor, keyboard, state, openFolders, onClose, onDone }) {
  const { core } = useDock();
  const ref2 = useRef7(null);
  const open = useOpenFolders(openFolders);
  const rows = rowsOf(state, null, open);
  const slot = rows.some((row) => "folder" in row.entry);
  const [renaming, setRenaming] = useState7(void 0);
  const [confirming, setConfirming] = useState7(void 0);
  const [drag, setDrag] = useState7(void 0);
  const dragging = useRef7(void 0);
  const rowList = useRef7(null);
  useMenuSurface(ref2, anchor, { depth: 0, align: "end", keyboard, onClose });
  useLayoutEffect7(() => {
    ref2.current?.focus({ preventScroll: true });
  }, []);
  const keepFocus2 = () => {
    ref2.current?.focus({ preventScroll: true, focusVisible: false });
  };
  const follow = (next) => {
    dragging.current = next;
    setDrag(next);
  };
  const addView = () => {
    void applied2(core, ADD_VIEW, {}).then((done) => {
      const added = done ? dockState(core.store.board())?.active : null;
      if (added !== void 0 && added !== null) setRenaming(keyOf({ view: added }));
    });
  };
  const addFolder = () => {
    const before2 = new Set(Object.keys(state.folders));
    void applied2(core, ADD_FOLDER, {}).then((done) => {
      const after2 = done ? Object.keys(dockState(core.store.board())?.folders ?? {}) : [];
      const added = after2.find((id) => !before2.has(id));
      if (added !== void 0) setRenaming(keyOf({ folder: added }));
    });
  };
  const grip = (entry) => ({
    onPointerDown(event) {
      if (event.button !== 0) return;
      event.preventDefault();
      event.currentTarget.setPointerCapture(event.pointerId);
      setConfirming(void 0);
      follow({ entry, drop: void 0 });
    },
    onPointerMove(event) {
      const el = rowList.current;
      if (dragging.current === void 0 || el === null) return;
      scrollNearEdge(ref2.current, event.clientY);
      follow({ entry, drop: dropAt(state, rows, spansOf(el), event.clientY, entry) });
    },
    onPointerUp() {
      const drop = dragging.current?.drop;
      follow(void 0);
      if (drop !== void 0) move(core, openFolders, entry, drop);
    },
    onPointerCancel() {
      follow(void 0);
    }
  });
  return /* @__PURE__ */ jsxs9(
    "div",
    {
      ref: ref2,
      popover: "auto",
      role: "dialog",
      "aria-label": "Manage views",
      tabIndex: -1,
      className: "blockr-dock-menu blockr-dock-views-manage",
      "data-align": "end",
      "data-depth": 0,
      onToggle: (event) => {
        if (event.newState === "closed") onClose({ restore: false, pointer: false });
      },
      onKeyDown: (event) => {
        if (event.key !== "Escape") return;
        event.preventDefault();
        event.stopPropagation();
        onClose({ restore: true, pointer: false });
      },
      children: [
        /* @__PURE__ */ jsx15("div", { ref: rowList, className: "blockr-dock-views-rows", children: rows.map((row, i) => {
          const { entry } = row;
          const key = keyOf(entry);
          const name2 = nameOf(state, entry);
          if (confirming === key) {
            return /* @__PURE__ */ jsx15(
              ConfirmRow,
              {
                name: name2,
                depth: row.depth,
                onRemove: () => {
                  keepFocus2();
                  setConfirming(void 0);
                  remove(core, entry);
                },
                onCancel: () => {
                  setConfirming(void 0);
                }
              },
              key
            );
          }
          return /* @__PURE__ */ jsx15(
            ManagedRow,
            {
              row,
              name: name2,
              slot,
              renaming: renaming === key,
              removable: removable(core, entry),
              dragging: drag !== void 0 && sameEntry(drag.entry, entry),
              mark: drag?.drop?.mark.row === i ? drag.drop.mark : void 0,
              grip: grip(entry),
              onFold: () => {
                if ("folder" in entry) openFolders.set(entry.folder, !row.open);
              },
              onRename: () => {
                setConfirming(void 0);
                setRenaming(key);
              },
              onRenamed: (next) => {
                rename(core, entry, next);
              },
              onRenameDone: () => {
                setRenaming(void 0);
              },
              onConfirm: () => {
                setRenaming(void 0);
                setConfirming(key);
              }
            },
            key
          );
        }) }),
        /* @__PURE__ */ jsx15("hr", { className: "blockr-dock-menu-divider" }),
        /* @__PURE__ */ jsxs9("div", { className: "blockr-dock-views-foot", children: [
          /* @__PURE__ */ jsxs9("button", { type: "button", className: "blockr-dock-views-add", onClick: addView, children: [
            /* @__PURE__ */ jsx15(PlusIcon, {}),
            "New view"
          ] }),
          /* @__PURE__ */ jsxs9("button", { type: "button", className: "blockr-dock-views-add", onClick: addFolder, children: [
            /* @__PURE__ */ jsx15(PlusIcon, {}),
            "New folder"
          ] }),
          /* @__PURE__ */ jsx15(
            "button",
            {
              type: "button",
              className: "blockr-dock-views-done",
              onClick: () => {
                keepFocus2();
                onDone();
              },
              children: "Done"
            }
          )
        ] })
      ]
    }
  );
}
function ManagedRow({
  row,
  name: name2,
  slot,
  renaming,
  removable: removable2,
  dragging,
  mark,
  grip,
  onFold,
  onRename,
  onRenamed,
  onRenameDone,
  onConfirm
}) {
  const el = useRef7(null);
  const nameButton = useRef7(null);
  const refocus = useRef7(false);
  const { entry } = row;
  const isView2 = "view" in entry;
  useLayoutEffect7(() => {
    if (renaming || !refocus.current) return;
    refocus.current = false;
    nameButton.current?.focus();
  }, [renaming]);
  const style = { ...depthStyle(row.depth), ...mark === void 0 ? {} : { "--drop-depth": mark.depth } };
  return /* @__PURE__ */ jsxs9(
    "div",
    {
      ref: el,
      className: "blockr-dock-views-managed",
      "data-row": "",
      "data-drop": mark?.at,
      "data-dragging": dragging ? "" : void 0,
      style,
      children: [
        /* @__PURE__ */ jsx15("span", { className: "blockr-dock-views-grip", "aria-hidden": "true", ...grip, children: /* @__PURE__ */ jsx15(GripIcon, {}) }),
        isView2 ? slot ? /* @__PURE__ */ jsx15("span", { className: "blockr-dock-views-slot" }) : null : /* @__PURE__ */ jsx15(
          "button",
          {
            type: "button",
            className: "blockr-dock-views-fold",
            "aria-expanded": row.open,
            "aria-label": `${row.open ? "Close" : "Open"} the folder ${name2}`,
            onClick: onFold,
            children: /* @__PURE__ */ jsx15(Chevron, {})
          }
        ),
        renaming ? /* @__PURE__ */ jsx15(
          NameField2,
          {
            name: name2,
            label: isView2 ? "View name" : "Folder name",
            className: "blockr-dock-views-field",
            onSubmit: onRenamed,
            onDone: () => {
              const doc = el.current?.ownerDocument;
              refocus.current = doc !== void 0 && el.current?.contains(doc.activeElement) === true;
              onRenameDone();
            }
          }
        ) : /* @__PURE__ */ jsx15(
          "button",
          {
            ref: nameButton,
            type: "button",
            className: "blockr-dock-views-name",
            onClick: onRename,
            ...cutTip((cut) => cut ? `${name2}
Click to rename` : "Click to rename"),
            children: name2
          }
        ),
        removable2 ? /* @__PURE__ */ jsx15(
          "button",
          {
            type: "button",
            className: "blockr-dock-views-remove",
            "aria-label": `Remove ${name2}`,
            title: isView2 ? "Remove view" : "Remove folder",
            onClick: onConfirm,
            children: /* @__PURE__ */ jsx15(RemoveIcon, {})
          }
        ) : null
      ]
    }
  );
}
function ConfirmRow({ name: name2, depth, onRemove, onCancel }) {
  const removeButton = useRef7(null);
  useLayoutEffect7(() => {
    removeButton.current?.focus();
  }, []);
  return /* @__PURE__ */ jsxs9(
    "div",
    {
      className: "blockr-dock-views-managed",
      "data-row": "",
      "data-confirming": "",
      style: depthStyle(depth),
      onBlur: (event) => {
        const next = event.relatedTarget;
        if (!(next instanceof Node) || !event.currentTarget.contains(next)) onCancel();
      },
      onKeyDown: (event) => {
        if (event.key !== "Escape") return;
        event.preventDefault();
        event.stopPropagation();
        onCancel();
      },
      children: [
        /* @__PURE__ */ jsx15("span", { className: "blockr-dock-views-confirm-text", children: `Remove \u201C${name2}\u201D?` }),
        /* @__PURE__ */ jsx15("button", { ref: removeButton, type: "button", className: "blockr-dock-views-confirm", onClick: onRemove, children: "Remove" })
      ]
    }
  );
}
function TabLine({ state, openFolders }) {
  const { core } = useDock();
  const line = useRef7(null);
  const { active } = state;
  const outer = activeHolders(state).at(-1);
  useLayoutEffect7(() => {
    reveal(line.current);
  }, [active, outer]);
  return /* @__PURE__ */ jsx15("nav", { ref: line, className: "blockr-dock-tabs", "aria-label": "Views", children: inFolder(state, null).map(
    (entry) => "view" in entry ? /* @__PURE__ */ jsx15(
      "button",
      {
        type: "button",
        className: "blockr-dock-tab",
        "aria-current": entry.view === active ? "page" : void 0,
        onClick: () => {
          activate(core, state, entry.view);
        },
        children: nameOf(state, entry)
      },
      keyOf(entry)
    ) : /* @__PURE__ */ jsx15(FolderTab, { state, folder: entry.folder, current: entry.folder === outer, openFolders }, keyOf(entry))
  ) });
}
function FolderTab({ state, folder: folder2, current, openFolders }) {
  const { core } = useDock();
  const open = useOpenFolders(openFolders);
  const [opened, setOpened] = useState7(null);
  const button = useRef7(null);
  const props = useMenuButton(
    opened !== null,
    (anchor, keyboard) => {
      openFolders.open(activeHolders(state));
      setOpened({ anchor, keyboard });
    },
    () => {
      setOpened(null);
    }
  );
  const close = useCallback6((how) => {
    setOpened(null);
    if (how.restore) button.current?.focus({ preventScroll: true, focusVisible: !how.pointer });
  }, []);
  const name2 = nameOf(state, { folder: folder2 });
  return /* @__PURE__ */ jsxs9(Fragment5, { children: [
    /* @__PURE__ */ jsxs9("button", { ref: button, type: "button", className: "blockr-dock-tab", "aria-current": current ? "true" : void 0, ...props, children: [
      name2,
      /* @__PURE__ */ jsx15(Chevron, {})
    ] }),
    opened === null ? null : /* @__PURE__ */ jsx15(
      Menu,
      {
        anchor: opened.anchor,
        label: name2,
        groups: [{ key: "views", rows: viewRows(core, state, folder2, open, openFolders) }],
        note: "This folder is empty.",
        keyboard: opened.keyboard,
        onClose: close
      }
    )
  ] });
}
function useOpenFolders(openFolders) {
  return useSyncExternalStore(openFolders.subscribe, openFolders.get);
}
function activeHolders(state) {
  return state.active === null ? [] : foldersHolding(state, { view: state.active });
}
function reveal(line) {
  if (line === null) return;
  const tab = line.querySelector("[aria-current]");
  if (tab === null) return;
  const bounds = line.getBoundingClientRect();
  const at2 = tab.getBoundingClientRect();
  if (at2.left < bounds.left) line.scrollLeft -= bounds.left - at2.left;
  else if (at2.right > bounds.right) line.scrollLeft += at2.right - bounds.right;
}
function scrollNearEdge(panel2, y) {
  if (panel2 === null) return;
  const { top, bottom } = panel2.getBoundingClientRect();
  if (y < top + EDGE2) panel2.scrollTop -= STEP;
  else if (y > bottom - EDGE2) panel2.scrollTop += STEP;
}
function spansOf(rowList) {
  return [...rowList.querySelectorAll(":scope > [data-row]")].map((row) => {
    const { top, bottom } = row.getBoundingClientRect();
    return { top, bottom };
  });
}
async function applied2(core, id, args) {
  return await core.commands.run(id, args).then(
    () => true,
    () => false
  );
}
function activate(core, state, view2) {
  if (view2 !== state.active) runEdit(core, ACTIVATE_VIEW, { view: view2 });
}
function rename(core, entry, name2) {
  if ("view" in entry) runEdit(core, RENAME_VIEW, { view: entry.view, name: name2 });
  else runEdit(core, RENAME_FOLDER, { folder: entry.folder, name: name2 });
}
function remove(core, entry) {
  if ("view" in entry) runEdit(core, REMOVE_VIEW, { view: entry.view });
  else runEdit(core, REMOVE_FOLDER, { folder: entry.folder });
}
function removable(core, entry) {
  return "view" in entry ? core.commands.preflight(REMOVE_VIEW, { view: entry.view }) : core.commands.preflight(REMOVE_FOLDER, { folder: entry.folder });
}
function move(core, openFolders, entry, drop) {
  const args = "view" in entry ? { view: entry.view, folder: drop.folder } : { folder: entry.folder, into: drop.folder };
  if (drop.before !== void 0) args.before = drop.before;
  const id = "view" in entry ? MOVE_VIEW : MOVE_FOLDER;
  if (!core.commands.preflight(id, args)) return;
  if (drop.folder !== null) openFolders.open([drop.folder]);
  runEdit(core, id, args);
}
function nameOf(state, entry) {
  return ("view" in entry ? state.views[entry.view]?.name : state.folders[entry.folder]?.name) ?? "";
}
function keyOf(entry) {
  return "view" in entry ? `view:${entry.view}` : `folder:${entry.folder}`;
}
function depthStyle(depth) {
  return { "--depth": depth };
}

// srcts/modules/dock/dock.tsx
import { Fragment as Fragment6, jsx as jsx16, jsxs as jsxs10 } from "react/jsx-runtime";
function PanelFrame({ params }) {
  const { panel: panel2 } = params;
  return "block" in panel2 ? /* @__PURE__ */ jsx16("div", { className: "blockr-dock-panel", "data-block": panel2.block, children: /* @__PURE__ */ jsx16(BlockCard, { id: panel2.block }) }) : /* @__PURE__ */ jsx16(PluginFrame, { plugin: panel2.plugin });
}
function PluginFrame({ plugin }) {
  const { core } = useDock();
  const mount = useCallback7((el) => core.mountPlugin(el, plugin), [core, plugin]);
  return /* @__PURE__ */ jsx16(Mounted, { mount, className: "blockr-dock-panel", data: { "data-plugin": plugin } });
}
var components = { [PANEL_COMPONENT]: PanelFrame };
function Tab(props) {
  const { core } = useDock();
  const { panel: panel2 } = props.params;
  const onPointerDown = "block" in panel2 ? () => {
    giveFocus(core, panel2.block);
  } : void 0;
  return /* @__PURE__ */ jsx16(DockviewDefaultTab, { ...props, hideClose: core.lock.board, onPointerDown });
}
function AddPanelButton({ api: api2, group }) {
  const button = usePickerButton(() => {
    const shown = group.activePanel;
    return shown === void 0 ? void 0 : panelFromId(shown.id);
  });
  if (api2.location.type === "edge") return null;
  return /* @__PURE__ */ jsx16("div", { className: "blockr-dock-add-slot", children: /* @__PURE__ */ jsx16("button", { type: "button", className: "blockr-dock-add", "aria-label": "Add panel", ...button, children: /* @__PURE__ */ jsx16("svg", { viewBox: "0 0 16 16", width: "16", height: "16", "aria-hidden": "true", focusable: "false", children: /* @__PURE__ */ jsx16("path", { d: "M8 3v10M3 8h10", fill: "none", stroke: "currentColor", strokeWidth: "1.5", strokeLinecap: "round" }) }) }) });
}
function showsNothing(board, drawn) {
  const view2 = activeView(board);
  if (view2 === void 0) return false;
  return !gridPanels(view2.grid).some((panel2) => "block" in panel2 ? Object.hasOwn(board.blocks, panel2.block) : drawn.has(panel2.plugin));
}
function EmptyView() {
  const { core, drawn } = useDock();
  const board = useBoard4(core);
  if (!showsNothing(board, drawn)) return null;
  if (!core.lock.board) return /* @__PURE__ */ jsx16(EmptyPrompt, {});
  return /* @__PURE__ */ jsxs10("div", { className: "blockr-dock-empty", children: [
    /* @__PURE__ */ jsx16(LockIcon, { size: 32 }),
    /* @__PURE__ */ jsx16("p", { children: "This view is empty" })
  ] });
}
function EmptyPrompt() {
  const button = usePickerButton(() => void 0);
  return /* @__PURE__ */ jsxs10("div", { className: "blockr-dock-empty", children: [
    /* @__PURE__ */ jsxs10("svg", { viewBox: "0 0 16 16", width: "32", height: "32", "aria-hidden": "true", focusable: "false", children: [
      /* @__PURE__ */ jsx16("circle", { cx: "8", cy: "8", r: "6.5", fill: "none", stroke: "currentColor" }),
      /* @__PURE__ */ jsx16("path", { d: "M8 5v6M5 8h6", fill: "none", stroke: "currentColor", strokeLinecap: "round" })
    ] }),
    /* @__PURE__ */ jsx16("p", { children: "Start by adding a panel" }),
    /* @__PURE__ */ jsx16("button", { type: "button", className: "blockr-dock-empty-add", ...button, children: "Add panel" })
  ] });
}
function Dock({ core, plugin, settings, narrow, drawn, viewsMenu, sidebar }) {
  const root = useRef8(null);
  const stop = useRef8(void 0);
  const [tooltips] = useState8(createTooltips);
  const shared = useMemo4(() => ({ core, plugin, drawn, tooltips }), [core, plugin, drawn, tooltips]);
  const locked = core.lock.board;
  const tabs = useViewTabs(core) && viewsMenu;
  const { pinned } = useSyncExternalStore2(sidebar.subscribe, sidebar.state);
  const onReady = useCallback7(
    (event) => {
      const el = root.current;
      if (el === null) return;
      stop.current?.();
      stop.current = drawDock({ core, plugin, api: event.api, el, settings, narrow, drawn });
    },
    [core, plugin, settings, narrow, drawn]
  );
  useEffect3(
    () => () => {
      stop.current?.();
      stop.current = void 0;
    },
    []
  );
  const view2 = /* @__PURE__ */ jsxs10(Fragment6, { children: [
    /* @__PURE__ */ jsx16(
      DockviewReact,
      {
        components,
        onReady,
        theme: themeLightSpaced,
        defaultRenderer: "always",
        defaultTabComponent: Tab,
        ...locked ? {} : { leftHeaderActionsComponent: AddPanelButton }
      }
    ),
    /* @__PURE__ */ jsx16(EmptyView, {})
  ] });
  return /* @__PURE__ */ jsx16(DockContext, { value: shared, children: /* @__PURE__ */ jsx16("div", { className: "blockr-dock", ref: root, "data-tabs": tabs ? "" : void 0, "data-pushed": pinned ? "" : void 0, children: locked ? view2 : /* @__PURE__ */ jsx16(PanelPicker, { children: view2 }) }) });
}

// srcts/modules/dock/navbar.tsx
import { useBoard as useBoard5 } from "@blockr/core/react";
import { useCallback as useCallback8, useEffect as useEffect4, useReducer, useRef as useRef9, useState as useState9, useSyncExternalStore as useSyncExternalStore3 } from "react";

// srcts/modules/dock/navbar-items.ts
var PARTS = ["logo", "name", "storage", "undo_redo", "spacer", "views", "read_only", "options"];
var DEFAULT_NAVBAR = PARTS.map((id) => ({ kind: "part", id }));
function holdsPart(items, id) {
  return items.some((item) => item.kind === "part" && item.id === id);
}
function readNavbar(args) {
  const sent = args.navbar;
  if (sent === void 0) return DEFAULT_NAVBAR;
  if (!Array.isArray(sent)) {
    throw new TypeError(`The navbar is ${JSON.stringify(sent)}, where the dock takes a list of navbar items.`);
  }
  return readList(sent, readItem, "on the bar");
}
function readList(list, read, where) {
  const items = list.map(read);
  const ids = /* @__PURE__ */ new Set();
  for (const item of items) {
    if (ids.has(item.id)) throw new TypeError(`More than one navbar item ${where} has the id ${JSON.stringify(item.id)}.`);
    ids.add(item.id);
  }
  return items;
}
function readItem(sent) {
  const { kind, id } = common(sent);
  const field = (name2) => text(sent, id, name2);
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
function readEntry(sent) {
  const { kind, id } = common(sent);
  const field = (name2) => text(sent, id, name2);
  if (kind === "link") return { kind, id, label: field("label"), href: field("href") };
  if (kind !== "menu") {
    throw new TypeError(`The navbar menu holds the item ${JSON.stringify(id)}, which is neither a link nor a menu.`);
  }
  const items = sent.items;
  if (!Array.isArray(items)) {
    throw new TypeError(`The items of the navbar menu ${JSON.stringify(id)} are ${JSON.stringify(items)}, where the dock takes a list.`);
  }
  return { kind, id, label: field("label"), items: readList(items, readEntry, `in the menu ${JSON.stringify(id)}`) };
}
function common(sent) {
  if (!isObject(sent) || typeof sent.kind !== "string" || typeof sent.id !== "string" || sent.id === "") {
    throw new TypeError(`The navbar item ${JSON.stringify(sent)} has no kind and id that the dock reads.`);
  }
  return { kind: sent.kind, id: sent.id };
}
function text(sent, id, name2) {
  const value = isObject(sent) ? sent[name2] : void 0;
  if (typeof value !== "string" || value === "") {
    throw new TypeError(`The ${name2} of the navbar item ${JSON.stringify(id)} is ${JSON.stringify(value)}, where the dock takes a non-empty string.`);
  }
  return value;
}
function isPart(id) {
  return PARTS.includes(id);
}

// srcts/modules/dock/navbar.tsx
import { jsx as jsx17, jsxs as jsxs11 } from "react/jsx-runtime";
function Navbar({ items, busy, sidebar, mac, openFolders }) {
  const { core } = useDock();
  const storage = useStorage(core);
  const { pinned } = useSyncExternalStore3(sidebar.subscribe, sidebar.state);
  const apart = new Set(items.flatMap((item) => item.kind === "storage" ? [item.id] : []));
  const tabs = useViewTabs(core) && holdsPart(items, "views");
  return /* @__PURE__ */ jsx17("header", { className: "blockr-dock-bar", "data-tabs": tabs ? "" : void 0, "data-pushed": pinned ? "" : void 0, children: items.map((item) => /* @__PURE__ */ jsx17(
    Item,
    {
      item,
      storage,
      apart,
      busy,
      sidebar,
      mac,
      openFolders
    },
    item.id
  )) });
}
function Item({ item, storage, apart, busy, sidebar, mac, openFolders }) {
  switch (item.kind) {
    case "part":
      return /* @__PURE__ */ jsx17(Part, { id: item.id, storage, apart, busy, sidebar, mac, openFolders });
    case "storage": {
      const control = storage?.failed === void 0 ? storage?.controls.find((c) => c.id === item.id) : void 0;
      return control === void 0 ? null : /* @__PURE__ */ jsx17(StorageControlView, { control });
    }
    case "link":
      return /* @__PURE__ */ jsx17(Link, { item });
    case "menu":
      return /* @__PURE__ */ jsx17(MenuButton, { label: item.label, entries: linkEntries(item.items), className: "blockr-dock-bar-button" });
    case "module":
      return /* @__PURE__ */ jsx17(ModuleSlot, { item });
  }
}
function Part({ id, storage, apart, busy, sidebar, mac, openFolders }) {
  const { core } = useDock();
  switch (id) {
    case "logo":
      return /* @__PURE__ */ jsx17(Logo, { busy });
    case "name":
      return /* @__PURE__ */ jsx17(BoardName, {});
    case "storage":
      return /* @__PURE__ */ jsx17(StorageControls, { storage, apart });
    case "undo_redo":
      return /* @__PURE__ */ jsx17(UndoRedo, { mac });
    case "spacer":
      return /* @__PURE__ */ jsx17("span", { className: "blockr-dock-bar-spacer", "aria-hidden": "true" });
    case "views":
      return /* @__PURE__ */ jsx17(Views, { openFolders });
    case "read_only":
      return core.lock.board ? /* @__PURE__ */ jsx17(ReadOnlyPill, {}) : null;
    case "options":
      return /* @__PURE__ */ jsx17(OptionsButton, { sidebar });
  }
}
var CELLS = [
  [0, 2],
  [0, 1],
  [0, 0],
  [1, 0],
  [2, 0],
  [1, 1],
  [2, 2]
];
function Logo({ busy }) {
  const turning = useSyncExternalStore3(busy.subscribe, busy.get);
  return /* @__PURE__ */ jsxs11("span", { className: turning ? "blockr-dock-logo is-computing" : "blockr-dock-logo", title: turning ? "Computing" : void 0, children: [
    /* @__PURE__ */ jsx17("svg", { className: "blockr-dock-logo-mark", width: "20", height: "20", viewBox: "0 0 224 224", "aria-hidden": "true", focusable: "false", children: CELLS.map(([x, y], i) => /* @__PURE__ */ jsx17("rect", { style: { "--i": i }, x: x * 80, y: y * 80, width: "64", height: "64", rx: "7" }, i)) }),
    /* @__PURE__ */ jsx17("span", { className: "blockr-dock-visually-hidden", role: "status", children: turning ? "Computing" : "" })
  ] });
}
function BoardName() {
  const { core } = useDock();
  const board = useBoard5(core);
  const [renaming, setRenaming] = useState9(false);
  const locked = core.lock.board;
  if (renaming) {
    return /* @__PURE__ */ jsx17(
      NameField2,
      {
        name: board.name,
        label: "Board name",
        className: "blockr-dock-bar-field",
        onSubmit: (name2) => {
          core.commands.run("blockr.core-rename_board", { name: name2 }).catch(() => void 0);
        },
        onDone: () => {
          setRenaming(false);
        }
      }
    );
  }
  const tip = cutTip((cut) => {
    if (locked) return cut ? board.name : void 0;
    return cut ? `${board.name}
Click to rename` : "Click to rename";
  });
  if (locked) {
    return /* @__PURE__ */ jsx17("span", { className: "blockr-dock-bar-name blockr-dock-bar-shrinks", ...tip, children: /* @__PURE__ */ jsx17("span", { className: "blockr-dock-bar-label", children: board.name }) });
  }
  return /* @__PURE__ */ jsx17(
    "button",
    {
      type: "button",
      className: "blockr-dock-bar-name blockr-dock-bar-shrinks",
      "aria-label": `Board name: ${board.name}. Rename`,
      onClick: () => {
        setRenaming(true);
      },
      ...tip,
      children: /* @__PURE__ */ jsx17("span", { className: "blockr-dock-bar-label", children: board.name })
    }
  );
}
function StorageControls({ storage, apart }) {
  if (storage === void 0) return null;
  if (storage.failed !== void 0) {
    return /* @__PURE__ */ jsx17(Warning, { message: `The storage controls of ${storage.package} failed: ${storage.failed}` });
  }
  const shown = storage.controls.filter((control) => !apart.has(control.id));
  if (shown.length === 0) return null;
  return /* @__PURE__ */ jsx17("div", { className: "blockr-dock-bar-storage blockr-dock-bar-shrinks", children: shown.map((control) => /* @__PURE__ */ jsx17(StorageControlView, { control }, control.id)) });
}
function StorageControlView({ control }) {
  const { core } = useDock();
  const tip = control.kind === "menu" || control.tooltip === void 0 ? cutTip((cut) => cut ? labelOf(control) : void 0) : {};
  switch (control.kind) {
    case "button":
      return /* @__PURE__ */ jsx17(
        "button",
        {
          type: "button",
          className: "blockr-dock-bar-button blockr-dock-bar-shrinks",
          disabled: control.disabled,
          title: control.tooltip,
          "aria-keyshortcuts": ariaShortcuts(control.shortcut),
          onClick: () => {
            void runButton(core, control);
          },
          ...tip,
          children: /* @__PURE__ */ jsx17("span", { className: "blockr-dock-bar-label", children: control.label })
        }
      );
    case "menu":
      return /* @__PURE__ */ jsx17(
        MenuButton,
        {
          label: control.label,
          entries: storageEntries(core, control.items),
          className: "blockr-dock-bar-button blockr-dock-bar-shrinks",
          onTip: (button) => {
            setTip(button, (cut) => cut ? control.label : void 0);
          }
        }
      );
    case "status":
      return /* @__PURE__ */ jsx17("span", { className: "blockr-dock-bar-status blockr-dock-bar-shrinks", title: control.tooltip, ...tip, children: /* @__PURE__ */ jsx17("span", { className: "blockr-dock-bar-label", children: control.text }) });
  }
}
function labelOf(control) {
  return control.kind === "status" ? control.text : control.label;
}
function storageEntries(core, items) {
  return items.map((item) => {
    switch (item.kind) {
      case "button":
        return {
          kind: "button",
          id: item.id,
          label: item.label,
          ...item.tooltip === void 0 ? {} : { tooltip: item.tooltip },
          ...item.shortcut === void 0 ? {} : { shortcut: item.shortcut },
          ...item.disabled === void 0 ? {} : { disabled: item.disabled },
          run: () => {
            void runButton(core, item);
          }
        };
      case "menu":
        return { kind: "menu", id: item.id, label: item.label, entries: storageEntries(core, item.items) };
      case "status":
        return { kind: "status", id: item.id, text: item.text, ...item.tooltip === void 0 ? {} : { tooltip: item.tooltip } };
    }
  });
}
function linkEntries(items) {
  return items.map(
    (item) => item.kind === "link" ? { kind: "link", id: item.id, label: item.label, href: item.href } : { kind: "menu", id: item.id, label: item.label, entries: linkEntries(item.items) }
  );
}
function Link({ item }) {
  return /* @__PURE__ */ jsx17("a", { className: "blockr-dock-bar-button", href: item.href, target: "_blank", rel: "noopener noreferrer", children: /* @__PURE__ */ jsx17("span", { className: "blockr-dock-bar-label", children: item.label }) });
}
function UndoRedo({ mac }) {
  const { core } = useDock();
  const { canUndo, canRedo } = useHistory(core);
  const step = (back) => {
    try {
      if (back) core.store.undo();
      else core.store.redo();
    } catch {
    }
  };
  return /* @__PURE__ */ jsxs11("div", { className: "blockr-dock-bar-steps", children: [
    /* @__PURE__ */ jsx17(
      "button",
      {
        type: "button",
        className: "blockr-dock-bar-button",
        title: `Undo (${shortcutLabel("Mod+Z", mac)})`,
        "aria-keyshortcuts": "Control+Z Meta+Z",
        disabled: !canUndo,
        onClick: () => {
          step(true);
        },
        children: "Undo"
      }
    ),
    /* @__PURE__ */ jsx17(
      "button",
      {
        type: "button",
        className: "blockr-dock-bar-button",
        title: `Redo (${shortcutLabel("Mod+Shift+Z", mac)})`,
        "aria-keyshortcuts": "Control+Shift+Z Meta+Shift+Z Control+Y",
        disabled: !canRedo,
        onClick: () => {
          step(false);
        },
        children: "Redo"
      }
    )
  ] });
}
function ReadOnlyPill() {
  return /* @__PURE__ */ jsxs11("span", { className: "blockr-dock-bar-pill", role: "status", "aria-label": "Read-only mode", title: "Editing is disabled by this deployment.", children: [
    /* @__PURE__ */ jsx17(LockIcon, {}),
    /* @__PURE__ */ jsx17("span", { children: "Read-only" })
  ] });
}
function OptionsButton({ sidebar }) {
  const { open } = useSyncExternalStore3(sidebar.subscribe, sidebar.state);
  return /* @__PURE__ */ jsx17(
    "button",
    {
      type: "button",
      className: "blockr-dock-bar-button blockr-dock-bar-options",
      "aria-expanded": open,
      "aria-controls": sidebar.id,
      onClick: () => {
        sidebar.set(!open);
      },
      children: "Options"
    }
  );
}
function ModuleSlot({ item }) {
  const { core } = useDock();
  const ref2 = useRef9(null);
  const [failure, setFailure] = useState9(void 0);
  useEffect4(() => {
    const el = ref2.current;
    if (el === null) return void 0;
    const controller = new AbortController();
    const { signal } = controller;
    const gone = () => signal.aborted;
    let cleanup;
    const draw = async () => {
      const mod = navbarModule(await core.module(item.module));
      if (gone()) return;
      const done = await mod.render({ el, core, signal });
      if (gone()) done?.();
      else cleanup = done;
    };
    draw().catch((error) => {
      if (gone()) return;
      const message = `The module ${item.module} of the navbar item "${item.id}" cannot draw: ${errorMessage(error)}`;
      core.log("blockr.dock").error(new Error(message, { cause: error }));
      setFailure(message);
    });
    return () => {
      controller.abort();
      try {
        cleanup?.();
      } catch (error) {
        core.log("blockr.dock").error(asError(error));
      }
    };
  }, [core, item.id, item.module]);
  if (failure !== void 0) return /* @__PURE__ */ jsx17(Warning, { message: failure });
  return /* @__PURE__ */ jsx17("span", { ref: ref2, className: "blockr-dock-bar-module", "data-module": item.module });
}
function navbarModule(exports) {
  if (!isObject(exports) || typeof exports.api !== "number") {
    throw new Error("It declares no major of core's JS API, as `export const api = 1`.");
  }
  if (exports.api !== API) {
    throw new Error(`It was built against major ${String(exports.api)} of core's JS API, where the dock's is ${String(API)}.`);
  }
  const mod = exports.default;
  if (!isObject(mod) || typeof mod.render !== "function") throw new Error("It has no default export with a render() function.");
  return mod;
}
function Warning({ message }) {
  return /* @__PURE__ */ jsx17("span", { className: "blockr-dock-bar-warning", role: "img", "aria-label": message, title: message, children: /* @__PURE__ */ jsx17(WarningIcon, {}) });
}
function useStorage(core) {
  const storage = core.plugins.storage();
  const [, redraw] = useReducer((n) => n + 1, 0);
  useEffect4(() => {
    if (storage?.subscribe === void 0) return void 0;
    try {
      return storage.subscribe(redraw);
    } catch (error) {
      core.log("blockr.dock").error(asError(error));
      return void 0;
    }
  }, [core, storage]);
  useBoard5(core);
  const drawn = drawnControls(core);
  const problem = drawn?.problem;
  useEffect4(() => {
    if (problem !== void 0) core.log("blockr.dock").error(problem);
  }, [core, problem]);
  return drawn;
}
function useHistory(core) {
  const subscribe = useCallback8(
    (changed) => core.store.subscribe(() => {
      changed();
    }),
    [core]
  );
  const canUndo = useSyncExternalStore3(subscribe, () => core.store.canUndo());
  const canRedo = useSyncExternalStore3(subscribe, () => core.store.canRedo());
  return { canUndo, canRedo };
}

// srcts/modules/dock/narrow.ts
function trackNarrow(win, breakpoint) {
  const narrowNow = () => breakpoint !== void 0 && win.innerWidth < breakpoint;
  let narrow = narrowNow();
  const listeners = /* @__PURE__ */ new Set();
  const onResize = () => {
    const now = narrowNow();
    if (now === narrow) return;
    narrow = now;
    for (const fn of [...listeners]) fn();
  };
  const follows = breakpoint !== void 0 && Number.isFinite(breakpoint);
  if (follows) win.addEventListener("resize", onResize);
  return {
    get: () => narrow,
    subscribe(fn) {
      listeners.add(fn);
      return () => {
        listeners.delete(fn);
      };
    },
    stop() {
      if (follows) win.removeEventListener("resize", onResize);
      listeners.clear();
    }
  };
}

// srcts/modules/dock/open-folders.ts
function createOpenFolders() {
  let open = /* @__PURE__ */ new Set();
  const listeners = /* @__PURE__ */ new Set();
  const change = (next) => {
    open = next;
    for (const fn of [...listeners]) fn();
  };
  return {
    get: () => open,
    set(folder2, on) {
      if (open.has(folder2) === on) return;
      change(on ? /* @__PURE__ */ new Set([...open, folder2]) : new Set([...open].filter((other) => other !== folder2)));
    },
    open(folders) {
      const closed = folders.filter((folder2) => !open.has(folder2));
      if (closed.length > 0) change(/* @__PURE__ */ new Set([...open, ...closed]));
    },
    subscribe(fn) {
      listeners.add(fn);
      return () => {
        listeners.delete(fn);
      };
    }
  };
}

// srcts/modules/dock/options-sidebar.tsx
import { useBoard as useBoard6 } from "@blockr/core/react";
import { useCallback as useCallback9, useEffect as useEffect5, useLayoutEffect as useLayoutEffect8, useRef as useRef10, useState as useState10, useSyncExternalStore as useSyncExternalStore4 } from "react";
import { jsx as jsx18, jsxs as jsxs12 } from "react/jsx-runtime";
var TITLE = "Board options";
var FOCUSABLE = [
  "a[href]",
  "button:not([disabled])",
  'input:not([disabled]):not([type="hidden"])',
  "select:not([disabled])",
  "textarea:not([disabled])",
  '[tabindex]:not([tabindex="-1"])',
  "details > summary:first-of-type"
].join(", ");
var OVERLAYS = [":popover-open", "dialog:modal"];
function categories(board, registered) {
  const held = Object.keys(board.options).flatMap((id) => {
    const def = Object.hasOwn(registered, id) ? registered[id] : void 0;
    return def === void 0 ? [] : [{ id, label: def.label, category: def.category }];
  });
  held.sort((a, b) => a.category.localeCompare(b.category) || a.label.localeCompare(b.label) || a.id.localeCompare(b.id));
  const grouped = /* @__PURE__ */ new Map();
  for (const { id, category } of held) grouped.set(category, [...grouped.get(category) ?? [], id]);
  return grouped;
}
function OptionsSidebar({ sidebar }) {
  const { core } = useDock();
  const board = useBoard6(core);
  const { open, pinned, expanded } = useSyncExternalStore4(sidebar.subscribe, sidebar.state);
  const grouped = categories(board, core.options.registered());
  const defaults = useDefaults(core, [...grouped.values()].flat());
  const ref2 = useRef10(null);
  const title = `${sidebar.id}-title`;
  const changed = /* @__PURE__ */ new Map();
  for (const [id, value] of defaults) {
    const held = Object.hasOwn(board.options, id) ? board.options[id] : void 0;
    if (held !== void 0 && !sameJson(held, value)) changed.set(id, value);
  }
  useFocus(ref2, open);
  useKeys(ref2, sidebar);
  usePressOutside(ref2, sidebar, open && !pinned);
  return /* @__PURE__ */ jsxs12(
    "div",
    {
      ref: ref2,
      id: sidebar.id,
      className: "blockr-dock-sidebar",
      role: "dialog",
      "aria-labelledby": title,
      tabIndex: -1,
      inert: !open,
      "data-open": open ? "" : void 0,
      children: [
        /* @__PURE__ */ jsxs12("header", { className: "blockr-dock-sidebar-header", children: [
          /* @__PURE__ */ jsx18("h2", { id: title, className: "blockr-dock-sidebar-title", children: TITLE }),
          grouped.size === 0 ? null : /* @__PURE__ */ jsx18(ExpandAll, { categories: [...grouped.keys()], expanded, sidebar }),
          /* @__PURE__ */ jsx18(
            "button",
            {
              type: "button",
              className: "blockr-dock-sidebar-button blockr-dock-sidebar-pin",
              "aria-label": "Pin sidebar",
              "aria-pressed": pinned,
              title: "Pin",
              onClick: () => {
                sidebar.pin(!pinned);
              },
              children: /* @__PURE__ */ jsx18(PinIcon, {})
            }
          ),
          /* @__PURE__ */ jsx18(
            "button",
            {
              type: "button",
              className: "blockr-dock-sidebar-button",
              "aria-label": "Close sidebar",
              title: "Close",
              onClick: () => {
                sidebar.set(false);
              },
              children: /* @__PURE__ */ jsx18(RemoveIcon, {})
            }
          )
        ] }),
        /* @__PURE__ */ jsx18("div", { className: "blockr-dock-sidebar-body", children: grouped.size === 0 ? /* @__PURE__ */ jsx18("p", { className: "blockr-dock-sidebar-note", children: "The board holds no board options." }) : /* @__PURE__ */ jsx18("div", { className: "blockr-dock-sidebar-categories", children: [...grouped].map(([category, ids]) => /* @__PURE__ */ jsx18(
          Category,
          {
            category,
            ids,
            expanded: expanded.includes(category),
            changed,
            sidebar
          },
          category
        )) }) })
      ]
    }
  );
}
function ExpandAll({
  categories: categories2,
  expanded,
  sidebar
}) {
  const all = categories2.every((category) => expanded.includes(category));
  return /* @__PURE__ */ jsx18(
    "button",
    {
      type: "button",
      className: "blockr-dock-sidebar-expand",
      "aria-expanded": all,
      onClick: () => {
        for (const category of categories2) sidebar.expand(category, !all);
      },
      children: all ? "Collapse all" : "Expand all"
    }
  );
}
function Category({ category, ids, expanded, changed, sidebar }) {
  const [folding, setFolding] = useState10(false);
  const folds = useRef10(0);
  return /* @__PURE__ */ jsxs12(
    "details",
    {
      className: "blockr-dock-sidebar-category",
      open: expanded,
      onToggle: (event) => {
        const details = event.currentTarget;
        sidebar.expand(category, details.open);
        if (details.open) return;
        folds.current += 1;
        const fold2 = folds.current;
        setFolding(true);
        void folded(details).then(() => {
          if (folds.current === fold2) setFolding(false);
        });
      },
      children: [
        /* @__PURE__ */ jsxs12("summary", { className: "blockr-dock-sidebar-summary", children: [
          /* @__PURE__ */ jsx18("span", { className: "blockr-dock-sidebar-summary-name", children: category }),
          /* @__PURE__ */ jsx18(Count, { n: ids.length }),
          /* @__PURE__ */ jsx18(Chevron, {})
        ] }),
        expanded || folding ? /* @__PURE__ */ jsx18("div", { className: "blockr-dock-sidebar-options", children: ids.map((id) => /* @__PURE__ */ jsx18(OptionLine, { option: id, reset: changed.get(id) }, id)) }) : null
      ]
    }
  );
}
function Count({ n }) {
  return /* @__PURE__ */ jsxs12("span", { className: "blockr-dock-sidebar-count", children: [
    /* @__PURE__ */ jsx18("span", { "aria-hidden": "true", children: n }),
    /* @__PURE__ */ jsx18("span", { className: "blockr-dock-visually-hidden", children: n === 1 ? ", 1 board option" : `, ${String(n)} board options` })
  ] });
}
async function folded(el) {
  const style = el.ownerDocument.defaultView?.getComputedStyle(el, "::details-content");
  const ms = style === void 0 ? 0 : longest(style.transitionDuration, style.transitionDelay);
  await new Promise((resolve) => {
    setTimeout(resolve, ms);
  });
}
function longest(durations, delays) {
  const ms = (list) => list.split(",").map((item) => {
    const time = Number.parseFloat(item);
    if (!Number.isFinite(time)) return 0;
    return item.trim().endsWith("ms") ? time : time * 1e3;
  });
  const duration = ms(durations);
  const delay = ms(delays);
  return Math.max(0, ...duration.map((d, i) => d + (delay[i % delay.length] ?? 0)));
}
function OptionLine({ option, reset }) {
  const { core } = useDock();
  const mount = useCallback9((el) => core.mountOption(el, option), [core, option]);
  const line = useRef10(null);
  const registered = core.options.registered();
  const label = Object.hasOwn(registered, option) ? registered[option]?.label ?? option : option;
  return /* @__PURE__ */ jsxs12("div", { ref: line, className: "blockr-dock-sidebar-option", "data-option": option, children: [
    /* @__PURE__ */ jsx18(Mounted, { mount, className: "blockr-dock-sidebar-option-ui" }),
    reset === void 0 ? null : /* @__PURE__ */ jsx18(
      "button",
      {
        type: "button",
        className: "blockr-dock-sidebar-reset",
        "aria-label": `Reset ${label} to its default`,
        title: "Reset to the default",
        onClick: () => {
          line.current?.querySelector(".blockr-dock-sidebar-option-ui")?.querySelector(FOCUSABLE)?.focus();
          runEdit(core, "blockr.core-set_option", { id: option, value: reset });
        },
        children: "Reset"
      }
    )
  ] });
}
function useDefaults(core, ids) {
  const [resolved, setResolved] = useState10(() => /* @__PURE__ */ new Map());
  const asked = useRef10(/* @__PURE__ */ new Set());
  useEffect5(() => {
    const registered = core.options.registered();
    for (const id of ids) {
      if (asked.current.has(id)) continue;
      asked.current.add(id);
      const def = Object.hasOwn(registered, id) ? registered[id] : void 0;
      if (def === void 0) continue;
      const given = def.default;
      void Promise.resolve(typeof given === "function" ? given(core) : given).then(
        (value) => {
          setResolved((before2) => new Map(before2).set(id, value));
        },
        () => void 0
      );
    }
  });
  return resolved;
}
function useFocus(ref2, open) {
  const before2 = useRef10(null);
  useLayoutEffect8(() => {
    const root = ref2.current;
    if (root === null) return;
    const doc = root.ownerDocument;
    if (open) {
      before2.current = doc.activeElement;
      const first = root.querySelector(".blockr-dock-sidebar-body")?.querySelector(FOCUSABLE);
      (first ?? root).focus();
      return;
    }
    const back = before2.current;
    before2.current = null;
    const active = doc.activeElement;
    const held = active === null || active === doc.body || root.contains(active);
    if (held && back instanceof HTMLElement && back.isConnected) back.focus();
  }, [ref2, open]);
}
function useKeys(ref2, sidebar) {
  useEffect5(() => {
    const root = ref2.current;
    if (root === null) return void 0;
    const onKey = (event) => {
      if (event.defaultPrevented) return;
      if (event.key === "Tab") {
        keepFocus(root, event);
        return;
      }
      if (event.key !== "Escape" || holdsOverlay(root) || sidebar.state().pinned) return;
      sidebar.set(false);
      event.preventDefault();
    };
    root.addEventListener("keydown", onKey);
    return () => {
      root.removeEventListener("keydown", onKey);
    };
  }, [ref2, sidebar]);
}
function keepFocus(root, event) {
  const focusable = [...root.querySelectorAll(FOCUSABLE)].filter(
    (el) => el.checkVisibility({ visibilityProperty: true }) && el.closest("[inert]") === null
  );
  const first = focusable[0];
  const last = focusable.at(-1);
  const active = root.ownerDocument.activeElement;
  if (first === void 0 || last === void 0) {
    event.preventDefault();
    root.focus();
  } else if (event.shiftKey && (active === first || active === root)) {
    event.preventDefault();
    last.focus();
  } else if (!event.shiftKey && (active === last || active === root)) {
    event.preventDefault();
    first.focus();
  }
}
function holdsOverlay(root) {
  return OVERLAYS.some((selector) => {
    try {
      return root.querySelector(selector) !== null;
    } catch {
      return false;
    }
  });
}
function usePressOutside(ref2, sidebar, armed) {
  useEffect5(() => {
    const root = ref2.current;
    if (root === null || !armed) return void 0;
    const doc = root.ownerDocument;
    const onPress = (event) => {
      const target = event.target;
      if (!(target instanceof Element) || root.contains(target)) return;
      if (target.closest(`[aria-controls="${sidebar.id}"], .blockr-dock-toasts`) !== null) return;
      sidebar.set(false);
    };
    doc.addEventListener("pointerdown", onPress, true);
    return () => {
      doc.removeEventListener("pointerdown", onPress, true);
    };
  }, [ref2, sidebar, armed]);
}

// srcts/modules/dock/settings.ts
async function readSettings(core) {
  const [tolerance, locks, breakpoint, fraction, spinnerDelay] = await Promise.all([
    gridSizeTolerance(core),
    layoutLocks(core),
    narrowBreakpoint(core),
    groupFraction(core),
    spinnerDelayMs(core)
  ]);
  return { tolerance, lockResize: locks.resize, lockRearrange: locks.rearrange, breakpoint, fraction, spinnerDelay };
}
var BREAKPOINT_FLOOR = 50;
async function narrowBreakpoint(core) {
  const sent = await core.blockrOption("narrow_breakpoint");
  if (sent === void 0) return void 0;
  const value = sent === null ? Infinity : typeof sent === "string" ? numberIn(sent) : sent;
  if (typeof value === "number" && value >= BREAKPOINT_FLOOR) return value;
  throw new TypeError(
    `The blockr option narrow_breakpoint is ${JSON.stringify(sent)}, where it takes a number of at least ${String(BREAKPOINT_FLOOR)} or Inf.`
  );
}
async function groupFraction(core) {
  const sent = await core.blockrOption("narrow_group_fraction");
  if (sent === void 0) return 0.8;
  const value = typeof sent === "string" ? numberIn(sent) : sent;
  if (typeof value === "number" && value > 0 && value <= 1) return value;
  throw new TypeError(`The blockr option narrow_group_fraction is ${JSON.stringify(sent)}, where it takes a number above 0 and at most 1.`);
}
var SPINNER_DELAY = 200;
async function spinnerDelayMs(core) {
  const sent = await core.blockrOption("spinner_delay_ms");
  const value = typeof sent === "string" ? numberIn(sent) : sent;
  if (typeof value !== "number" || !Number.isFinite(value)) return SPINNER_DELAY;
  const ms = Math.trunc(value);
  return ms >= 0 ? ms : SPINNER_DELAY;
}
function numberIn(text2) {
  const trimmed = text2.trim();
  if (trimmed === "Inf") return Infinity;
  return trimmed === "" ? Number.NaN : Number(trimmed);
}

// srcts/modules/dock/sidebar.ts
var CLOSED = { open: false, pinned: false, expanded: [] };
var made = 0;
function createSidebar() {
  made += 1;
  let state = CLOSED;
  const listeners = /* @__PURE__ */ new Set();
  const change = (next) => {
    if (next.open === state.open && next.pinned === state.pinned && next.expanded === state.expanded) return;
    state = next;
    for (const fn of [...listeners]) fn();
  };
  return {
    id: `blockr-dock-sidebar-${String(made)}`,
    state: () => state,
    set(open) {
      change(open ? { ...state, open: true } : { ...state, open: false, pinned: false });
    },
    pin(pinned) {
      if (state.open) change({ ...state, pinned });
    },
    expand(category, expanded) {
      if (state.expanded.includes(category) === expanded) return;
      const next = expanded ? [...state.expanded, category] : state.expanded.filter((c) => c !== category);
      change({ ...state, expanded: next });
    },
    subscribe(fn) {
      listeners.add(fn);
      return () => {
        listeners.delete(fn);
      };
    }
  };
}

// srcts/modules/dock/start.ts
var PAGE_ID = "page";
var PAGE_NAME = "Page";
function startingPatches(board, drawn, view2) {
  const state = dockState(board);
  if (state === void 0) return [];
  const at2 = (...segments) => pointer(["plugins", DOCK, ...segments]);
  const patches = [];
  const added = Object.keys(state.views).length === 0;
  let views = state.views;
  if (added) {
    const page = { name: PAGE_NAME, folder: null, index: mintKey(state, null, void 0), grid: startingGrid(board, drawn) };
    patches.push({ op: "add", path: at2("views", PAGE_ID), value: page });
    views = { [PAGE_ID]: page };
  }
  const draws = (panel2) => "block" in panel2 || drawn.includes(panel2.plugin);
  for (const [id, held] of Object.entries(state.views)) {
    if (gridPanels(held.grid).every(draws)) continue;
    patches.push({ op: "replace", path: at2("views", id, "grid"), value: prunePanels(held.grid, draws) });
  }
  const named = view2 !== void 0 && Object.hasOwn(views, view2) ? view2 : void 0;
  const active = named ?? (added ? PAGE_ID : state.active);
  if (active !== state.active) patches.push({ op: "replace", path: at2("active"), value: active });
  return patches;
}
function startingGrid(board, drawn) {
  const blocks = Object.keys(board.blocks).map((block2) => ({ block: block2 }));
  const plugins = drawn.map((plugin2) => ({ plugin: plugin2 }));
  const [block] = blocks;
  const [plugin] = plugins;
  return {
    orientation: "horizontal",
    children: block === void 0 ? [] : [{ panels: blocks, active: block }],
    sizes: block === void 0 ? [] : [1],
    ...plugin === void 0 ? {} : { rails: { left: { panels: plugins, active: plugin, collapsed: false, size: RAIL_SIZE } } }
  };
}
function drawnPlugins(core) {
  const plugins = core.plugins.appPlugins();
  return new Map(
    Object.entries(plugins).flatMap(([id, plugin]) => core.plugins.pluginClass(id).ui === void 0 ? [] : [[id, plugin.name]])
  );
}
function urlView(location) {
  const view2 = new URLSearchParams(location?.search ?? "").get("view");
  return view2 === null || view2 === "" ? void 0 : view2;
}
function startOnBoard(core, plugin, drawn, view2) {
  const patches = startingPatches(core.store.board(), drawn, view2);
  if (patches.length > 0) core.store.apply({ patches }, plugin, { undo: false });
}

// srcts/modules/dock/toasts.tsx
import { useBoard as useBoard7 } from "@blockr/core/react";
import { useSyncExternalStore as useSyncExternalStore5 } from "react";
import { jsx as jsx19, jsxs as jsxs13 } from "react/jsx-runtime";
var SHOWN_FOR = 5e3;
var FADES_FOR = 300;
function createToasts(notifications) {
  const shown = /* @__PURE__ */ new Map();
  const gone = /* @__PURE__ */ new Set();
  const listeners = /* @__PURE__ */ new Set();
  let heard = notifications.get();
  let list = [];
  let stopped = false;
  function publish() {
    const next = heard.flatMap((notification) => {
      const toast = shown.get(notification.id)?.toast;
      return toast === void 0 ? [] : [toast];
    });
    if (next.length === list.length && next.every((toast, i) => toast === list[i])) return;
    list = next;
    for (const fn of [...listeners]) fn();
  }
  function countDown(id, toast) {
    if (stopped || toast.toast.notification.severity !== "message" || toast.kept.size > 0) return;
    clearTimeout(toast.timer);
    toast.timer = setTimeout(() => {
      toast.toast = { ...toast.toast, fading: true };
      toast.timer = setTimeout(() => {
        hide(id);
      }, FADES_FOR);
      publish();
    }, SHOWN_FOR);
  }
  function hide(id) {
    const toast = shown.get(id);
    if (toast === void 0) return;
    clearTimeout(toast.timer);
    shown.delete(id);
    gone.add(id);
    publish();
  }
  function update2() {
    const holding = new Set(heard.map((notification) => notification.id));
    for (const [id, toast] of shown) {
      if (holding.has(id)) continue;
      clearTimeout(toast.timer);
      shown.delete(id);
    }
    for (const id of gone) {
      if (!holding.has(id)) gone.delete(id);
    }
    for (const notification of heard) {
      if (onCard(notification) || gone.has(notification.id)) continue;
      const toast = shown.get(notification.id);
      if (toast === void 0) {
        const added = { toast: { notification, fading: false }, timer: void 0, kept: /* @__PURE__ */ new Set() };
        shown.set(notification.id, added);
        countDown(notification.id, added);
      } else if (toast.toast.notification !== notification) {
        toast.toast = { ...toast.toast, notification };
      }
    }
    publish();
  }
  const off = notifications.subscribe((all) => {
    heard = all;
    update2();
  });
  update2();
  return {
    list: () => list,
    subscribe(fn) {
      const wrapped = () => {
        fn();
      };
      listeners.add(wrapped);
      return () => {
        listeners.delete(wrapped);
      };
    },
    close: hide,
    keep(id, by) {
      const toast = shown.get(id);
      if (toast === void 0) return;
      toast.kept.add(by);
      clearTimeout(toast.timer);
      toast.timer = void 0;
      if (!toast.toast.fading) return;
      toast.toast = { ...toast.toast, fading: false };
      publish();
    },
    release(id, by) {
      const toast = shown.get(id);
      if (toast?.kept.delete(by) !== true) return;
      countDown(id, toast);
    },
    stop() {
      stopped = true;
      off();
      for (const toast of shown.values()) clearTimeout(toast.timer);
    }
  };
}
function concerned(about, core, board) {
  if (about === void 0) return "Board";
  if ("option" in about) return `Board option "${own(core.options.registered(), about.option)?.label ?? about.option}"`;
  if ("plugin" in about) return `Plugin "${own(core.plugins.appPlugins(), about.plugin)?.name ?? about.plugin}"`;
  if ("stack" in about) return `Stack "${own(board.stacks, about.stack)?.name ?? about.stack}"`;
  const block = (id) => `"${own(board.blocks, id)?.name ?? id}"`;
  if ("block" in about) return `Block ${block(about.block)}`;
  const link = own(board.links, about.link);
  return link === void 0 ? `Link "${about.link}"` : `Link from ${block(link.from)} to ${block(link.to)}`;
}
function sourcePackage(notification) {
  return notification.source.module?.package;
}
function own(record, key) {
  return Object.hasOwn(record, key) ? record[key] : void 0;
}
function ToastList({ core, toasts }) {
  const list = useSyncExternalStore5(toasts.subscribe, toasts.list);
  const board = useBoard7(core);
  if (list.length === 0) return null;
  return /* @__PURE__ */ jsx19("section", { className: "blockr-dock-toasts", "aria-label": "Notifications", children: list.map((toast) => /* @__PURE__ */ jsx19(
    ToastItem,
    {
      toast,
      title: concerned(toast.notification.about, core, board),
      toasts
    },
    toast.notification.id
  )) });
}
function ToastItem({ toast, title, toasts }) {
  const { notification, fading } = toast;
  const { id, severity } = notification;
  const pkg = sourcePackage(notification);
  return /* @__PURE__ */ jsxs13(
    "div",
    {
      className: "blockr-dock-toast",
      role: severity === "message" ? "status" : "alert",
      "data-severity": severity,
      "data-fading": fading ? "" : void 0,
      onPointerEnter: () => {
        toasts.keep(id, "pointer");
      },
      onPointerLeave: () => {
        toasts.release(id, "pointer");
      },
      onFocus: () => {
        toasts.keep(id, "focus");
      },
      onBlur: (event) => {
        if (!(event.relatedTarget instanceof Node && event.currentTarget.contains(event.relatedTarget))) {
          toasts.release(id, "focus");
        }
      },
      children: [
        /* @__PURE__ */ jsxs13("div", { className: "blockr-dock-toast-text", children: [
          /* @__PURE__ */ jsxs13("p", { className: "blockr-dock-toast-title", children: [
            title,
            pkg === void 0 ? null : /* @__PURE__ */ jsxs13("span", { className: "blockr-dock-toast-package", children: [
              " \xB7 ",
              pkg
            ] })
          ] }),
          /* @__PURE__ */ jsx19("p", { className: "blockr-dock-toast-message", children: plainText(notification.message) })
        ] }),
        /* @__PURE__ */ jsx19(
          "button",
          {
            type: "button",
            className: "blockr-dock-toast-close",
            "aria-label": "Close",
            onClick: () => {
              toasts.close(id);
            },
            children: /* @__PURE__ */ jsx19("span", { "aria-hidden": "true", children: "\xD7" })
          }
        )
      ]
    }
  );
}

// srcts/modules/dock/index.tsx
import { Fragment as Fragment7, jsx as jsx20, jsxs as jsxs14 } from "react/jsx-runtime";
var api = API;
var SHOW_PANEL = "blockr.dock-show_panel";
var frontend = {
  async render({ el, core, plugin, signal, args }) {
    const items = readNavbar(args);
    const settings = await readSettings(core);
    if (signal.aborted) return void 0;
    const win = el.ownerDocument.defaultView ?? window;
    const drawn = drawnPlugins(core);
    startOnBoard(core, plugin, [...drawn.keys()], urlView(win.location));
    const narrow = trackNarrow(win, settings.breakpoint);
    const release = demandShown(core, drawn, narrow);
    const busy = trackBusy(core, drawn, narrow, settings.spinnerDelay);
    const sidebar = createSidebar();
    const openFolders = createOpenFolders();
    const toasts = createToasts(core.notifications);
    const unbind = bindShortcuts(el.ownerDocument, core, () => barButtons(core, items));
    const root = createRoot(el);
    const shared = { core, plugin, drawn, tooltips: createTooltips() };
    flushSync(() => {
      root.render(
        /* @__PURE__ */ jsxs14(Fragment7, { children: [
          /* @__PURE__ */ jsx20(DockContext, { value: shared, children: /* @__PURE__ */ jsx20(Navbar, { items, busy, sidebar, mac: onMac(win), openFolders }) }),
          /* @__PURE__ */ jsx20(
            Dock,
            {
              core,
              plugin,
              settings,
              narrow,
              drawn,
              viewsMenu: holdsPart(items, "views"),
              sidebar
            }
          ),
          /* @__PURE__ */ jsx20(DockContext, { value: shared, children: /* @__PURE__ */ jsx20(OptionsSidebar, { sidebar }) }),
          /* @__PURE__ */ jsx20(ToastList, { core, toasts })
        ] })
      );
    });
    return () => {
      root.unmount();
      unbind();
      toasts.stop();
      busy.stop();
      release();
      narrow.stop();
    };
  },
  async show({ object: object2, core }) {
    const args = { panel: object2 };
    if (!core.commands.preflight(SHOW_PANEL, args)) return;
    await core.commands.run(SHOW_PANEL, args);
  }
};
var dock_default = {
  options: ["compact", "view_tabs", "sync_rails", "show_conditions", "dark_mode"],
  // TypeScript gives the objects in the JSON's arrays properties that may be
  // undefined, which no Json has, so it does not take the schema for a Json,
  // though it is one.
  state: { schema: dock_schema_default, default: defaultState },
  update,
  commands: dockCommands,
  frontend
};
export {
  api,
  dock_default as default
};
