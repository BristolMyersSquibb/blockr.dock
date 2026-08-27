#' Dock layout: dockView's native representation
#'
#' A `dock_layout` is dockView's own layout representation -- the
#' `{grid, panels, activeGroup}` payload it renders -- the wire form at the
#' client boundary. It is distinct from our [dock_grid][dock-grid] (a view's
#' canonical geometry) and [dock_view()][dock_view] (a view's structure): a
#' `dock_layout` is what the browser echoes and what a restore pushes back,
#' geometry fused with resolved panel content in dockView's own shape. It is
#' produced and consumed only at the dockView seam and is never stored on the
#' board nor passed through the update lifecycle.
#'
#' `new_dock_layout()` wraps a raw dockView state / payload list (a client
#' echo) as a `dock_layout`; `is_dock_layout()` is the class check;
#' `validate_dock_layout()` returns its input and errors on a malformed shape.
#' Cast across the seam with `as_dock_layout()` (build the dockView payload
#' from a [dock_grid][dock-grid] against the board's blocks and extensions),
#' [as_dock_grid()] (canonical geometry from a layout), `as_dock_rails()` (the
#' rails a layout pins to its edges) and [as_dock_view()][dock_view]
#' (membership from a layout, the union of its grid and its rails).
#'
#' @param x Object to wrap, validate, test, or cast.
#' @param ... Passed on to methods (e.g. `blocks` / `extensions` to resolve
#'   panel content when casting a `dock_grid`).
#'
#' @return `new_dock_layout()`, `validate_dock_layout()` and `as_dock_layout()`
#'   return a `dock_layout`; `is_dock_layout()` a boolean.
#'
#' @name dock-layout
#' @export
new_dock_layout <- function(x = list()) {
  structure(x, class = "dock_layout")
}

#' @rdname dock-layout
#' @export
is_dock_layout <- function(x) {
  inherits(x, "dock_layout")
}

#' @rdname dock-layout
#' @export
validate_dock_layout <- function(x) {

  if (!is_dock_layout(x) || !is.list(x)) {
    blockr_abort(
      "Expecting a `dock_layout` object.",
      class = "dock_layout_structure_invalid"
    )
  }

  if (!"grid" %in% names(x)) {
    blockr_abort(
      "A `dock_layout` must carry a `grid` component.",
      class = "dock_layout_structure_invalid"
    )
  }

  active <- x[["activeGroup"]]

  if (not_null(active) && !is_string(active)) {
    blockr_abort(
      "A `dock_layout` `activeGroup` must be a string or `NULL`.",
      class = "dock_layout_structure_invalid"
    )
  }

  if ("panels" %in% names(x) && !is.list(x[["panels"]])) {
    blockr_abort(
      "A `dock_layout` `panels` component must be a list.",
      class = "dock_layout_structure_invalid"
    )
  }

  invisible(x)
}

#' @rdname dock-layout
#' @export
as_dock_layout <- function(x, ...) {
  UseMethod("as_dock_layout")
}

#' @export
as_dock_layout.dock_layout <- function(x, ...) x

#' @export
as_dock_layout.list <- function(x, ...) {
  validate_dock_layout(new_dock_layout(x))
}

#' @export
as_dock_layout.dock_grid <- function(x, blocks = list(), extensions = list(),
                                     rails = list(), ...) {

  blocks <- as_blocks(blocks)
  ext_coll <- as_dock_extensions(extensions)

  panel_ids <- c(grid_panel_ids(x), rail_panel_ids(rails))

  blk_pids <- panel_ids[maybe_block_panel_id(panel_ids)]
  ext_pids <- panel_ids[maybe_ext_panel_id(panel_ids)]

  blk_ids <- as_obj_id(new_block_panel_id(blk_pids))
  ext_ids <- as_obj_id(new_ext_panel_id(ext_pids))

  blk_panels <- lapply(split(blocks[blk_ids], seq_along(blk_ids)), block_panel)
  ext_panels <- lapply(split(ext_coll[ext_ids], seq_along(ext_ids)), ext_panel)

  panels <- lapply(c(blk_panels, ext_panels), create_layout_panel)
  names(panels) <- chr_xtr(panels, "id")

  tree <- grid_to_tree(x)

  out <- list(grid = tree, panels = panels)

  if (length(panel_ids)) {
    out[["activeGroup"]] <- coal(
      focus_group_id(tree, x[["focus"]]), "1", fail_all = FALSE
    )
  }

  if (length(rails)) {
    out[["edgeGroups"]] <- rails_to_edges(rails)
  }

  new_dock_layout(out)
}

#' @export
as_dock_grid.dock_layout <- function(x, ...) {
  tree_to_grid(
    x[["grid"]],
    coal(x[["activeGroup"]], x[["active_group"]], fail_all = FALSE)
  )
}

#' @rdname dock-layout
#' @export
as_dock_rails <- function(x, ...) {
  UseMethod("as_dock_rails")
}

#' @export
as_dock_rails.dock_layout <- function(x, ...) {

  edges <- x[["edgeGroups"]]

  if (!length(edges)) {
    return(list())
  }

  set_names(map(edge_to_rail, edges, names(edges)), names(edges))
}

# The dockView `edgeGroups` payload: one entry per rail, keyed by the edge it
# pins to. Visibility is derived here rather than stored -- a rail holding
# panels is shown, an empty one hidden -- so a restore lands on exactly the
# rule the client re-asserts on every panel add, remove and move.
rails_to_edges <- function(rails) {
  set_names(lapply(rails, rail_to_edge), chr_xtr(rails, "position"))
}

rail_to_edge <- function(rail) {

  panels <- rail[["panels"]]

  group <- list(
    views = as.list(panels),
    id = rail_group_id(rail[["position"]])
  )

  if (length(panels)) {
    group[["activeView"]] <- coal(
      rail[["active"]], panels[[1L]], fail_all = FALSE
    )
  }

  c(
    list(
      visible = length(panels) > 0L,
      collapsed = isTRUE(rail[["collapsed"]]),
      group = group
    ),
    if (not_null(rail[["size"]])) {
      list(size = rail[["size"]])
    },
    if (not_null(rail[["collapsed_size"]])) {
      list(collapsedSize = rail[["collapsed_size"]])
    }
  )
}

edge_to_rail <- function(edge, position) {

  group <- edge[["group"]]

  new_dock_rail(
    position,
    as.character(unlst(group[["views"]])),
    active = group[["activeView"]],
    collapsed = isTRUE(edge[["collapsed"]]),
    size = edge[["size"]],
    collapsed_size = edge[["collapsedSize"]]
  )
}

# A rail's dockView group id. The grid's ids are minted as consecutive integers
# by `grid_to_tree()`, so keying off the edge keeps the two apart and lets a
# `position = list(referenceGroup = )` name a rail without consulting the tree.
rail_group_id <- function(position) {
  paste0("rail-", position)
}

#' @export
as.list.dock_layout <- function(x, ...) {
  unclass(x)
}

#' @param ... Forwarded to methods.
#' @rdname view
#' @export
as_dock_view <- function(x, ...) {
  UseMethod("as_dock_view")
}

#' @export
as_dock_view.dock_view <- function(x, ...) x

#' @export
as_dock_view.dock_layout <- function(x, ...) {
  new_dock_view(
    c(grid_panel_ids(as_dock_grid(x)), rail_panel_ids(as_dock_rails(x)))
  )
}

# Expand our compact grid into dockView's native tree: assign type tags and
# fresh deterministic group ids, hoisting the root branch under a
# `{root, orientation}` wrapper. Inverse of `tree_to_grid()`.
grid_to_tree <- function(grid) {

  counter <- new.env(parent = emptyenv())
  counter$n <- 0L

  next_id <- function() {
    counter$n <- counter$n + 1L
    as.character(counter$n)
  }

  to_tree <- function(node, size) {

    if (is_grid_leaf(node)) {
      return(
        list(
          type = "leaf",
          data = list(
            views = as.list(node[["panels"]]),
            activeView = coal(node[["active"]], node[["panels"]][[1L]],
                              fail_all = FALSE),
            id = next_id()
          ),
          size = size
        )
      )
    }

    list(
      type = "branch",
      data = map(to_tree, node[["children"]], node[["sizes"]]),
      size = size
    )
  }

  list(
    root = list(
      type = "branch",
      data = map(to_tree, grid[["children"]], grid[["sizes"]]),
      size = 1
    ),
    orientation = toupper(
      coal(grid[["orientation"]], "horizontal", fail_all = FALSE)
    )
  )
}

# Collapse dockView's native tree into our compact grid: drop type tags and
# volatile ids, resolve the focused group to its panel. Inverse of
# `grid_to_tree()`. A leaf root (a single-group page a live dock can echo) is
# lifted to the sole child of the compact root.
tree_to_grid <- function(tree, active_group = NULL) {

  from_tree <- function(node) {

    if (identical(node[["type"]], "leaf")) {

      views <- as.character(unlst(node[["data"]][["views"]]))

      return(
        list(
          panels = views,
          active = coal(node[["data"]][["activeView"]], views[[1L]],
                        fail_all = FALSE)
        )
      )
    }

    kids <- node[["data"]]

    list(children = lapply(kids, from_tree), sizes = dbl_xtr(kids, "size"))
  }

  root <- tree[["root"]]

  if (is.null(root) || !length(root)) {
    return(new_dock_grid())
  }

  leaf_root <- identical(root[["type"]], "leaf")
  kids <- if (leaf_root) list(root) else root[["data"]]

  new_dock_grid(
    children = lapply(kids, from_tree),
    sizes = if (leaf_root) 1 else dbl_xtr(kids, "size"),
    orientation = tolower(
      coal(tree[["orientation"]], "horizontal", fail_all = FALSE)
    ),
    focus = focus_panel(tree, active_group)
  )
}

# The open panel of a dockView group (`activeGroup` id -> its active view).
# NULL for the load-default first group ("1").
focus_panel <- function(tree, active_group) {

  if (is.null(active_group) || identical(active_group, "1")) {
    return(NULL)
  }

  for (leaf in grid_leaves(tree)) {
    if (identical(leaf[["id"]], active_group)) {
      return(leaf[["activeView"]])
    }
  }

  NULL
}

# The dockView group holding `focus` (a panel id -> its `activeGroup` id), so
# a focused panel can be restored as the active group.
focus_group_id <- function(tree, focus) {

  if (is.null(focus)) {
    return(NULL)
  }

  for (leaf in grid_leaves(tree)) {
    if (focus %in% unlist(leaf[["views"]])) {
      return(leaf[["id"]])
    }
  }

  NULL
}

# All leaf `data` records of a dockView tree, in reading order.
grid_leaves <- function(tree) {

  leaves <- list()

  walk <- function(node) {

    if (is.null(node) || !length(node)) {
      return(invisible())
    }

    if (identical(node[["type"]], "leaf")) {
      leaves[[length(leaves) + 1L]] <<- node[["data"]]
    } else {
      lapply(node[["data"]], walk)
    }

    invisible()
  }

  walk(tree[["root"]])

  leaves
}
