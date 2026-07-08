#' Dock grid authoring
#'
#' A [dock_grid][dock-grid] is a view's geometry: the arrangement of its
#' panels into nested splits and tab groups, with sizes. `dock_grid()` is the
#' authoring DSL that builds one; view *membership* (which panels belong) and
#' the display *name* live on the view (see [dock_view()][dock_view]), not the
#' grid.
#'
#' Construct a grid with:
#'
#' * `dock_grid(...)`: the page-level container. Its `...` are the
#'   children of the root branch. Bare strings become single-panel
#'   leaves, character vectors become tabbed leaves, lists become
#'   nested branches. Use [panels()] for a tabbed leaf with an explicit
#'   open tab, and [group()] for a branch with explicit sizes.
#' * `panels(..., active = NULL)`: a tabbed leaf whose tab strip holds
#'   the given panel ids. `active` selects the initially-open tab; the
#'   first id wins by default. A single-panel `panels()` is permitted
#'   but redundant (a bare string is equivalent).
#' * `group(..., sizes = NULL)`: a branch container. `sizes` is a
#'   numeric vector parallel to `...` that overrides the even split.
#' * `default_layout(blocks, extensions)` produces the default board
#'   arrangement (extensions on top, blocks below) as a `list(views, grids)`
#'   the constructor consumes.
#'
#' `dock_grid()` accepts `orientation = "horizontal" | "vertical"` for the
#' top-level split direction and `sizes` for the root-branch ratios. The
#' dockView-native `{grid, panels, activeGroup}` payload dockViewR consumes is a
#' [dock_layout][dock-layout], built from a grid against the board's blocks and
#' extensions. The JSON-string boundary is [layout_to_json()] /
#' [layout_from_json()].
#'
#' @param ... For `dock_grid()` and `group()`, grid children (bare ids,
#'   character vectors, lists, `panels()`, or `group()`). For `panels()`,
#'   panel ids. Otherwise reserved for generic consistency.
#' @param orientation Top-level split direction; one of `"horizontal"`
#'   (default) or `"vertical"`.
#' @param active For `panels()`, the id of the tab to open by default.
#' @param sizes Numeric vector parallel to `...`, giving each child's
#'   share of the parent (positive; need not sum to 1).
#' @param blocks,extensions Dock board components to arrange (for
#'   `default_layout()`).
#'
#' @examples
#' blks <- c(
#'   a = blockr.core::new_dataset_block(),
#'   b = blockr.core::new_head_block()
#' )
#'
#' # Tabbed leaf with an explicit open tab
#' panels("a", "b", "edit_board_extension", active = "edit_board_extension")
#'
#' # Branch with explicit child ratios
#' group("a", "b", sizes = c(0.3, 0.7))
#'
#' # Composing them inside a grid
#' dock_grid(
#'   "a",
#'   panels("b", "edit_board_extension", active = "edit_board_extension"),
#'   sizes = c(0.3, 0.7)
#' )
#'
#' # Vertical top-level split
#' dock_grid("a", "b", orientation = "vertical")
#'
#' @return `dock_grid()` returns a [dock_grid][dock-grid] object. `panels()`
#' returns a `dock_panels` node and `group()` returns a `dock_group` node --
#' both are grid sub-trees usable inside `dock_grid()` / `group()`.
#' `default_layout()` returns a `list` with `views` (a `dock_views`) and
#' `grids` (a `dock_grids`).
#'
#' @rdname layout
#' @export
dock_grid <- function(..., orientation = c("horizontal", "vertical"),
                      sizes = NULL) {

  orientation <- match.arg(orientation)
  children <- list(...)

  validate_sizes(sizes, children)

  root_spec <- if (length(children) == 0L) {
    NULL
  } else {
    new_dock_group(children, sizes)
  }

  new_dock_grid(build_grid_tree(root_spec, orientation = orientation))
}

#' @rdname layout
#' @export
panels <- function(..., active = NULL) {
  ids <- chr_ply(list(...), identity)

  if (length(ids) && !is.null(active)) {

    if (!is_string(active) || !active %in% ids) {
      blockr_abort(
        "`active` must be one of the panel ids.",
        class = "dock_panels_active_invalid"
      )
    }
  }

  new_dock_panels(ids, active = active)
}

#' @rdname layout
#' @export
group <- function(..., sizes = NULL) {
  children <- list(...)
  validate_sizes(sizes, children)
  new_dock_group(children, sizes)
}

new_dock_panels <- function(ids, active = NULL) {
  structure(
    list(views = as.list(ids), active = active),
    class = c("dock_panels", "dock_node")
  )
}

new_dock_group <- function(children, sizes = NULL) {
  structure(
    list(children = children, sizes = sizes),
    class = c("dock_group", "dock_node")
  )
}

is_dock_panels <- function(x) {
  inherits(x, "dock_panels")
}

is_dock_group <- function(x) {
  inherits(x, "dock_group")
}

is_dock_node <- function(x) {
  inherits(x, "dock_node")
}

validate_sizes <- function(sizes, children) {
  if (is.null(sizes)) {
    return(invisible(NULL))
  }

  if (!is.numeric(sizes) || length(sizes) != length(children) ||
        any(sizes <= 0)) {
    blockr_abort(
      paste(
        "`sizes` must be a positive numeric vector of length equal to",
        "the number of children."
      ),
      class = "dock_layout_sizes_invalid"
    )
  }

  invisible(NULL)
}

#' @rdname layout
#' @export
default_layout <- function(blocks, extensions) {

  blocks <- as_blocks(blocks)
  ext_coll <- as_dock_extensions(extensions)

  ext_pids <- as.character(as_ext_panel_id(ext_coll))
  blk_pids <- as.character(as_block_panel_id(blocks))

  spec <- list()
  if (length(ext_pids)) spec <- c(spec, list(ext_pids))
  if (length(blk_pids)) spec <- c(spec, list(blk_pids))

  grid <- do.call(dock_grid, spec)

  views <- new_dock_views(
    mint_view_ids(list(new_dock_view(layout_panel_ids(grid))))
  )

  list(
    views = views,
    grids = new_dock_grids(set_names(list(grid), names(views)))
  )
}

# Rewrite a grid's bare leaf ids to canonical panel ids via `id_map`. Only
# fully-bare grids are rewritten (see `resolve_panel_ids()`); a name clash
# between an extension and a block falls back to a default two-group grid.
resolve_grid <- function(grid, id_map) {

  grid <- as_dock_grid(grid)

  panel_ids <- layout_panel_ids(grid)

  bare <- length(panel_ids) && all(panel_ids %in% names(id_map)) &&
    any(!panel_ids %in% id_map)

  if (!bare) {
    return(grid)
  }

  if (anyDuplicated(names(id_map)) > 0L) {

    blockr_warn(
      "Cannot use extension names that overlap with block names.",
      class = "extension_block_name_clash"
    )

    return(clash_default_grid(id_map))
  }

  grid[["grid"]] <- rewrite_grid_leaves(grid[["grid"]], id_map)

  grid
}

clash_default_grid <- function(id_map) {

  ext_pids <- unique(id_map[maybe_ext_panel_id(id_map)])
  blk_pids <- unique(id_map[maybe_block_panel_id(id_map)])

  spec <- list()
  if (length(ext_pids)) spec <- c(spec, list(as.character(ext_pids)))
  if (length(blk_pids)) spec <- c(spec, list(as.character(blk_pids)))

  do.call(dock_grid, spec)
}

rewrite_grid_leaves <- function(grid, id_map) {

  rename_leaf <- function(leaf) {

    views <- chr_ply(leaf[["data"]][["views"]], identity)
    active <- leaf[["data"]][["activeView"]]

    leaf[["data"]][["views"]] <- as.list(
      chr_mply(coal, as.list(id_map)[views], views)
    )
    leaf[["data"]][["activeView"]] <- coal(id_map[[active]], active)

    leaf
  }

  grid_map_leaves(grid, rename_leaf)
}

# Shared tree renderer for a `dock_grid`: read the wire spec off the geometry
# and draw it under a `<label>` header.
format_grid_tree <- function(x, label, bare) {

  spec <- layout_to_spec(x)
  orientation <- coal(spec[["orientation"]], "horizontal", fail_all = FALSE)

  header <- paste0("<", label, "> ", orientation)
  children <- spec[["children"]]

  if (!length(children)) {
    return(paste0(header, " (empty)"))
  }

  c(
    header,
    format_dock_nodes(
      children, spec[["sizes"]], orientation, spec[["focus"]], bare,
      prefix = ""
    )
  )
}

str_value_ids <- function(x, label) {

  ids <- panel_obj_ids(layout_panel_ids(x))

  if (!length(ids)) {
    return(paste0("<", label, "> (empty)"))
  }

  paste0("<", label, "> ", paste0(ids, collapse = ", "))
}

flip_orientation <- function(x) {
  if (identical(x, "horizontal")) "vertical" else "horizontal"
}

format_dock_nodes <- function(children, sizes, orientation, focus, bare,
                              prefix) {

  n <- length(children)

  render_one <- function(i) {

    last <- i == n
    branch <- if (last) "\u2514\u2500 " else "\u251c\u2500 "
    indent <- if (last) "   " else "\u2502  "

    format_dock_node(
      children[[i]],
      orientation,
      if (length(sizes)) sizes[[i]] else NULL,
      focus,
      bare,
      paste0(prefix, branch),
      paste0(prefix, indent)
    )
  }

  unlst(lapply(seq_len(n), render_one))
}

format_dock_node <- function(node, orientation, size, focus, bare,
                             line_prefix, child_prefix) {

  if (is.character(node)) {

    paste0(
      line_prefix,
      dock_panel_label(node, bare),
      dock_attrs(dock_size_label(size), dock_focus_label(node, focus))
    )

  } else if (not_null(node[["panels"]])) {

    format_dock_tabs(node, size, focus, bare, line_prefix, child_prefix)

  } else if (not_null(node[["children"]])) {

    format_dock_branch(
      node, orientation, size, focus, bare, line_prefix, child_prefix
    )

  } else {

    blockr_abort(
      "Encountered an unknown grid node while formatting.",
      class = "dock_layout_format_invalid"
    )
  }
}

format_dock_branch <- function(node, orientation, size, focus, bare,
                               line_prefix, child_prefix) {

  orientation <- flip_orientation(orientation)

  c(
    paste0(
      line_prefix, "group",
      dock_attrs(orientation, dock_size_label(size))
    ),
    format_dock_nodes(
      node[["children"]], node[["sizes"]], orientation, focus, bare,
      child_prefix
    )
  )
}

format_dock_tabs <- function(node, size, focus, bare, line_prefix,
                             child_prefix) {

  tabs <- chr_ply(node[["panels"]], identity)
  active <- coal(node[["active"]], tabs[[1L]], fail_all = FALSE)
  n <- length(tabs)

  render_tab <- function(i) {

    branch <- if (i == n) "\u2514\u2500 " else "\u251c\u2500 "
    id <- tabs[[i]]

    paste0(
      child_prefix, branch,
      dock_panel_label(id, bare),
      dock_attrs(
        if (identical(id, active)) "active",
        dock_focus_label(id, focus)
      )
    )
  }

  c(
    paste0(line_prefix, "tabs", dock_attrs(dock_size_label(size))),
    unlst(lapply(seq_len(n), render_tab))
  )
}

dock_panel_label <- function(id, bare) {
  if (isTRUE(bare)) panel_obj_ids(id) else id
}

dock_size_label <- function(size) {
  if (is.null(size)) NULL else paste0(round(100 * size), "%")
}

dock_focus_label <- function(id, focus) {
  if (not_null(focus) && identical(id, focus)) "focus" else NULL
}

dock_attrs <- function(...) {

  tokens <- c(...)

  if (!length(tokens)) {
    return("")
  }

  paste0(" (", paste(tokens, collapse = ", "), ")")
}

# Build the dockview-shape grid tree (`{root, orientation}`) from a
# user-facing spec. Accepts:
#   - NULL                       → empty branch root
#   - character (single or vec)  → tabbed leaf (single elem ⇒ one tab)
#   - dock_panels                → tabbed leaf with the given active tab
#   - dock_group                 → branch with the given sizes
#   - bare list                  → branch (even split)
build_grid_tree <- function(spec, orientation = "horizontal") {

  group_id <- 0L

  next_id <- function() {
    group_id <<- group_id + 1L
    as.character(group_id)
  }

  make_leaf <- function(views, size, active_view = NULL) {
    list(
      type = "leaf",
      data = list(
        views = as.list(views),
        activeView = coal(active_view, views[[1L]]),
        id = next_id()
      ),
      size = size
    )
  }

  make_branch <- function(children, size) {
    list(
      type = "branch",
      data = filter_empty(children),
      size = size
    )
  }

  walk <- function(node, size = 1) {

    if (is.character(node)) {
      if (length(node) == 0L) {
        return(NULL)
      }
      return(make_leaf(node, size = size))
    }

    if (is_dock_panels(node)) {
      return(
        make_leaf(node[["views"]], size = size,
                  active_view = node[["active"]])
      )
    }

    if (is_dock_group(node)) {
      n <- length(node[["children"]])
      child_sizes <- normalise_sizes(coal(node[["sizes"]], empty_or_even(n)))
      children <- Map(walk, node[["children"]], child_sizes)
      return(make_branch(children, size = size))
    }

    if (is.list(node)) {
      n <- length(node)
      child_sizes <- normalise_sizes(empty_or_even(n))
      children <- Map(walk, node, child_sizes)
      return(make_branch(children, size = size))
    }

    blockr_abort(
      "Unknown layout node type: {class(node)}.",
      class = "dock_layout_node_invalid"
    )
  }

  if (is.null(spec)) {
    spec <- list()
  }

  list(
    root = walk(spec),
    orientation = toupper(orientation)
  )
}

empty_or_even <- function(n) {
  if (n) rep(1 / n, n) else numeric()
}

# Back-compat wrapper used by snapshot tests. Top-level character
# vectors split into separate leaves (legacy entry-point behaviour),
# matching the pre-Option B `draw_panel_tree()`.
draw_panel_tree <- function(x) {
  if (is.character(x)) {
    x <- as.list(x)
  }
  build_grid_tree(x)
}

grid_panel_ids <- function(grid) {
  chr_ply(unlst(lst_xtr(grid_leaves(grid), "views")), identity)
}

#' @rdname layout-json
#' @export
layout_panel_ids <- function(layout) {
  grid_panel_ids(as_dock_grid(layout)[["grid"]])
}

create_layout_panel <- function(x) {

  remove <- x[["remove"]]

  if (isTRUE(remove[["enable"]])) {
    tab_component <- "manual"
  } else {
    tab_component <- "custom"
  }

  if (isTRUE(remove[["enable"]]) && not_null(remove[["callback"]])) {
    remove_callback <- list(
      `__IS_FUNCTION__` = TRUE,
      source = unclass(remove[["callback"]])
    )
  } else {
    remove_callback <- NULL
  }

  c(
    x[c("id", "title")],
    list(
      contentComponent = "default",
      tabComponent = tab_component,
      params = list(
        content = list(html = c(x[["content"]][["html"]])),
        style = x[["style"]],
        removeCallback = remove_callback
      )
    )
  )
}
