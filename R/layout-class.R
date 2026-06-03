#' Dock layout
#'
#' A `dock_layout` is the panel arrangement for a single view: a tree of
#' block / extension IDs, with at most one leaf marked as initially
#' active. A board holds a `dock_layouts` collection (one layout per
#' view); panel content is derived on demand from the board's blocks and
#' extensions, so only the arrangement is stored in a `dock_layout`. See
#' [is_dock_layouts()] for the collection-level helpers.
#'
#' Construct a layout with:
#'
#' * `dock_layout(...)`: the page-level container. Its `...` are the
#'   children of the root branch. Bare strings become single-panel
#'   leaves, character vectors become tabbed leaves, lists become
#'   nested branches. Use [panels()] for a tabbed leaf with an explicit
#'   open tab, and [group()] for a branch with explicit sizes.
#' * `panels(..., active = NULL)`: a tabbed leaf whose tab strip holds
#'   the given panel IDs. `active` selects the initially-open tab; the
#'   first ID wins by default. A single-panel `panels()` is permitted
#'   but redundant (a bare string is equivalent).
#' * `group(..., sizes = NULL)`: a branch container. `sizes` is a
#'   numeric vector parallel to `...` that overrides the even split.
#' * `default_layout(blocks, extensions)` produces the default two-row
#'   arrangement (extensions on top, blocks below) for a board.
#'
#' `dock_layout()` accepts `orientation = "horizontal" | "vertical"`
#' for the top-level split direction, `sizes` for the root-branch
#' ratios, and `name` for the view's display label. In
#' `new_dock_board(layouts = list(...))` the list name is the view's
#' stable *id* (the container's key, like a block id), minted when
#' absent; `name` sets the free-form display label on the view itself.
#' When no name is given, one is derived from the id for display. Which
#' view starts active is a property of the collection, not of any one
#' layout: pass `new_dock_board(active = )` (a view id) to choose it,
#' defaulting to the first.
#'
#' A *view* is the conceptual page-level container; a *layout* is the
#' panel arrangement inside a view. The dockview-shape `grid + panels`
#' payload that dockViewR consumes is an internal projection of a
#' `dock_layout` against the board's blocks and extensions; it is not a
#' public type.
#'
#' `as_dock_layout()` coerces to a `dock_layout`: a `dock_layout`
#' (identity), a `board` (its active layout), or a spec list (`as.list()`
#' of a layout, or a parsed [layout_to_json()] string). Pass `blocks` /
#' `extensions` to resolve bare IDs to canonical panel IDs and validate.
#' `as.list()` of a `dock_layout` returns that spec list. The JSON-string
#' boundary is [layout_to_json()] / [layout_from_json()].
#'
#' @param ... For `dock_layout()` and `group()`, layout children (bare
#'   IDs, character vectors, lists, `panels()`, or `group()`). For
#'   `panels()`, panel IDs. Otherwise reserved for generic consistency.
#' @param orientation Top-level split direction; one of `"horizontal"`
#'   (default) or `"vertical"`.
#' @param active For `panels()`, the ID of the tab to open by default.
#' @param sizes Numeric vector parallel to `...`, giving each child's
#'   share of the parent (positive; need not sum to 1).
#' @param name For `dock_layout()`, an optional display label for the
#'   view (free-form). When omitted, a label is derived from the view's
#'   id. The view's id is the list name in
#'   `new_dock_board(layouts = list(...))`, minted when absent and unique
#'   across the views of a `dock_layouts`.
#' @param blocks,extensions Dock board components. For `default_layout()`
#'   the components to arrange; for `as_dock_layout()`, optional, used to
#'   resolve bare IDs and validate the result.
#'
#' @examples
#' blks <- c(
#'   a = blockr.core::new_dataset_block(),
#'   b = blockr.core::new_head_block()
#' )
#'
#' exts <- list(
#'   edit = new_edit_board_extension()
#' )
#'
#' # The default arrangement for a given set of blocks and extensions
#' default_layout(blks, exts)
#'
#' # Tabbed leaf with an explicit open tab
#' panels("a", "b", "edit_board_extension", active = "edit_board_extension")
#'
#' # Branch with explicit child ratios
#' group("a", "b", sizes = c(0.3, 0.7))
#'
#' # Composing them inside a layout
#' dock_layout(
#'   "a",
#'   panels("b", "edit_board_extension", active = "edit_board_extension"),
#'   sizes = c(0.3, 0.7)
#' )
#'
#' # Vertical top-level split
#' dock_layout("a", "b", orientation = "vertical")
#'
#' @return `dock_layout()` and `default_layout()` return a `dock_layout`
#' object. `panels()` returns a `dock_panels` node and `group()` returns
#' a `dock_group` node — both are layout sub-trees usable inside
#' `dock_layout()` / `group()`. `as_dock_layout()` returns a
#' `dock_layout` (from a board or a spec list); `as.list()` of a
#' `dock_layout` returns the spec list. `is_dock_layout()` returns a
#' boolean. `validate_dock_layout()` returns its input and throws on
#' error.
#'
#' @rdname layout
#' @export
dock_layout <- function(..., orientation = c("horizontal", "vertical"),
                        sizes = NULL, name = NULL) {

  orientation <- match.arg(orientation)
  children <- list(...)

  validate_sizes(sizes, children)

  root_spec <- if (length(children) == 0L) {
    NULL
  } else {
    new_dock_group(children, sizes)
  }

  res <- new_dock_layout(
    grid = build_grid_tree(root_spec, orientation = orientation)
  )

  if (!is.null(name)) {
    view_name(res) <- name
  }

  res
}

#' @rdname layout
#' @export
panels <- function(..., active = NULL) {
  ids <- chr_ply(list(...), identity)

  if (length(ids) && !is.null(active)) {

    if (!is_string(active) || !active %in% ids) {
      blockr_abort(
        "`active` must be one of the panel IDs.",
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

new_dock_layout <- function(grid = NULL, active_group = NULL) {

  if (is.null(grid)) {
    grid <- build_grid_tree(NULL)
  }

  content <- list(grid = grid)

  if (length(grid_panel_ids(grid))) {
    content[["activeGroup"]] <- coal(active_group, "1")
  }

  validate_dock_layout(structure(content, class = "dock_layout"))
}

#' @param x Object
#' @rdname layout
#' @export
is_dock_layout <- function(x) {
  inherits(x, "dock_layout")
}

#' @rdname layout
#' @export
as_dock_layout <- function(x, ...) {
  UseMethod("as_dock_layout")
}

#' @export
as_dock_layout.dock_layout <- function(x, ...) x

#' @export
as_dock_layout.board <- function(x, ...) {
  active_layout(x)
}

#' @export
as_dock_layout.list <- function(x, blocks = NULL, extensions = NULL, ...) {

  if ("grid" %in% names(x) && !"children" %in% names(x)) {
    blockr_abort(
      paste(
        "This looks like dockview's internal layout, which is not a public",
        "input. Pass a layout spec (see `?layout_to_json`) instead."
      ),
      class = "dock_layout_dockview_input"
    )
  }

  layout <- spec_to_layout(x)

  if (!is.null(blocks) || !is.null(extensions)) {
    layout <- resolve_dock_layout(
      blocks = coal(blocks, list()),
      extensions = coal(extensions, list()),
      layout = layout
    )
  }

  layout
}

#' @export
as.list.dock_layout <- function(x, ...) {
  layout_to_spec(x)
}

#' @param bare For `format()` / `print()`, drop the `block_panel-` /
#'   `ext_panel-` prefixes from panel IDs (see [panel_obj_ids()]).
#' @rdname layout
#' @export
format.dock_layout <- function(x, ..., bare = TRUE) {

  spec <- as.list(x)
  orientation <- coal(spec[["orientation"]], "horizontal", fail_all = FALSE)

  header <- paste0("<dock_layout> ", orientation)
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

#' @rdname layout
#' @export
print.dock_layout <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
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
      "Encountered an unknown layout node while formatting.",
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

#' @rdname layout
#' @export
default_layout <- function(blocks, extensions) {

  blks <- names(as_blocks(blocks))
  exts <- names(as_dock_extensions(extensions))

  spec <- list()
  if (length(exts)) spec <- c(spec, list(exts))
  if (length(blks)) spec <- c(spec, list(blks))

  do.call(dock_layout, spec)
}

#' @rdname layout
#' @export
validate_dock_layout <- function(x, blocks = character()) {

  if (is.null(x)) {
    return(x)
  }

  if (is_blocks(blocks)) {
    blocks <- names(blocks)
  }

  if (!is.list(x) || !is_dock_layout(x)) {
    blockr_abort(
      "Expecting the `layout` component of a `dock_board` to be a list ",
      "inheriting from `dock_layout`.",
      class = "dock_layout_invalid"
    )
  }

  if (!"grid" %in% names(x)) {
    blockr_abort(
      "Expecting a `layout` to contain a `grid` component.",
      class = "dock_layout_invalid"
    )
  }

  unexpected <- setdiff(names(x), c("grid", "activeGroup"))

  if (length(unexpected)) {
    blockr_abort(
      "Not expecting `layout` component{?s} {unexpected}.",
      class = "dock_layout_invalid"
    )
  }

  panel_ids <- layout_panel_ids(x)

  is_blk_pn <- maybe_block_panel_id(panel_ids)
  is_ext_pn <- maybe_ext_panel_id(panel_ids)

  raw_ids <- !(is_blk_pn | is_ext_pn)

  if (length(blocks) && any(raw_ids)) {
    blockr_abort(
      "Malformed layout panel ID{?s} {panel_ids[raw_ids]}.",
      class = "dock_layout_invalid"
    )
  }

  if (length(blocks)) {
    extra <- setdiff(panel_ids[is_blk_pn], as_block_panel_id(blocks))

    if (length(extra)) {
      blockr_abort(
        "Unknown layout panel{?s} {extra}.",
        class = "dock_layout_invalid"
      )
    }
  }

  x
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
      child_sizes <- coal(node[["sizes"]], empty_or_even(n))
      children <- Map(walk, node[["children"]], child_sizes)
      return(make_branch(children, size = size))
    }

    if (is.list(node)) {
      n <- length(node)
      child_sizes <- empty_or_even(n)
      children <- Map(walk, node, child_sizes)
      return(make_branch(children, size = size))
    }

    blockr_abort(
      "Unknown layout node type: {.cls {class(node)}}.",
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
  layout <- as_dock_layout(layout)
  grid_panel_ids(layout[["grid"]])
}

resolve_dock_layout <- function(blocks = list(), extensions = list(),
                                layout = default_layout(blocks, extensions)) {

  blocks <- as_blocks(blocks)
  ext_coll <- as_dock_extensions(extensions)

  layout <- as_dock_layout(layout)
  view_nm <- view_name(layout)

  ext_pid <- as_ext_panel_id(ext_coll)
  ext_key <- ext_alias_ids(ext_coll)
  ext_cls <- names(ext_coll)

  aliased <- ext_key != ext_cls

  id_map <- set_names(
    c(ext_pid, ext_pid[aliased], as_block_panel_id(blocks)),
    c(ext_key, ext_cls[aliased], names(blocks))
  )

  panel_ids <- grid_panel_ids(layout[["grid"]])

  if (length(panel_ids) &&
        all(panel_ids %in% names(id_map)) &&
        any(!panel_ids %in% id_map)) {

    if (anyDuplicated(names(id_map)) > 0L) {

      blockr_warn(
        "Cannot use extension names that overlap with block names.",
        class = "extension_block_name_clash"
      )

      spec <- list()
      if (length(ext_coll)) {
        spec <- c(spec, list(as.character(as_ext_panel_id(ext_coll))))
      }
      if (length(blocks)) {
        spec <- c(spec, list(as.character(as_block_panel_id(blocks))))
      }

      layout <- do.call(dock_layout, spec)

    } else {

      layout[["grid"]] <- rewrite_grid_leaves(layout[["grid"]], id_map)
    }
  }

  if (!is.null(view_nm)) {
    view_name(layout) <- view_nm
  }

  validate_dock_layout(layout, blocks)
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

dockview_payload <- function(layout, blocks = list(), extensions = list()) {

  layout <- as_dock_layout(layout)
  blocks <- as_blocks(blocks)
  ext_list <- as.list(as_dock_extensions(extensions))

  panel_ids <- grid_panel_ids(layout[["grid"]])

  blk_pids <- panel_ids[maybe_block_panel_id(panel_ids)]
  ext_pids <- panel_ids[maybe_ext_panel_id(panel_ids)]

  blk_ids <- as_obj_id(new_block_panel_id(blk_pids))
  ext_ids <- as_obj_id(new_ext_panel_id(ext_pids))

  blk_panels <- lapply(split(blocks[blk_ids], seq_along(blk_ids)), block_panel)
  ext_panels <- lapply(ext_list[ext_ids], ext_panel)

  panels <- lapply(c(blk_panels, ext_panels), create_layout_panel)
  names(panels) <- chr_xtr(panels, "id")

  out <- list(grid = layout[["grid"]], panels = panels)

  if (!is.null(layout[["activeGroup"]])) {
    out[["activeGroup"]] <- layout[["activeGroup"]]
  }

  out
}
