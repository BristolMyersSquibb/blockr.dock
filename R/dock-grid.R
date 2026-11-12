# A view's geometry is a `dock_grid`, a compact description that does not
# depend on dockView.
# It holds the nesting, the orientation, the sizes of each branch and the
# focused panel, with no type tags and no volatile ids.
#
# Shape: a `dock_grid` carries an `orientation`, `children`, `sizes` and an
# optional `focus` and `rails`.
# A leaf holds its `panels` (a character vector) and its open `active` tab,
# and a branch holds `children` (nodes) and their `sizes`.
# The `rails` component is flat like `focus` rather than part of the recursion:
# a map keyed by edge, each holding the panels pinned there.
# The tree and the rails partition the panels the grid places -- a railed panel
# is absent from the tree, which is what puts it outside the splitview.
# The `children` and `sizes` are parallel, the `sizes` are 0-1 ratios summing
# to 1, and `focus` is a panel id or NULL.
# A branch nests, and its split direction alternates with depth (as dockView
# does), so only the root orientation is stored.
# Relative sizes ride through verbatim.

is_grid_leaf <- function(node) not_null(node[["panels"]])

even_sizes <- function(n) if (n) rep(1 / n, n) else numeric()

normalise_sizes <- function(sizes) {

  if (!length(sizes)) {
    return(numeric())
  }

  total <- sum(sizes)

  if (total > 0) sizes / total else sizes
}

new_dock_grid <- function(children = list(), sizes = NULL,
                          orientation = "horizontal", focus = NULL,
                          rails = NULL) {

  content <- list(orientation = orientation, children = children, sizes = sizes)

  if (not_null(focus)) {
    content[["focus"]] <- focus
  }

  if (length(rails)) {
    content[["rails"]] <- view_rail_set(rails)
  }

  canonicalize_grid(structure(content, class = "dock_grid"))
}

# The canonical form, computed directly on our shape: every branch's sizes are
# normalised to ratios summing to 1 (defaulting to an even split when absent),
# and a `focus` that no longer names a placed panel is dropped -- placed meaning
# in the tree or in a rail, since a rail's open tab can hold focus too. There
# are no volatile ids to reassign, so two grids of the same shape are
# `identical()`.
canonicalize_grid <- function(grid) {

  norm_children <- function(children, sizes) {

    n <- length(children)
    sizes <- normalise_sizes(if (length(sizes) == n) sizes else even_sizes(n))

    list(children = lapply(children, norm_node), sizes = sizes)
  }

  norm_node <- function(node) {

    if (is_grid_leaf(node)) {
      return(node)
    }

    norm_children(node[["children"]], node[["sizes"]])
  }

  root <- norm_children(grid[["children"]], grid[["sizes"]])

  grid[["children"]] <- root[["children"]]
  grid[["sizes"]] <- root[["sizes"]]

  # The tree and the rails partition what the grid places, so a panel a rail
  # claims leaves the tree.
  # Enforcing it here rather than at the call sites makes it a property of the
  # type: however a grid is built, no panel is in both.
  # An empty rail at its defaults says nothing -- every dock offers every edge
  # -- so it is dropped rather than stored, the same way a stale `focus` is.
  if (length(grid[["rails"]])) {
    grid[["rails"]] <- Filter(Negate(rail_is_default), grid[["rails"]])
    if (!length(grid[["rails"]])) grid[["rails"]] <- NULL
  }

  railed <- rail_panel_ids(grid[["rails"]])

  if (length(railed)) {

    kept <- prune_tree(
      grid[["children"]], grid[["sizes"]],
      setdiff(grid_tree_ids(grid), railed)
    )

    grid[["children"]] <- kept[["children"]]
    grid[["sizes"]] <- normalise_sizes(kept[["sizes"]])
  }

  if (not_null(grid[["focus"]]) && !grid[["focus"]] %in% grid_panel_ids(grid)) {
    grid[["focus"]] <- NULL
  }

  grid
}

# All panel ids a grid places: its tree, in reading order (root children first,
# depth-first), then whatever its rails hold. The two partition the set, so a
# panel appears exactly once.
grid_panel_ids <- function(grid) {
  c(grid_tree_ids(grid), rail_panel_ids(grid[["rails"]]))
}

# Just the tree half -- what the splitview arranges.
grid_tree_ids <- function(grid) {

  collect <- function(node) {
    if (is_grid_leaf(node)) {
      node[["panels"]]
    } else {
      unlst(lapply(node[["children"]], collect))
    }
  }

  as.character(unlst(lapply(grid[["children"]], collect)))
}

#' Canonical view grid
#'
#' A `dock_grid` is a view's geometry in a compact form that does not depend on
#' dockView: nested splits and tab groups with sizes normalised to 0-1 ratios
#' and no volatile ids, so two grids of the same shape compare `identical()`.
#' It is written with [dock_grid()][layout].
#' The function `as_dock_grid()` casts another `dock_grid` (identity) or a list
#' of the same shape into one, and is idempotent.
#' The check `is_dock_grid()` tests the class, and `validate_dock_grid()`
#' returns its input and errors on a malformed or non-canonical grid, or on one
#' that places a panel more than once.
#'
#' @param x Object to cast (a `dock_grid` or a list), validate, or test.
#' @param ... Passed on to methods.
#' @return `as_dock_grid()` and `validate_dock_grid()` a `dock_grid`;
#'   `is_dock_grid()` a boolean.
#' @name dock-grid
#' @export
as_dock_grid <- function(x, ...) {
  UseMethod("as_dock_grid")
}

#' @rdname dock-grid
#' @export
is_dock_grid <- function(x) {
  inherits(x, "dock_grid")
}

#' @rdname dock-grid
#' @export
validate_dock_grid <- function(x) {

  if (!is_dock_grid(x)) {
    blockr_abort(
      "Expecting a `dock_grid` object.",
      class = "dock_grid_structure_invalid"
    )
  }

  if (!is.list(x) ||
        !all(c("orientation", "children", "sizes") %in% names(x))) {
    blockr_abort(
      "A `dock_grid` must carry `orientation`, `children` and `sizes`.",
      class = "dock_grid_structure_invalid"
    )
  }

  unexpected <- setdiff(
    names(x), c("orientation", "children", "sizes", "focus", "rails")
  )

  if (length(unexpected)) {
    blockr_abort(
      "Unexpected `dock_grid` component{?s} {unexpected}.",
      class = "dock_grid_structure_invalid"
    )
  }

  validate_grid_rails(x[["rails"]])

  if (!grid_sizes_canonical(x)) {
    blockr_abort(
      "A `dock_grid` must be canonical: each branch's sizes sum to 1.",
      class = "dock_grid_not_canonical"
    )
  }

  # A grid says where each panel goes, so a panel it places twice -- twice in
  # the tree, twice in one rail, or once in each of two rails -- has no
  # arrangement to express.
  # The tree/rail overlap `canonicalize_grid()` prunes has a principled winner
  # (the rail claims the panel), while two occurrences of the same kind have
  # none, so first-wins would apply an arbitrary choice quietly to someone's
  # layout.
  # Hence a rejection here rather than a dedup there, where the grid is
  # written.
  placed <- grid_panel_ids(x)
  dup <- unique(placed[duplicated(placed)])

  if (length(dup)) {
    blockr_abort(
      "A `dock_grid` places each panel once, but {dup} {?is/are} placed more \\
       than once.",
      class = "dock_grid_panel_duplicated"
    )
  }

  invisible(x)
}

#' @export
as_dock_grid.dock_grid <- function(x, ...) x

#' @export
as_dock_grid.list <- function(x, ...) {

  new_dock_grid(
    children = coal(x[["children"]], list(), fail_all = FALSE),
    sizes = x[["sizes"]],
    orientation = coal(x[["orientation"]], "horizontal", fail_all = FALSE),
    focus = x[["focus"]],
    rails = lapply(x[["rails"]], as_dock_rail)
  )
}

#' @export
as.list.dock_grid <- function(x, ...) {

  x <- unclass(x)
  x[["rails"]] <- lapply(x[["rails"]], unclass)

  x
}

# Each branch splits its extent among its direct children, so their sizes are
# ratios summing to 1; a leaf carries no children to constrain. Empty branches
# (no children) are vacuously canonical.
grid_sizes_canonical <- function(grid) {

  ok <- TRUE

  walk <- function(children, sizes) {

    if (length(children) && !isTRUE(all.equal(sum(sizes), 1))) {
      ok <<- FALSE
    }

    for (node in children) {
      if (!is_grid_leaf(node)) {
        walk(node[["children"]], node[["sizes"]])
      }
    }
  }

  walk(grid[["children"]], grid[["sizes"]])

  ok
}

#' @export
str_value.dock_grid <- function(x, ...) {
  str_value_ids(x, "dock_grid")
}

#' @importFrom utils str
#' @export
str.dock_grid <- function(object, ...) {
  cat(" ", str_value(object), "\n", sep = "")
  invisible(object)
}

#' @export
format.dock_grid <- function(x, ..., bare = TRUE) {
  format_grid_tree(x, "dock_grid", bare)
}

#' @export
print.dock_grid <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

# Keep only the leaf panels in `members`, dropping any leaf or branch left
# empty.
# The function `canonicalize_grid()` prunes what the rails have claimed with it.
prune_tree <- function(children, sizes, members) {

  prune <- function(children, sizes) {

    keep <- lgl_ply(children, function(node) not_null(prune_node(node)))

    list(
      children = lapply(children[keep], prune_node),
      sizes = sizes[keep]
    )
  }

  prune_node <- function(node) {

    if (is_grid_leaf(node)) {

      kept <- intersect(node[["panels"]], members)

      if (!length(kept)) {
        return(NULL)
      }

      active <- node[["active"]]
      keep_active <- not_null(active) && active %in% kept

      list(
        panels = kept,
        active = if (keep_active) active else kept[[1L]]
      )
    } else {

      pruned <- prune(node[["children"]], node[["sizes"]])

      if (!length(pruned[["children"]])) {
        return(NULL)
      }

      pruned
    }
  }

  prune(children, sizes)
}

#' Dock grid authoring
#'
#' A [dock_grid][dock-grid] is a view's geometry: the arrangement of its
#' panels into nested splits and tab groups, with sizes.
#' The function `dock_grid()` is the authoring DSL that builds one.
#'
#' Construct a grid with:
#'
#' * `dock_grid(...)`: the view-level container. Its `...` are the
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
#' * `rail(..., position = "left")`: a tab group pinned to one edge of
#'   the view rather than arranged by the splitview. Write it among the
#'   grid's children; it is not one, so it never counts towards `sizes`,
#'   and the members it names leave the grid tree.
#'
#' The function `dock_grid()` accepts `orientation = "horizontal" |
#' "vertical"` for the top-level split direction and `sizes` for the
#' root-branch ratios.
#'
#' @param ... For `dock_grid()` and `group()`, grid children (`blk()`
#'   references, bare ids, character vectors, lists, `panels()`, or
#'   `group()`).
#'   For `panels()` and `rail()`, panel references or ids.
#' @param orientation Top-level split direction; one of `"horizontal"`
#'   (default) or `"vertical"`.
#' @param active For `panels()` and `rail()`, the id of the tab to open by
#'   default.
#' @param position For `rail()`, the edge the rail pins to; `"left"` (the
#'   default) or `"right"`.
#' @param collapsed For `rail()`, whether it opens collapsed to its bare tab
#'   strip. A user collapses a rail by clicking its open tab, and that choice
#'   is stored, so a restored board comes back the way it was left.
#' @param size,collapsed_size For `rail()`, its width (or height, on a
#'   horizontal edge) in pixels when open and when collapsed to its bare tab
#'   strip.
#' @param sizes Numeric vector parallel to `...`, giving each child's
#'   share of the parent (positive; need not sum to 1).
#'
#' @examples
#' # Panels named with typed references; bare id strings work too
#' panels(blk("a"), blk("b"), active = blk("b"))
#'
#' # Branch with explicit child ratios
#' group(blk("a"), blk("b"), sizes = c(0.3, 0.7))
#'
#' # A block panel beside a tabbed block leaf
#' dock_grid(
#'   blk("a"),
#'   panels(blk("b"), blk("c"), active = blk("c")),
#'   sizes = c(0.3, 0.7)
#' )
#'
#' # Vertical top-level split
#' dock_grid(blk("a"), blk("b"), orientation = "vertical")
#'
#' # A block parked on the left edge, out of the splitview
#' dock_grid(blk("a"), rail(blk("b"), position = "left"))
#'
#' @return The function `dock_grid()` returns a [dock_grid][dock-grid] object,
#' `panels()` a `dock_panels` node and `group()` a `dock_group` node, both
#' grid sub-trees usable inside `dock_grid()` / `group()`.
#' A `rail()` is a `dock_rail`, written among a grid's children but pinned
#' outside the splitview.
#'
#' @rdname layout
#' @export
dock_grid <- function(..., orientation = c("horizontal", "vertical"),
                      sizes = NULL) {

  orientation <- match.arg(orientation)
  nodes <- list(...)

  # A `rail()` sits among the children as written, but it is not one: it is
  # pinned outside the splitview, so it never counts towards `sizes`.
  railed <- lgl_ply(nodes, is_dock_rail)
  children <- nodes[!railed]

  validate_sizes(sizes, children)

  new_dock_grid(
    children = lapply(children, build_grid_node),
    sizes = sizes,
    orientation = orientation,
    rails = nodes[railed]
  )
}

#' @rdname layout
#' @export
panels <- function(..., active = NULL) {
  ids <- chr_ply(list(...), as_panel_string)

  if (!is.null(active)) {
    active <- as_panel_string(active)
  }

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

# Render a `dock_grid` as an indented tree under a `<label>` header, reading
# our compact shape directly.
format_grid_tree <- function(x, label, bare) {

  orientation <- coal(x[["orientation"]], "horizontal", fail_all = FALSE)

  header <- paste0("<", label, "> ", orientation)
  children <- x[["children"]]

  if (!length(children)) {
    return(paste0(header, " (empty)"))
  }

  c(
    header,
    format_dock_nodes(
      children, x[["sizes"]], orientation, x[["focus"]], bare,
      prefix = ""
    )
  )
}

str_value_ids <- function(x, label) {

  ids <- panel_obj_ids(grid_panel_ids(x))

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

  if (is_grid_leaf(node)) {

    if (length(node[["panels"]]) == 1L) {

      id <- node[["panels"]][[1L]]

      paste0(
        line_prefix,
        dock_panel_label(id, bare),
        dock_attrs(dock_size_label(size), dock_focus_label(id, focus))
      )

    } else {

      format_dock_tabs(node, size, focus, bare, line_prefix, child_prefix)
    }

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

# Build a compact grid node from a user-facing DSL spec. Accepts:
#   - character (single or vector) → leaf (single id, or tabs for a vector)
#   - dock_panels                  → leaf with the given active tab
#   - dock_group                   → branch with the given sizes
#   - bare list                    → branch (even split)
build_grid_node <- function(node) {

  if (is_panel_ref(node)) {
    node <- as_panel_string(node)
  }

  if (is.character(node)) {
    return(list(panels = node, active = node[[1L]]))
  }

  if (is_dock_panels(node)) {

    ids <- chr_ply(node[["views"]], identity)

    return(
      list(
        panels = ids,
        active = coal(node[["active"]], ids[[1L]], fail_all = FALSE)
      )
    )
  }

  if (is_dock_group(node)) {
    return(
      list(
        children = lapply(node[["children"]], build_grid_node),
        sizes = coal(node[["sizes"]], even_sizes(length(node[["children"]])),
                     fail_all = FALSE)
      )
    )
  }

  if (is.list(node)) {
    return(
      list(
        children = lapply(node, build_grid_node),
        sizes = even_sizes(length(node))
      )
    )
  }

  blockr_abort(
    "Unknown layout node type: {class(node)}.",
    class = "dock_layout_node_invalid"
  )
}
