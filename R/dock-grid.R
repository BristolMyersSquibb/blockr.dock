# A view's geometry has two representations. dockView's is a `dock_layout`:
# its native grid tree (type-tagged nodes, volatile group ids) fused with the
# resolved panels it renders. Ours is a `dock_grid`: a compact, dockView-
# independent description -- nesting, orientation, per-branch sizes, and the
# focused panel, with no type tags and no volatile ids. `dock_grid` is what we
# canonicalise, reason about, and persist; the two convert through
# `as_dock_layout()` (expand ours into dockView's, resolving panels) and
# `as_dock_grid()` (collapse dockView's back into ours). Swapping dockView
# touches only `dock_layout` and those two casts -- every stored grid is
# unaffected.
#
# Shape: a `dock_grid` carries an `orientation`, `children`, `sizes` and an
# optional `focus`. A leaf holds its `panels` (a character vector) and its open
# `active` tab; a branch holds `children` (nodes) and their `sizes`.
# `children` and `sizes` are parallel; `sizes` are 0-1 ratios summing to 1;
# `focus` is a panel id or NULL. A branch nests, and its split direction
# alternates with depth (as dockView does), so only the root orientation is
# stored. Relative sizes ride through verbatim (faithful for serialization and
# programmatic resize); the commit guard tolerates pixel-rounding jitter via
# `all.equal(tolerance = grid_size_tol())`, so a window resize is absorbed
# while a deliberate sash drag still commits.
#
# The grid holds geometry only. Which panels a view actually shows is its
# membership's call, resolved when the placement is read (`view_grid()`): the
# view's members drive, the grid supplies their arrangement, a member the grid
# omits is appended a default spot, and a ghost (a grid panel no longer a
# member) is dropped. Membership is authoritative; the grid never decides
# existence.

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
                          orientation = "horizontal", focus = NULL) {

  content <- list(orientation = orientation, children = children, sizes = sizes)

  if (not_null(focus)) {
    content[["focus"]] <- focus
  }

  canonicalize_grid(structure(content, class = "dock_grid"))
}

# The canonical form, computed directly on our shape: every branch's sizes are
# normalised to ratios summing to 1 (defaulting to an even split when absent),
# and a `focus` that no longer names a placed panel is dropped. There are no
# volatile ids to reassign, so two grids of the same shape are `identical()`.
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

  if (not_null(grid[["focus"]]) && !grid[["focus"]] %in% grid_panel_ids(grid)) {
    grid[["focus"]] <- NULL
  }

  grid
}

# All panel ids in a grid, in reading order (root children first, depth-first).
grid_panel_ids <- function(grid) {

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
#' A `dock_grid` is a view's geometry in our compact, dockView-independent
#' form -- nested splits and tab groups with sizes normalised to 0-1 ratios and
#' no volatile ids, so two casts of the same layout compare `identical()`. It is
#' authored with [dock_grid()][layout] and produced by `as_dock_grid()`, which
#' casts another `dock_grid` (identity) or a [dock_layout][dock-layout]
#' (dockView's client echo) into it and is idempotent. `is_dock_grid()` is the
#' class check; `validate_dock_grid()` returns its input and errors on a
#' malformed or non-canonical grid.
#'
#' @param x Object to cast (a `dock_grid` or a [dock_layout][dock-layout]),
#'   validate, or test.
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
    names(x), c("orientation", "children", "sizes", "focus")
  )

  if (length(unexpected)) {
    blockr_abort(
      "Unexpected `dock_grid` component{?s} {unexpected}.",
      class = "dock_grid_structure_invalid"
    )
  }

  if (!grid_sizes_canonical(x)) {
    blockr_abort(
      "A `dock_grid` must be canonical: each branch's sizes sum to 1.",
      class = "dock_grid_not_canonical"
    )
  }

  invisible(x)
}

#' @export
as_dock_grid.dock_grid <- function(x, ...) x

#' @export
as_dock_grid.list <- function(x, ...) {

  if ("grid" %in% names(x)) {
    return(as_dock_grid(as_dock_layout(x)))
  }

  new_dock_grid(
    children = coal(x[["children"]], list(), fail_all = FALSE),
    sizes = x[["sizes"]],
    orientation = coal(x[["orientation"]], "horizontal", fail_all = FALSE),
    focus = x[["focus"]]
  )
}

#' @export
as.list.dock_grid <- function(x, ...) {
  unclass(x)
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

# The sash-position noise floor, an absolute tolerance on the 0-1 size ratios: a
# window resize re-derives ratios from integer pixels and jitters them well
# under a per-cent, so half a per-cent absorbs that noise without eating a
# deliberate drag. The default is a guess, so it is a `blockr_option()` -- tune
# it with `options(blockr.dock_grid_size_tol = ...)`, no code change needed.
grid_size_tol <- function() {
  blockr_option("dock_grid_size_tol", 0.005)
}

# Approximate grid equality: structure exact, relative sizes within the supplied
# `tolerance`. Deferring to `all.equal` keeps the stored sizes faithful (only
# the comparison is fuzzy); comparing the unclassed list walks each pane's size
# individually, and `scale = 1` makes the tolerance absolute on the ratio scale.
# The R-default tolerance stays near-exact, so `all.equal()` / `expect_equal()`
# on a grid is unaffected unless a caller passes a tolerance (the mirror does).
#' @export
all.equal.dock_grid <- function(target, current, ..., scale = 1) {
  all.equal(unclass(target), unclass(current), ..., scale = scale)
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

# Restrict a grid to `members`, dropping ghosts and unknowns and pruning any
# leaf or branch left empty, as a canonical `dock_grid`. The construction-time
# cleaner and the drop half of `place_members()`: it removes, never adds, so a
# member the grid omits stays absent here (defaulted only in `place_members()`).
restrict_grid <- function(grid, members) {

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

  root <- prune(grid[["children"]], grid[["sizes"]])

  new_dock_grid(
    children = root[["children"]],
    sizes = root[["sizes"]],
    orientation = grid[["orientation"]],
    focus = grid[["focus"]]
  )
}

# The member-driven placement of a view: membership decides *which* panels
# appear, the grid only *how*. A ghost (grid panel no longer a member) is
# dropped by `restrict_grid()`; a member the grid omits is appended a default
# single-panel spot. Membership is authoritative -- the grid never withholds.
place_members <- function(grid, members) {

  base <- restrict_grid(grid, members)

  missing <- setdiff(members, grid_panel_ids(base))

  append_default_leaves(base, missing)
}

default_leaf <- function(pid) {
  list(panels = pid, active = pid)
}

# Append members the grid never placed as fresh single-panel leaves at the
# root, each sized to the mean of the existing panes so a newcomer blends in
# rather than dominating; the constructor renormalises. The "default a member
# the grid omits" half of `place_members()`.
append_default_leaves <- function(grid, pids) {

  if (!length(pids)) {
    return(grid)
  }

  size <- if (length(grid[["sizes"]])) mean(grid[["sizes"]]) else 1

  new_dock_grid(
    children = c(grid[["children"]], lapply(pids, default_leaf)),
    sizes = c(grid[["sizes"]], rep(size, length(pids))),
    orientation = grid[["orientation"]],
    focus = grid[["focus"]]
  )
}

new_dock_grids <- function(x = list()) {
  structure(x, class = "dock_grids")
}

#' @rdname view
#' @export
is_dock_grids <- function(x) {
  inherits(x, "dock_grids")
}

#' @param views A `dock_views` collection, used to check that grids key
#'   known views.
#' @rdname view
#' @export
validate_dock_grids <- function(x, views = NULL) {

  if (is.null(x)) {
    return(x)
  }

  if (!is_dock_grids(x) || !is.list(x)) {
    blockr_abort(
      "Expecting a `dock_grids` object or `NULL`.",
      class = "dock_grids_structure_invalid"
    )
  }

  ids <- names(x)

  if (length(x) && (is.null(ids) || any(ids == ""))) {
    blockr_abort(
      "All grids must be keyed by view id.",
      class = "dock_grids_ids_missing"
    )
  }

  if (not_null(views)) {

    unknown <- setdiff(ids, names(views))

    if (length(unknown)) {
      blockr_abort(
        "Grid{?s} {unknown} reference no known view.",
        class = "dock_grids_unknown_view"
      )
    }
  }

  for (id in ids) {

    grid <- x[[id]]

    if (is.null(grid)) {
      next
    }

    validate_dock_grid(grid)
  }

  x
}

#' @rdname view
#' @export
board_grids <- function(x) {
  stopifnot(is_dock_board(x))
  x[["grids"]]
}

#' @rdname view
#' @export
`board_grids<-` <- function(x, value) {
  stopifnot(is_dock_board(x))
  x[["grids"]] <- validate_dock_grids(value, board_views(x))
  invisible(x)
}

# A view's placement geometry, member-driven: membership is authoritative for
# which panels appear, the grid only for their arrangement. No grid (NULL, or
# a member-less view) falls back to a default over the members; otherwise the
# grid's arrangement is kept for the members it places, a member it omits is
# given a default spot, and a ghost (grid panel no longer a member) is dropped.
# This is where placement is resolved, on read.
view_grid <- function(view, grid) {

  members <- view_members(view)

  if (is.null(grid) || !length(members)) {
    default_grid(members)
  } else {
    place_members(grid, members)
  }
}

# The active view's placement grid: which view is active is `active_view()`,
# its geometry an index into `board_grids()` (NULL where unexpressed, so
# `view_grid()` falls back to a default over the members).
active_view_grid <- function(board) {

  id <- active_view(board)

  view_grid(board_views(board)[[id]], board_grids(board)[[id]])
}

default_grid <- function(members) {
  new_dock_grid(children = lapply(members, default_leaf))
}


coerce_dock_grids <- function(grids, id_map) {

  if (is.null(grids)) {
    return(new_dock_grids())
  }

  if (is_dock_grids(grids)) {
    return(grids)
  }

  new_dock_grids(lapply(grids, resolve_grid, id_map = id_map))
}

# Restrict each grid to the (already cleaned) membership of the view it keys,
# dropping ghosts and unknown panels so stored geometry never outlives the
# board. Grids keyed by an unknown view are left for validation to reject.
restrict_grids_to_views <- function(grids, views) {

  for (id in names(grids)) {

    grid <- grids[[id]]

    if (not_null(grid) && id %in% names(views)) {
      grids[[id]] <- restrict_grid(grid, view_members(views[[id]]))
    }
  }

  grids
}

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
#' extensions.
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

  new_dock_grid(
    children = lapply(children, build_grid_node),
    sizes = sizes,
    orientation = orientation
  )
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

  rewrite_grid_leaves(grid, id_map)
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

  rename1 <- function(id) coal(id_map[[id]], id, fail_all = FALSE)

  rewrite_node <- function(node) {

    if (is_grid_leaf(node)) {
      return(
        list(panels = chr_ply(node[["panels"]], rename1),
             active = rename1(node[["active"]]))
      )
    }

    list(children = lapply(node[["children"]], rewrite_node),
         sizes = node[["sizes"]])
  }

  new_dock_grid(
    children = lapply(grid[["children"]], rewrite_node),
    sizes = grid[["sizes"]],
    orientation = grid[["orientation"]],
    focus = if (not_null(grid[["focus"]])) rename1(grid[["focus"]]) else NULL
  )
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

#' Panel IDs of a grid or layout
#'
#' `layout_panel_ids()` returns the canonical panel IDs (`block_panel-...` /
#' `ext_panel-...`) a [dock_grid][dock-grid] or [dock_layout][dock-layout]
#' references; `panel_obj_ids()` strips those prefixes back to the bare block /
#' extension IDs.
#'
#' @param layout A [dock_grid][dock-grid] or [dock_layout][dock-layout].
#' @param ids Character vector of panel IDs.
#' @return Character vectors of IDs.
#' @name panel-ids
#' @export
layout_panel_ids <- function(layout) {
  grid_panel_ids(as_dock_grid(layout))
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
