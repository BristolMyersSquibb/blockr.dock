# A view's grid says which panels the view shows, and where
# (https://github.com/BristolMyersSquibb/blockr/wiki/Rewrite-1.2.1#grids).
# As an app script writes it with `dock_grid()`, it has the shape of the grid
# that the dock's state holds, with a panel reference in place of each panel.
# Its top split holds an `orientation`, its `children` and their `sizes`, and
# it holds its rails by edge in `rails`, where it has any.
# A child is a split, with its `children` and their `sizes`, or a tab group,
# with its `panels` and the `active` one, which it shows.
# A rail holds its `panels`, its `active` one, whether it is `collapsed`, and
# its `size` in pixels.
#
# The functions that write a grid hold a panel reference for each panel, and
# take a bare id for the reference to a block's panel.
# R turns each reference into a panel as it writes the dock's state, with
# `grid_state()`.
# A grid is in its canonical form once written: the shares of each split sum
# to 1, and it holds no tab group, split or rail without a panel.

#' Writing a grid
#'
#' A view's grid says which panels the view shows, and where.
#' Its top split divides the view into tab groups and splits, which divide the
#' other way, each with its share.
#' A rail on the left or right edge of the view holds panels outside the
#' splits.
#' Each tab group and each rail holds panels, the cards of blocks and the UIs
#' of plugins, with a tab for each panel.
#' The functions here write a grid, which [dock_view()] takes:
#'
#' * The function `dock_grid(...)` writes the grid.
#'   Its `...` are what its top split holds, in order, and its rails.
#'   A panel reference or a bare id is a tab group of that panel alone, and a
#'   character vector a tab group with a tab for each of its ids.
#'   A list is a split, which gives each of its children an equal share.
#' * The function `panels(..., active = NULL)` writes a tab group of the
#'   panels given.
#'   It shows `active`, or its first panel where `active` is `NULL`.
#' * The function `group(..., sizes = NULL)` writes a split, which divides
#'   the other way from the split that holds it, with the share of each child
#'   in `sizes`.
#' * The function `rail(..., position = "left")` writes a rail, which holds
#'   the panels given on the edge of the view that `position` names.
#'   The grid takes it among the children of `dock_grid()`, but it is none of
#'   them, so it takes no share in `sizes`.
#'
#' A grid names each panel by a panel reference, as [blk()] and [plg()] write
#' them, or by a bare id, which names a block.
#' It keeps each reference as written, and the dock turns the name of a plugin
#' into the id of the plugin's class as [blockr.core::new_board()] builds the
#' board (see [board_views()]).
#'
#' The shares in `sizes` are positive numbers, which need not sum to 1, since
#' the grid scales them to ratios that do.
#' A split without `sizes` gives each of its children an equal share.
#' The grid drops a tab group, split or rail that holds no panel, and the
#' share of the one it drops goes to its siblings, in proportion.
#' The functions refuse a share that is not positive, and a rail's width that
#' is below 0.
#'
#' @param ... For `dock_grid()` and `group()`, what the split holds: panel
#'   references, bare ids, character vectors, lists, `panels()` and `group()`,
#'   and for `dock_grid()` alone, `rail()`.
#'   For `panels()` and `rail()`, the panels, as panel references or bare ids.
#' @param orientation How the top split divides the view: `"horizontal"`, the
#'   default, puts its children side by side, and `"vertical"` one above the
#'   other.
#' @param active For `panels()` and `rail()`, the panel that it shows, which
#'   is its first where `active` is `NULL`.
#' @param position For `rail()`, the edge of the view that it sits on,
#'   `"left"`, the default, or `"right"`.
#' @param collapsed For `rail()`, whether it shows its tabs alone.
#' @param size For `rail()`, its width in pixels while it is open, 500 unless
#'   given, which fits a row of core's Edit board plugin on one line.
#' @param sizes The share of each child of the split, positive numbers, one
#'   for each child.
#'
#' @examples
#' # A tab group of a and b, which shows b, beside c
#' dock_grid(panels("a", "b", active = "b"), "c", sizes = c(0.3, 0.7))
#'
#' # The block a above a split of b and c
#' dock_grid("a", group("b", "c"), orientation = "vertical")
#'
#' # The block a, with core's Edit board plugin on a rail on the left edge
#' dock_grid(blk("a"), rail(plg("blockr.core-edit_board_plugin")))
#'
#' @return The function `dock_grid()` returns a `dock_grid`.
#'   The function `panels()` returns a tab group, `group()` a split and
#'   `rail()` a `dock_rail`, which `dock_grid()` takes.
#'
#' @rdname layout
#' @export
dock_grid <- function(..., orientation = c("horizontal", "vertical"),
                      sizes = NULL) {

  orientation <- match.arg(orientation)
  nodes <- list(...)

  railed <- lgl_ply(nodes, is_dock_rail)
  children <- nodes[!railed]

  check_sizes(sizes, length(children))

  grid <- c(
    list(orientation = orientation),
    grid_split(lapply(children, grid_node), sizes)
  )

  rails <- grid_rails(nodes[railed])

  if (length(rails)) {
    grid[["rails"]] <- rails
  }

  grid <- structure(grid, class = "dock_grid")
  validate_dock_grid(grid)
  grid
}

#' @rdname layout
#' @export
panels <- function(..., active = NULL) {

  refs <- as_panel_refs(list(...))

  structure(
    list(panels = refs, active = active_panel_ref(active, refs)),
    class = c("dock_panels", "dock_node")
  )
}

#' @rdname layout
#' @export
group <- function(..., sizes = NULL) {

  children <- list(...)
  check_sizes(sizes, length(children))

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

# The shares that a script gives the children of a split, `n` of them: one
# positive number for each, as the schema of the dock's state has them.
check_sizes <- function(sizes, n) {

  if (is.null(sizes)) {
    return(invisible())
  }

  if (!is.numeric(sizes) || is.object(sizes)) {
    blockr_abort(
      "The argument `sizes` gives the shares of a split's children, as ",
      "numbers.",
      class = "dock_layout_sizes_invalid"
    )
  }

  if (length(sizes) != n) {
    blockr_abort(
      "The argument `sizes` gives a share for each of the split's {n} ",
      "child{?ren}, while it gives {length(sizes)}.",
      class = "dock_layout_sizes_invalid"
    )
  }

  bad <- as.character(sizes[is.na(sizes) | !is.finite(sizes) | sizes <= 0])

  if (length(bad)) {
    blockr_abort(
      "A share in `sizes` is a positive number, which the share{?s} {bad} ",
      "{?is/are} not.",
      class = "dock_layout_sizes_invalid"
    )
  }

  invisible()
}

# A child of a split, from what a script gives: a panel reference or bare ids
# for a tab group, `panels()` for a tab group, and `group()` or a list for a
# split. It is `NULL` where it holds no panel.
grid_node <- function(x) {

  if (is_dock_rail(x)) {
    blockr_abort(
      "A rail sits on an edge of the view, so `dock_grid()` takes it, and ",
      "neither `group()` nor a list does.",
      class = "dock_layout_node_invalid"
    )
  }

  if (is_dock_panels(x)) {
    return(tab_group(x[["panels"]], x[["active"]]))
  }

  if (is_dock_group(x)) {
    return(nested_split(lapply(x[["children"]], grid_node), x[["sizes"]]))
  }

  if (is_panel_ref(x) || (is.character(x) && !is.object(x))) {
    return(tab_group(as_panel_refs(x)))
  }

  if (is.list(x) && !is.object(x)) {
    return(nested_split(lapply(x, grid_node)))
  }

  blockr_abort(
    "A grid holds panel references, bare ids, `panels()`, `group()` and ",
    "lists, and not an object of class {class(x)[1L]}.",
    class = "dock_layout_node_invalid"
  )
}

# A tab group of `panels`, which shows `active`, or its first panel where
# `active` is `NULL`. It is `NULL` where it holds no panel.
tab_group <- function(panels, active = NULL) {

  if (!length(panels)) {
    return(NULL)
  }

  list(panels = panels, active = coal(active, panels[[1L]], fail_all = FALSE))
}

# A split inside another, which is `NULL` where it holds nothing.
nested_split <- function(nodes, sizes = NULL) {

  split <- grid_split(nodes, sizes)

  if (!length(split[["children"]])) {
    return(NULL)
  }

  split
}

# A split of `nodes`, with the share of each in `sizes`, or an equal share for
# each where `sizes` is `NULL`.
# A node that holds no panel, which `grid_node()` gives as `NULL`, leaves the
# split with its share, and the shares that stay are scaled to sum to 1.
grid_split <- function(nodes, sizes = NULL) {

  sizes <- as.double(coal(sizes, rep(1, length(nodes)), fail_all = FALSE))
  keep <- !lgl_ply(nodes, is.null)

  list(children = nodes[keep], sizes = sizes[keep] / sum(sizes[keep]))
}

#' Grids
#'
#' A `dock_grid` is a view's grid, as [dock_grid()][layout] writes it.
#' The function `is_dock_grid()` tests whether an object is one.
#' The function `validate_dock_grid()` returns a grid, and refuses one that is
#' not in its canonical form, as `dock_grid()` writes it.
#' In that form, the shares of each split are positive and sum to 1.
#' No tab group, split or rail is empty, and each tab group and rail shows one
#' of its own panels.
#' A view has at most one rail on each of its two edges, left and right.
#'
#' @param x An object.
#'
#' @return The function `is_dock_grid()` returns a boolean, and
#'   `validate_dock_grid()` its input, invisibly.
#'
#' @examples
#' grid <- dock_grid("a", "b", sizes = c(1, 3))
#' is_dock_grid(grid)
#' grid$sizes
#'
#' @name dock-grid
#' @export
is_dock_grid <- function(x) {
  inherits(x, "dock_grid")
}

#' @rdname dock-grid
#' @export
validate_dock_grid <- function(x) {

  if (!is_dock_grid(x) || !is.list(x)) {
    blockr_abort(
      "Expecting a `dock_grid` object.",
      class = "dock_grid_structure_invalid"
    )
  }

  fields <- c("orientation", "children", "sizes", "rails")

  if (!all(fields[1:3] %in% names(x)) || !all(names(x) %in% fields)) {
    blockr_abort(
      "A `dock_grid` holds its `orientation`, `children` and `sizes`, and ",
      "its `rails` where it has any.",
      class = "dock_grid_structure_invalid"
    )
  }

  if (!is_string(x[["orientation"]]) ||
        !x[["orientation"]] %in% c("horizontal", "vertical")) {
    blockr_abort(
      "The orientation of a `dock_grid` is \"horizontal\" or \"vertical\".",
      class = "dock_grid_structure_invalid"
    )
  }

  check_split <- function(node, top = FALSE) {

    children <- node[["children"]]
    sizes <- node[["sizes"]]

    if (!is.list(children) || (!top && !length(children)) ||
          !is.numeric(sizes) || length(sizes) != length(children) ||
          any(sizes <= 0) ||
          (length(sizes) && !isTRUE(all.equal(sum(sizes), 1)))) {
      blockr_abort(
        "A `dock_grid` must be canonical: each split holds a child, and its ",
        "shares are positive and sum to 1.",
        class = "dock_grid_not_canonical"
      )
    }

    for (child in children) {
      if (not_null(child[["panels"]])) {
        check_tab_group(child, "tab group")
      } else {
        check_split(child)
      }
    }
  }

  check_split(x, top = TRUE)

  validate_grid_rails(x[["rails"]])

  invisible(x)
}

# A tab group, or a rail, `what`, holds panel references and shows one of
# them.
check_tab_group <- function(x, what) {

  refs <- x[["panels"]]

  if (!is.list(refs) || !length(refs) || !all(lgl_ply(refs, is_panel_ref)) ||
        !any(lgl_ply(refs, identical, x[["active"]]))) {
    blockr_abort(
      "A {what} of a `dock_grid` holds panel references and shows one of ",
      "them.",
      class = "dock_grid_not_canonical"
    )
  }

  invisible(x)
}

# The grid with `f()` applied to each of its panels, in its tab groups and on
# its rails, which keeps its shape. Since a rail holds `panels`, the walk
# takes it as it takes a tab group.
map_grid_panels <- function(grid, f) {

  node <- function(x) {

    if (not_null(x[["panels"]])) {
      x[["panels"]] <- lapply(x[["panels"]], f)
      x[["active"]] <- f(x[["active"]])
    } else {
      x[["children"]] <- lapply(x[["children"]], node)
    }

    x
  }

  grid[["children"]] <- lapply(grid[["children"]], node)

  if (length(grid[["rails"]])) {
    grid[["rails"]] <- lapply(grid[["rails"]], node)
  }

  grid
}

# The grid as the dock's state holds it, with the panel that `resolve()`
# gives for each reference, as `list(block = "a")`.
grid_state <- function(grid, resolve) {
  map_grid_panels(unclass(grid), resolve)
}

# A grid of the dock's state as `dock_grid()` writes it, with a reference to
# each panel, as `blk()` and `plg()` write them.
grid_from_state <- function(grid) {
  structure(map_grid_panels(grid, panel_ref_of), class = "dock_grid")
}

# The panels that a grid places, in reading order: those of its tab groups,
# depth first, then those of its rails, left before right.
grid_panels <- function(grid) {

  flat <- function(x) coal(unlst(x), list(), fail_all = FALSE)

  walk <- function(x) {

    if (not_null(x[["panels"]])) {
      return(x[["panels"]])
    }

    flat(lapply(x[["children"]], walk))
  }

  c(
    flat(lapply(grid[["children"]], walk)),
    flat(lst_xtr(grid[["rails"]], "panels"))
  )
}

# The default grid over a view's panels, which R writes for a view that the
# script gives panels in place of a grid: the plugins' panels in one tab
# group, beside the blocks' panels in another, as today's `default_grid()`
# does.
default_grid <- function(panels) {

  plugin <- lgl_ply(panels, function(panel) not_null(panel[["plugin"]]))
  groups <- Filter(length, list(panels[plugin], panels[!plugin]))

  c(list(orientation = "horizontal"), grid_split(lapply(groups, tab_group)))
}

# The panels that a grid of the dock's state places more than once, each
# named as a sentence names it.
panels_placed_twice <- function(grid) {
  labels <- chr_ply(grid_panels(grid), panel_label)
  unique(labels[duplicated(labels)])
}

#' @export
str_value.dock_grid <- function(x, ...) {

  ids <- chr_xtr(grid_panels(x), "id")

  if (!length(ids)) {
    return("<dock_grid> (empty)")
  }

  paste0("<dock_grid> ", paste0(ids, collapse = ", "))
}

#' @importFrom utils str
#' @export
str.dock_grid <- function(object, ...) {
  cat(" ", str_value(object), "\n", sep = "")
  invisible(object)
}

# The print leaves out what says nothing of the layout: the orientation of a
# split that holds fewer than two children, and the share of an only child.
#' @export
format.dock_grid <- function(x, ...) {

  if (!length(grid_panels(x))) {
    return("<dock_grid> (empty)")
  }

  division <- split_division(x[["children"]], x[["orientation"]])

  c(
    paste0("<dock_grid>", if (not_null(division)) paste0(" ", division)),
    format_grid_body(x, list())
  )
}

#' @export
print.dock_grid <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

# How a split divides, where it holds several children, since the orientation
# of a split with one child says nothing of the layout.
split_division <- function(children, orientation) {
  if (length(children) > 1L) orientation
}

# The lines of a grid below its first line: its splits as a tree, then each of
# its rails on a line.
# The panel of a block whose card hides a section shows the sections that the
# card shows, which `visible` gives by the block's id.
format_grid_body <- function(grid, visible) {

  rails <- chr_mply(
    function(rail, edge) {
      paste0(
        edge,
        " rail",
        grid_attrs(if (rail[["collapsed"]]) "collapsed"),
        ": ",
        format_tabs(rail, visible)
      )
    },
    grid[["rails"]],
    names(grid[["rails"]])
  )

  c(
    format_nodes(
      grid[["children"]],
      grid[["sizes"]],
      grid[["orientation"]],
      visible
    ),
    rails
  )
}

# The lines of a split's children, as a tree, each after its share.
# An only child has no share in the tree.
format_nodes <- function(children, sizes, orientation, visible) {

  shared <- length(children) > 1L

  format_tree(
    map(
      function(node, size) {

        lines <- format_node(node, orientation, visible)

        if (shared) {
          lines[1L] <- paste0(round(100 * size), "%: ", lines[1L])
        }

        lines
      },
      children,
      sizes
    )
  )
}

# The lines of a child of a split that divides as `orientation` says.
# A tab group lists its panels on one line, and a split, which divides the
# other way, holds the lines of its children below its own.
format_node <- function(node, orientation, visible) {

  if (is.null(node[["children"]])) {
    return(format_tabs(node, visible))
  }

  inner <- setdiff(c("horizontal", "vertical"), orientation)

  c(
    paste0("group", grid_attrs(split_division(node[["children"]], inner))),
    format_nodes(node[["children"]], node[["sizes"]], inner, visible)
  )
}

# The panels of a tab group or a rail, on one line, each with its marks: that
# it is a plugin's panel, that it is the tab that its group shows, where the
# group holds several, and after a semicolon, the sections that the card of a
# block's panel shows, where the card hides one.
format_tabs <- function(node, visible) {

  refs <- node[["panels"]]
  several <- length(refs) > 1L

  tabs <- chr_ply(
    refs,
    function(ref) {

      kind <- ref_kind(ref)

      paste0(
        ref[["id"]],
        grid_attrs(
          kind,
          if (several && identical(ref, node[["active"]])) "active",
          sections = if (is.null(kind)) section_mark(ref[["id"]], visible)
        )
      )
    }
  )

  paste(tabs, collapse = ", ")
}

# The sections that the card of the block `id` shows, where `visible` gives
# them, as one letter: "i" for its inputs, "o" for its outputs and "n" for
# neither.
# A card that `visible` gives no entry shows both, and has no mark.
section_mark <- function(id, visible) {

  if (!id %in% names(visible)) {
    return(NULL)
  }

  shown <- visible[[id]]

  if (length(shown)) substr(shown, 1L, 1L) else "n"
}

# The lines of `items`, each a character vector of lines, as the branches of a
# tree.
# Each item's first line follows its branch, and the item's other lines go
# below it, indented.
format_tree <- function(items) {

  n <- length(items)

  unlst(
    map(
      function(lines, last) {
        c(
          paste0(if (last) "\u2514\u2500 " else "\u251c\u2500 ", lines[1L]),
          paste0(if (last) "   " else "\u2502  ", lines[-1L], recycle0 = TRUE)
        )
      },
      items,
      seq_len(n) == n
    )
  )
}

# How the tree marks the panel of a plugin, which it names by its id alone.
ref_kind <- function(ref) {
  if (identical(ref[["type"]], "plugin")) "plugin"
}

# The marks of a line of the print, in parentheses after a space, with the
# sections that a card shows after a semicolon, or nothing where there is no
# mark.
grid_attrs <- function(..., sections = NULL) {

  marks <- c(...)

  tokens <- c(if (length(marks)) paste(marks, collapse = ", "), sections)

  if (!length(tokens)) {
    return("")
  }

  paste0(" (", paste(tokens, collapse = "; "), ")")
}
