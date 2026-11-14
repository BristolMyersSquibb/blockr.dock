# A rail holds panels on the left or right edge of a view, outside its splits,
# as dockview's edge group does.
# The function `rail()` writes one for `dock_grid()`, which keeps it in its
# `rails`, by the edge that its `position` names, without that `position`.
# A rail's widths are pixels rather than shares, since the dock sizes a rail
# against its own width, and not as a share of a split.
#
# A grid holds a rail only while the rail holds panels, since a rail's
# `active` names one of them, so `dock_grid()` drops a rail that holds none,
# whatever its widths.
# Rails sit on the left and right edges alone: the top edge is where every
# tab group's tabs sit, and the bottom edge is left out with it.

rail_positions <- c("left", "right")

#' @rdname layout
#' @export
rail <- function(..., position = c("left", "right"), active = NULL,
                 collapsed = FALSE, size = 260, collapsed_size = 35) {

  position <- match.arg(position)
  refs <- as_panel_refs(list(...))

  if (!is_bool(collapsed)) {
    blockr_abort(
      "Whether a rail is collapsed is `TRUE` or `FALSE`.",
      class = "dock_rail_collapsed_invalid"
    )
  }

  check_rail_size(size, "size")
  check_rail_size(collapsed_size, "collapsed_size")

  structure(
    list(
      position = position,
      panels = refs,
      active = active_panel_ref(active, refs),
      collapsed = collapsed,
      size = as.double(size),
      collapsed_size = as.double(collapsed_size)
    ),
    class = "dock_rail"
  )
}

is_dock_rail <- function(x) {
  inherits(x, "dock_rail")
}

# A rail's width, `arg`, is a number of pixels of 0 or more, as the schema of
# the dock's state has it.
check_rail_size <- function(x, arg) {

  if (!is_number(x) || is.na(x) || !is.finite(x) || x < 0) {
    blockr_abort(
      "The width of a rail in `{arg}` is a number of pixels of 0 or more.",
      class = "dock_rail_size_invalid"
    )
  }

  invisible(x)
}

# The rails that a script gives `dock_grid()`, by edge, left before right,
# without the rails that hold no panel.
# A view has one rail at most on each edge.
grid_rails <- function(rails) {

  edges <- chr_xtr(rails, "position")
  twice <- unique(edges[duplicated(edges)])

  if (length(twice)) {
    blockr_abort(
      "A view has one rail at most on each edge, while the grid gives two on ",
      "the {twice} edge{?s}.",
      class = "dock_rails_position_clash"
    )
  }

  rails <- set_names(rails, edges)[intersect(rail_positions, edges)]
  rails <- Filter(function(rail) length(rail[["panels"]]) > 0L, rails)

  lapply(rails, function(rail) unclass(rail)[names(rail) != "position"])
}

# The rails of a grid, by edge, as `dock_grid()` keeps them.
validate_grid_rails <- function(rails) {

  if (is.null(rails)) {
    return(invisible(rails))
  }

  edges <- names(rails)

  if (!is.list(rails) || is.object(rails) || !length(rails) ||
        is.null(edges) || !all(edges %in% rail_positions) ||
        anyDuplicated(edges)) {
    blockr_abort(
      "A `dock_grid` holds its rails by the edge that each sits on, one at ",
      "most on each of the edges {rail_positions}.",
      class = "dock_rails_structure_invalid"
    )
  }

  for (rail in rails) {

    check_tab_group(rail, "rail")

    if (!is_bool(rail[["collapsed"]])) {
      blockr_abort(
        "Whether a rail is collapsed is `TRUE` or `FALSE`.",
        class = "dock_rail_collapsed_invalid"
      )
    }

    check_rail_size(rail[["size"]], "size")
    check_rail_size(rail[["collapsed_size"]], "collapsed_size")
  }

  invisible(rails)
}

#' @export
str_value.dock_rail <- function(x, ...) {

  ids <- chr_xtr(x[["panels"]], "id")
  held <- if (length(ids)) paste0(ids, collapse = ", ") else "(empty)"

  paste0("<dock_rail> ", x[["position"]], ": ", held)
}

#' @export
format.dock_rail <- function(x, ...) {
  str_value(x)
}

#' @export
print.dock_rail <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}
