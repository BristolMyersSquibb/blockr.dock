#' @param members Ordered character vector of panel IDs.
#' @param name Optional display name for the view.
#' @param views A `dock_views` collection, used to check membership when
#'   validating grids.
#' @rdname view
#' @keywords internal
new_dock_view <- function(members = character(), name = NULL) {

  res <- structure(
    list(members = as.character(members)),
    class = "dock_view"
  )

  if (!is.null(name)) {
    view_name(res) <- name
  }

  res
}

#' @rdname view
#' @export
is_dock_view <- function(x) {
  inherits(x, "dock_view")
}

#' @rdname view
#' @export
view_members <- function(x) {
  stopifnot(is_dock_view(x))
  x[["members"]]
}

#' @export
str_value.dock_view <- function(x, ...) {

  ids <- panel_obj_ids(view_members(x))

  if (!length(ids)) {
    return("<dock_view> (empty)")
  }

  paste0("<dock_view> ", paste0(ids, collapse = ", "))
}

#' @importFrom utils str
#' @export
str.dock_view <- function(object, ...) {
  cat(" ", str_value(object), "\n", sep = "")
  invisible(object)
}

new_dock_views <- function(views) {
  finalize_views_active(views)
}

# Wrap an already id-keyed list of `dock_view`s as a `dock_views` without
# minting fresh ids -- the runtime rebuild and deserialization paths, where
# identity must survive recomputes.
reconstruct_dock_views <- function(views) {
  validate_dock_views(finalize_views_active(views))
}

# Assemble views into a `dock_views`, defaulting the active view to the first
# (written through the setter). A view never carries its own active marker --
# the container owns it, exactly as for `dock_layouts`.
finalize_views_active <- function(views) {

  res <- structure(views, class = "dock_views")

  if (length(res)) {
    active_view(res) <- names(res)[1L]
  }

  res
}

#' @rdname view
#' @export
is_dock_views <- function(x) {
  inherits(x, "dock_views")
}

#' @rdname view
#' @export
validate_dock_views <- function(x) {

  if (!is_dock_views(x) || !is.list(x)) {
    blockr_abort(
      "Expecting a `dock_views` object.",
      class = "dock_views_structure_invalid"
    )
  }

  if (length(x) == 0L) {
    blockr_abort(
      "At least one view is required.",
      class = "dock_views_empty"
    )
  }

  validate_view_ids(names(x))

  for (v in x) {

    if (!is_dock_view(v)) {
      blockr_abort(
        "All elements of `dock_views` must be `dock_view` objects.",
        class = "dock_views_element_invalid"
      )
    }
  }

  validate_active_attr(x, names(x))

  x
}

# Ids key the container and become DOM / namespace ids, so they must be safe
# identifiers (no whitespace); free-form display labels live on the view as a
# name. Shared by `validate_dock_views()` and `validate_dock_layouts()`.
validate_view_ids <- function(ids) {

  if (is.null(ids) || any(ids == "")) {
    blockr_abort(
      "All views must carry an id.",
      class = "dock_views_ids_missing"
    )
  }

  if (anyDuplicated(ids) > 0L) {
    blockr_abort(
      "View ids must be unique.",
      class = "dock_views_ids_duplicated"
    )
  }

  bad <- ids[!grepl("^[A-Za-z0-9._-]+$", ids)]

  if (length(bad)) {
    blockr_abort(
      "View id{?s} {bad} must be a safe identifier (letters, digits, . - _).",
      class = "dock_view_id_invalid"
    )
  }

  invisible(ids)
}

# "Exactly one active" is structural: the container holds a single active id,
# so the only guard needed is that it names a view that is present.
validate_active_attr <- function(x, ids) {

  active <- attr(x, "active", exact = TRUE)

  if (!is.null(active) && !active %in% ids) {
    blockr_abort(
      "Active view {active} does not exist.",
      class = "dock_view_not_found"
    )
  }

  invisible(x)
}

#' @export
active_view.dock_views <- function(x) {

  id <- attr(x, "active", exact = TRUE)

  if (is.null(id) || !id %in% names(x)) {
    return(NULL)
  }

  id
}

#' @export
`active_view<-.dock_views` <- function(x, value) {

  stopifnot(is_string(value))

  if (!value %in% names(x)) {
    blockr_abort(
      "View {value} does not exist.",
      class = "dock_view_not_found"
    )
  }

  attr(x, "active") <- value

  invisible(x)
}

#' @export
str_value.dock_views <- function(x, ...) {

  ids <- names(x)

  marks <- rep("", length(x))
  active <- active_view(x)

  if (not_null(active)) {
    marks[ids == active] <- " (active)"
  }

  lines <- paste0("  ", ids, ": ", chr_ply(x, str_value), marks)

  paste(
    c(paste0("<dock_views[", length(x), "]>"), lines),
    collapse = "\n"
  )
}

#' @importFrom utils str
#' @export
str.dock_views <- function(object, ...) {
  cat(str_value(object), "\n", sep = "")
  invisible(object)
}

#' @rdname view
#' @export
board_views <- function(x) {
  stopifnot(is_dock_board(x))
  x[["views"]]
}

#' @rdname view
#' @export
`board_views<-` <- function(x, value) {
  stopifnot(is_dock_board(x))
  x[["views"]] <- validate_dock_views(value)
  invisible(x)
}

new_dock_grids <- function(x = list()) {
  structure(x, class = "dock_grids")
}

#' @rdname view
#' @export
is_dock_grids <- function(x) {
  inherits(x, "dock_grids")
}

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

    validate_dock_layout(grid)

    if (not_null(views)) {

      extra <- setdiff(layout_panel_ids(grid), view_members(views[[id]]))

      if (length(extra)) {
        blockr_abort(
          paste(
            "Grid for view {id} references panel{?s} {extra} outside",
            "its membership."
          ),
          class = "dock_grid_not_subset"
        )
      }
    }
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

# Rebuild the fused `dock_layouts` (grid-bearing, one entry per view) from the
# split structure + grid slots. A view with no stored grid gets a default
# single-panel-per-member grid, so the composed handle is always a valid
# `dock_layouts` -- the bridge the update lifecycle still reads until #294
# rewires it onto the two slots directly.
compose_layouts <- function(views, grids = NULL) {

  ids <- names(views)

  build_one <- function(id) {

    grid <- if (is.null(grids)) NULL else grids[[id]]

    ly <- if (is.null(grid)) {
      default_grid(view_members(views[[id]]))
    } else {
      grid
    }

    nm <- view_name(views[[id]])

    if (!is.null(nm)) {
      view_name(ly) <- nm
    }

    ly
  }

  res <- reconstruct_dock_layouts(set_names(lapply(ids, build_one), ids))

  active <- active_view(views)

  if (!is.null(active)) {
    active_view(res) <- active
  }

  res
}

# Split a fused `dock_layouts` into the structure (`dock_views`) and geometry
# (`dock_grids`) slots: panel set + display name become the view record, the
# grid (sans name) becomes the stored grid. Inverse of compose_layouts() for
# any membership-covering grid.
decompose_layouts <- function(layouts) {

  ids <- names(layouts)

  make_view <- function(id) {
    new_dock_view(layout_panel_ids(layouts[[id]]), view_name(layouts[[id]]))
  }

  views <- reconstruct_dock_views(set_names(lapply(ids, make_view), ids))

  active <- active_view(layouts)

  if (!is.null(active)) {
    active_view(views) <- active
  }

  grids <- new_dock_grids(
    set_names(lapply(unname(layouts), strip_view_name), ids)
  )

  list(views = views, grids = grids)
}

strip_view_name <- function(x) {
  attr(x, "view_name") <- NULL
  x
}

default_grid <- function(members) {

  if (!length(members)) {
    return(new_dock_layout())
  }

  new_dock_layout(grid = build_grid_tree(as.list(members)))
}
