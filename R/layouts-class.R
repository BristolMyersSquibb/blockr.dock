#' Dock views (layouts)
#'
#' A `dock_board` always holds a `dock_layouts` object (multi-view tabs).
#' Single-page boards are a degenerate case: one auto-named "Page" view.
#' Blocks and extensions are shared across views via the board's DAG;
#' view membership is a layout concern only.
#'
#' Multiple views are defined via `dock_layouts()`, which accepts named
#' list elements -- each either a raw grid spec (a possibly-nested list
#' of block/extension IDs) or a `dock_grid` object created with
#' [dock_grid()]. A grid can be marked as the initially active one by
#' passing `active = TRUE` to `dock_grid()`; if none is marked, the
#' first one is used. View CRUD is enabled unless the dock is locked
#' (see `is_dock_locked()`).
#'
#' @param ... Named list elements, each a `dock_grid`, a `dock_layout`,
#'   or a raw list of block/extension IDs.
#'
#' @return `dock_layouts()` returns a `dock_layouts` object.
#'   `is_dock_layouts()` returns a boolean.
#'   `active_view()` returns a string and `active_view<-()` returns
#'   the modified `dock_layouts` (or `dock_board`) object invisibly.
#'   `view_ids()` returns all IDs (block + extension) found in a layout
#'   specification. The `view_can_crud()` helper returns `FALSE` when the
#'   dock is locked.
#'
#' @examples
#' # First view is active by default
#' ly <- dock_layouts(
#'   Analysis = list("dataset_1", "head_1"),
#'   Overview = list("dag_extension")
#' )
#' is_dock_layouts(ly)
#' active_view(ly)
#'
#' # Mark a specific view as initially active
#' ly2 <- dock_layouts(
#'   Analysis = list("dataset_1"),
#'   Overview = dock_grid("dag_extension", active = TRUE)
#' )
#' active_view(ly2)
#'
#' @rdname view
#' @export
new_dock_layouts <- function(...) {
  vws <- list(...)

  # Unwrap a single list argument only when it looks like a wrapped collection
  # of named views -- e.g. dock_layouts(list(A = ..., B = ...)).
  # Do NOT unwrap dock_layouts(V1 = list("a")) where vws is already correct.
  if (length(vws) == 1L && is.list(vws[[1L]]) && !is_dock_layout(vws[[1L]])) {
    inner <- vws[[1L]]
    inner_nms <- names(inner)
    if (!is.null(inner_nms) && all(nzchar(inner_nms))) {
      vws <- inner
    }
  }

  if (!length(vws)) {
    vws <- list(Page = list())
  }

  # Auto-default first view to active if none is marked.
  if (!any(vapply(vws, is_active_view, logical(1L)))) {
    vws[[1L]] <- set_active_view(vws[[1L]])
  }

  structure(vws, class = "dock_layouts")
}

#' @rdname view
#' @export
validate_dock_layouts <- function(x) {
  if (!is_dock_layouts(x) || !is.list(x)) {
    blockr_abort(
      "Expecting a `dock_layouts` object.",
      class = "dock_layouts_structure_invalid"
    )
  }

  if (length(x) == 0L) {
    blockr_abort(
      "At least one view is required.",
      class = "dock_layouts_empty"
    )
  }

  nms <- names(x)

  if (is.null(nms) || any(nms == "")) {
    blockr_abort(
      "All views must be named.",
      class = "dock_layouts_names_missing"
    )
  }

  if (anyDuplicated(nms) > 0L) {
    blockr_abort(
      "View names must be unique.",
      class = "dock_layouts_names_duplicated"
    )
  }

  for (v in x) {
    if (!is.list(v)) {
      blockr_abort(
        "All elements of `dock_layouts` must be lists.",
        class = "dock_layouts_element_invalid"
      )
    }
  }

  n_active <- sum(vapply(x, is_active_view, logical(1L)))

  if (n_active > 1L) {
    blockr_abort(
      "Expecting at most one active view, found {n_active}.",
      class = "dock_layouts_multiple_active"
    )
  }

  x
}

#' @rdname view
#' @export
dock_layouts <- function(...) {
  validate_dock_layouts(new_dock_layouts(...))
}

#' @param x Object
#' @rdname view
#' @export
is_dock_layouts <- function(x) {
  inherits(x, "dock_layouts")
}

#' @rdname view
#' @export
view_ids <- function(x) {
  stopifnot(is.list(x))
  unique(unlist(x))
}

#' @rdname view
#' @export
active_view <- function(x) {
  UseMethod("active_view")
}

#' @export
active_view.dock_layouts <- function(x) {
  idx <- which(vapply(x, is_active_view, logical(1L)))[1L]
  names(x)[idx]
}

#' @export
active_view.dock_board <- function(x) {
  active_view(board_layouts(x))
}

#' @param value Replacement value
#' @rdname view
#' @export
`active_view<-` <- function(x, value) {
  UseMethod("active_view<-")
}

#' @export
`active_view<-.dock_layouts` <- function(x, value) {
  stopifnot(is_string(value))

  if (!value %in% names(x)) {
    blockr_abort(
      "View {value} does not exist.",
      class = "dock_view_not_found"
    )
  }

  for (nm in names(x)) {
    x[[nm]] <- set_active_view(x[[nm]], identical(nm, value))
  }

  invisible(x)
}

#' @export
`active_view<-.dock_board` <- function(x, value) {
  ly <- board_layouts(x)
  active_view(ly) <- value
  board_layouts(x) <- ly
  invisible(x)
}

#' @rdname view
#' @export
view_can_crud <- function(x) {
  stopifnot(is_dock_layouts(x))
  !is_dock_locked()
}

#' @rdname view
#' @export
`board_layouts<-` <- function(x, value) {
  stopifnot(is_dock_board(x))
  x[["layouts"]] <- validate_dock_layouts(value)
  invisible(x)
}

#' @rdname view
#' @export
board_layouts <- function(x) {
  stopifnot(is_dock_board(x))
  x[["layouts"]]
}

#' @param ... Generic consistency
#' @rdname view
#' @export
as_dock_layouts <- function(x, ...) {
  UseMethod("as_dock_layouts")
}

#' @export
as_dock_layouts.dock_layouts <- function(x, ...) x

#' @export
as_dock_layouts.dock_layout <- function(x, ...) {
  dock_layouts(Page = set_active_view(x))
}

#' @export
as_dock_layouts.list <- function(x, ...) {
  dock_layouts(x)
}

# Internal helpers ------------------------------------------------------------

is_active_view <- function(x) {
  isTRUE(attr(x, "active"))
}

is_view <- function(x) {
  is_dock_layout(x) || is_dock_grid(x) || (is.list(x) && !is.object(x))
}

set_active_view <- function(x, active = TRUE) {
  stopifnot(is_view(x))
  attr(x, "active") <- if (isTRUE(active)) TRUE else NULL
  x
}
