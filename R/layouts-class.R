#' Dock views (layouts)
#'
#' A `dock_board` can contain multiple views (global tabs), each with its
#' own DockView layout. Blocks and extensions are shared across views
#' via the board's DAG; view membership is a layout concern only.
#'
#' Multiple views are defined via `dock_layouts()`, which accepts named
#' list elements — each a (possibly nested) list of block and extension
#' IDs (the same format accepted by `create_dock_layout(grid = ...)`).
#' A plain named `list()` is also accepted and auto-detected by
#' [new_dock_board()]. View CRUD is enabled unless the dock is locked
#' (see `is_dock_locked()`).
#'
#' @param ... Named list elements, each a layout specification (possibly
#'   nested list of block/extension IDs) or a `dock_layout` object.
#' @param active Optional name of the initially active view. Defaults to
#'   the first element.
#'
#' @return `dock_layouts()` returns a `dock_layouts` object.
#'   `is_dock_layouts()` returns a boolean.
#'   `active_view()` returns a string and `active_view<-()` returns
#'   the modified `dock_layouts` object invisibly.
#'   `view_ids()` returns all IDs (block + extension) found in a layout
#'   specification. The `view_can_crud()` helper returns `FALSE` when the
#'   dock is locked.
#'
#' @examples
#' # Explicit constructor
#' ly <- dock_layouts(
#'   Analysis = list("dataset_1", "head_1"),
#'   Overview = list("dag_extension")
#' )
#' is_dock_layouts(ly)
#' active_view(ly)
#'
#' # Set initial active view
#' ly2 <- dock_layouts(
#'   Analysis = list("dataset_1"),
#'   Overview = list("dag_extension"),
#'   active = "Overview"
#' )
#' active_view(ly2)
#'
#' @rdname view
#' @export
new_dock_layouts <- function(...) {
  vws <- list(...)

  # Unwrap a single list argument only when it looks like a wrapped collection

  # of named views — e.g. dock_layouts(list(A = ..., B = ...)).
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

  structure(
    vws,
    active = names(vws)[1L],
    class = "dock_layouts"
  )
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

  x
}

#' @rdname view
#' @export
dock_layouts <- function(..., active = NULL) {
  res <- validate_dock_layouts(new_dock_layouts(...))

  if (!is.null(active)) {
    active_view(res) <- active
  }

  res
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
  stopifnot(is_dock_layouts(x))
  attr(x, "active")
}

#' @param value Replacement value
#' @rdname view
#' @export
`active_view<-` <- function(x, value) {
  stopifnot(is_dock_layouts(x), is_string(value))

  if (!value %in% names(x)) {
    blockr_abort(
      "View {value} does not exist.",
      class = "dock_view_not_found"
    )
  }

  attr(x, "active") <- value
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
`dock_layouts<-` <- function(x, value) {
  stopifnot(is_dock_board(x))
  x[["layout"]] <- validate_dock_layouts(value)
  invisible(x)
}

#' @rdname view
#' @export
board_views <- function(x) {
  stopifnot(is_dock_board(x))

  ly <- x[["layout"]]

  if (is_dock_layouts(ly)) {
    ly
  } else {
    NULL
  }
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
as_dock_layouts.list <- function(x, ...) {
  dock_layouts(x)
}
