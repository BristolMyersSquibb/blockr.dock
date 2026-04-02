#' Dock workspaces
#'
#' A `dock_board` can contain multiple workspaces (global tabs), each with its
#' own DockView layout. Blocks and extensions are shared across workspaces
#' via the board's DAG; workspace membership is a layout concern only.
#'
#' A single workspace is created with `dock_workspace()`, which accepts a
#' layout specification as a (possibly nested) list of block and extension
#' IDs — the same format accepted by `create_dock_layout(grid = ...)`.
#' Block and extension membership is derived from that layout.
#' Multiple workspaces are combined via `dock_workspaces()`. Workspace CRUD
#' is enabled unless the dock is locked (see `is_dock_locked()`).
#'
#' @param layout Layout specification as a (possibly nested) list of block
#'   and extension IDs, e.g. `list("dataset_1", "head_1")` or
#'   `list("edit_board_extension", list("a", "b"))`.
#'
#' @return `dock_workspace()` returns a `dock_workspace` object.
#'   `dock_workspaces()` returns a `dock_workspaces` object.
#'   `is_dock_workspace()` and `is_dock_workspaces()` return booleans.
#'   `active_workspace()` returns a string and `active_workspace<-()` returns
#'   the modified `dock_workspaces` object invisibly.
#'   `workspace_ids()` returns all IDs (block + extension) found in the
#'   layout. The `ws_can_crud()` helper returns `FALSE` when the dock is
#'   locked.
#'
#' @rdname workspace
#' @export
dock_workspace <- function(layout = list()) {
  structure(
    list(layout = layout),
    class = "dock_workspace"
  )
}

#' @rdname workspace
#' @export
is_dock_workspace <- function(x) {
  inherits(x, "dock_workspace")
}

#' @param x Object
#' @rdname workspace
#' @export
workspace_ids <- function(x) {
  stopifnot(is_dock_workspace(x))
  unique(unlist(x[["layout"]]))
}

#' @rdname workspace
#' @export
workspace_layout <- function(x) {
  stopifnot(is_dock_workspace(x))
  x[["layout"]]
}

#' @param value Replacement value
#' @rdname workspace
#' @export
`workspace_layout<-` <- function(x, value) {
  stopifnot(is_dock_workspace(x))
  x[["layout"]] <- value
  x
}

#' @param ... Named `dock_workspace` objects
#' @rdname workspace
#' @export
dock_workspaces <- function(...) {
  ws <- list(...)

  if (length(ws) == 1L && is.list(ws[[1L]]) && !is_dock_workspace(ws[[1L]])) {
    ws <- ws[[1L]]
  }

  if (!length(ws)) {
    ws <- list(Page = dock_workspace())
  }

  if (is.null(names(ws)) || any(names(ws) == "")) {
    blockr_abort(
      "All workspaces must be named.",
      class = "dock_workspaces_names_missing"
    )
  }

  validate_dock_workspaces(
    structure(
      ws,
      active = names(ws)[1L],
      class = "dock_workspaces"
    )
  )
}

#' @rdname workspace
#' @export
is_dock_workspaces <- function(x) {
  inherits(x, "dock_workspaces")
}

#' @rdname workspace
#' @export
validate_dock_workspaces <- function(x) {
  if (!is_dock_workspaces(x) || !is.list(x)) {
    blockr_abort(
      "Expecting workspaces to inherit from `dock_workspaces` and be a list.",
      class = "dock_workspaces_structure_invalid"
    )
  }

  if (length(x) == 0L) {
    blockr_abort(
      "At least one workspace is required.",
      class = "dock_workspaces_empty"
    )
  }

  nms <- names(x)

  if (is.null(nms) || any(nms == "")) {
    blockr_abort(
      "All workspaces must be named.",
      class = "dock_workspaces_names_missing"
    )
  }

  if (anyDuplicated(nms) > 0L) {
    blockr_abort(
      "Workspace names must be unique.",
      class = "dock_workspaces_names_duplicated"
    )
  }

  for (ws in x) {
    if (!is_dock_workspace(ws)) {
      blockr_abort(
        "All elements of `dock_workspaces` must be `dock_workspace` objects.",
        class = "dock_workspaces_element_invalid"
      )
    }
  }

  x
}

#' @rdname workspace
#' @export
active_workspace <- function(x) {
  stopifnot(is_dock_workspaces(x))
  attr(x, "active")
}

#' @rdname workspace
#' @export
`active_workspace<-` <- function(x, value) {
  stopifnot(is_dock_workspaces(x), is_string(value))

  if (!value %in% names(x)) {
    blockr_abort(
      "Workspace {value} does not exist.",
      class = "dock_workspace_not_found"
    )
  }

  attr(x, "active") <- value
  invisible(x)
}

#' @rdname workspace
#' @export
ws_can_crud <- function(x) {
  stopifnot(is_dock_workspaces(x))
  !is_dock_locked()
}

#' @param workspaces Replacement value
#' @rdname workspace
#' @export
`dock_workspaces<-` <- function(x, value) {
  stopifnot(is_dock_board(x))
  x[["workspaces"]] <- validate_dock_workspaces(value)
  invisible(x)
}

# Build a workspace mapping: which IDs belong to which workspace
ws_map <- function(workspaces) {
  stopifnot(is_dock_workspaces(workspaces))

  ids <- list()

  for (ws_name in names(workspaces)) {
    for (id in workspace_ids(workspaces[[ws_name]])) {
      ids[[id]] <- c(ids[[id]], ws_name)
    }
  }

  ids
}
