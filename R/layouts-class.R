#' Dock views (layouts)
#'
#' A `dock_board` always holds a `dock_layouts` object (multi-view tabs).
#' Single-page boards are a degenerate case: one auto-named "Page" view.
#' Blocks and extensions are shared across views via the board's DAG;
#' view membership is a layout concern only.
#'
#' Each view carries a stable, immutable **id** (the key into the
#' `dock_layouts` collection) that is distinct from its editable display
#' **name**. This mirrors the id / name separation used for blocks (an
#' immutable id keys the collection; [blockr.core::block_name()] is an
#' editable label). The id is minted once when the view is created and
#' never changes — rename only rewrites the name attribute, never the
#' key. Use `view_name()` / `view_name<-()` to read and write a view's
#' display name and `view_names()` for all names in a collection.
#'
#' Multi-view boards are defined by passing a named list to
#' `new_dock_board()`'s `layouts` argument: each **name is the view's id**
#' (the container's key, like a block id — minted when absent), each value
#' is the panel arrangement inside that view (a [dock_layout()], or a raw
#' list of block / extension IDs). The display name is set on the layout
#' via `dock_layout(name = )`; with none set, a label is derived from the
#' id. A view can be marked as the initially-active one by passing
#' `active = TRUE` to [dock_layout()]; if none is marked, the first one is
#' used. View CRUD is enabled unless the dock is locked (see
#' `is_dock_locked()`).
#'
#' Users do not normally construct a `dock_layouts` directly; instead
#' they pass a plain named list to `new_dock_board(layouts = ...)`,
#' which resolves, validates and wraps it (minting any absent ids).
#'
#' @return `is_dock_layouts()` returns a boolean.
#'   `validate_dock_layouts()` returns its input and throws on error.
#'   `active_view()` returns the active view's id, or `NULL` when no
#'   view is active, and `active_view<-()` returns the modified
#'   `dock_layouts` (or `dock_board`) object invisibly. `view_name()`
#'   returns a view's explicit display name (or `NULL`), `view_name<-()`
#'   the modified `dock_layout`, and `view_names()` a character vector of
#'   display labels keyed by view id (derived from the id where a view
#'   has no explicit name).
#'
#' @examples
#' brd <- new_dock_board(
#'   blocks = c(
#'     dataset_1 = blockr.core::new_dataset_block(),
#'     head_1 = blockr.core::new_head_block()
#'   ),
#'   layouts = list(
#'     analysis = dock_layout("dataset_1", "head_1", name = "Analysis"),
#'     overview = dock_layout("dataset_1", name = "Overview", active = TRUE)
#'   )
#' )
#' view_names(board_layouts(brd))
#'
#' @rdname view
#' @keywords internal
new_dock_layouts <- function(...) {
  vws <- list(...)

  if (length(vws) == 1L && is.list(vws[[1L]]) && !is_dock_layout(vws[[1L]])) {
    vws <- vws[[1L]]
  }

  if (!length(vws)) {
    # Keyless: id is minted, display name derived from it (no hardcoded id).
    vws <- list(new_dock_layout())
  }

  finalize_layouts_active(mint_view_ids(vws))
}

# Wrap an already id-keyed list of `dock_layout`s (each carrying its
# `view_name` attribute) as a `dock_layouts` without minting fresh ids.
# Used by the runtime rebuild paths and new-format deserialization, where
# identity must be preserved across recomputes.
reconstruct_dock_layouts <- function(views) {
  validate_dock_layouts(finalize_layouts_active(views))
}

# Assemble views into an id-keyed `dock_layouts` list. The incoming list
# name is the view's stable id; a keyless entry (like an unnamed block)
# gets one minted with blockr.core's `rand_names()` — the same generator
# block / stack / link ids use. The display name lives on the object
# (`dock_layout(name = )`) and is left untouched. `reserved` lists ids
# already in use (the board's existing views, for a delta-driven add).
mint_view_ids <- function(views, reserved = character()) {

  ids <- names(views)

  if (is.null(ids)) {
    ids <- rep("", length(views))
  }

  given <- nzchar(ids)

  if (any(!given)) {
    ids[!given] <- rand_names(c(reserved, ids[given]), n = sum(!given))
  }

  set_names(views, ids)
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

  ids <- names(x)

  if (is.null(ids) || any(ids == "")) {
    blockr_abort(
      "All views must carry an id.",
      class = "dock_layouts_ids_missing"
    )
  }

  if (anyDuplicated(ids) > 0L) {
    blockr_abort(
      "View ids must be unique.",
      class = "dock_layouts_ids_duplicated"
    )
  }

  # Ids key the container and become DOM / namespace ids, so they must be
  # safe identifiers (no whitespace); free-form display labels live on the
  # view as a name. The allowed set covers the styles the `ids` package
  # emits — letters, digits, and the `_` / `-` / `.` separators.
  bad <- ids[!grepl("^[A-Za-z0-9._-]+$", ids)]

  if (length(bad)) {
    blockr_abort(
      "View id{?s} {bad} must be a safe identifier (letters, digits, . - _).",
      class = "dock_view_id_invalid"
    )
  }

  for (v in x) {
    if (!is_dock_layout(v)) {
      blockr_abort(
        "All elements of `dock_layouts` must be `dock_layout` objects.",
        class = "dock_layouts_element_invalid"
      )
    }
  }

  # "Exactly one active" is structural: the container holds a single active
  # id (see `active_view()`), so two-or-more active can no longer be
  # represented and needs no count check. We only guard the field against
  # pointing at a view that isn't here.
  active <- attr(x, "active", exact = TRUE)

  if (!is.null(active) && !active %in% ids) {
    blockr_abort(
      "Active view {active} does not exist.",
      class = "dock_view_not_found"
    )
  }

  x
}

dock_layouts <- function(...) {
  validate_dock_layouts(new_dock_layouts(...))
}

#' @param x Object
#' @rdname view
#' @export
is_dock_layouts <- function(x) {
  inherits(x, "dock_layouts")
}

#' @noRd
layout_ids <- function(x) {
  stopifnot(is.list(x))
  unique(unlist(x))
}

#' @param x A `dock_layout` (for `view_name()` / `view_name<-()`) or a
#'   `dock_layouts` collection (for `view_names()`).
#' @rdname view
#' @export
view_name <- function(x) {
  # Stored under "view_name" rather than "name" so it can't partial-match
  # the "names" attribute (`attr()` matches partially by default).
  attr(x, "view_name", exact = TRUE)
}

#' @rdname view
#' @export
`view_name<-` <- function(x, value) {
  stopifnot(is_dock_layout(x), is_string(value))
  attr(x, "view_name") <- value
  x
}

#' @rdname view
#' @export
view_names <- function(x) {
  stopifnot(is_dock_layouts(x))
  set_names(chr_mply(view_label, x, names(x)), names(x))
}

# A view's display label: its explicit name, or — when unset — one
# derived from the id, the same way blockr.core derives a default block
# name from its class (underscores to spaces, capitalise the first
# letter). The id is the collection key, so this needs both.
view_label <- function(layout, id) {
  coal(view_name(layout), name_from_id(id), fail_all = FALSE)
}

name_from_id <- function(id) {
  res <- gsub("[._-]", " ", id)
  paste0(toupper(substr(res, 1L, 1L)), substring(res, 2L))
}

#' @rdname view
#' @export
active_view <- function(x) {
  UseMethod("active_view")
}

#' @export
active_view.dock_layouts <- function(x) {

  id <- attr(x, "active", exact = TRUE)

  if (is.null(id) || !id %in% names(x)) {
    return(NULL)
  }

  id
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

  attr(x, "active") <- value

  invisible(x)
}

#' @export
`active_view<-.dock_board` <- function(x, value) {
  ly <- board_layouts(x)
  active_view(ly) <- value
  board_layouts(x) <- ly
  invisible(x)
}

#' @noRd
views_can_crud <- function(x) {
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
  dock_layouts(x)
}

# Wrap an id-keyed list of views as a `dock_layouts`, recording the active
# view as a single field on the container. The active view is the one
# carrying a construction-time hint (`dock_layout(active = TRUE)`), or the
# first view when none is hinted; the per-view hints are dropped so the
# container is the sole owner of the marker.
finalize_layouts_active <- function(views) {

  hinted <- lgl_ply(views, has_active_hint)

  active <- if (any(hinted)) {
    names(views)[which(hinted)[1L]]
  } else if (length(views)) {
    names(views)[1L]
  } else {
    NULL
  }

  views <- lapply(views, `attr<-`, "active", NULL)

  structure(views, class = "dock_layouts", active = active)
}

has_active_hint <- function(x) {
  isTRUE(attr(x, "active", exact = TRUE))
}
