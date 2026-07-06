#' Dock views: structure and grid
#'
#' A `dock_board` stores its views as two independent slots. **Structure**
#' — which panels belong to each view, plus the view names, ids and the
#' active view — is a `dock_views` collection of `dock_view` objects, read
#' with `board_views()`. **Grid** — the geometry of each view (nesting, tab
#' groups, sizes) — is a separate, `NULL`-valid `dock_grids` slot, read with
#' `board_grids()`. `board_layouts()` composes the two into the
#' grid-bearing `dock_layouts` handle the update lifecycle still reads.
#' Single-page boards are a degenerate case: one auto-named "Page" view.
#' Blocks and extensions are shared across views via the board's DAG; view
#' membership is a layout concern only.
#'
#' Each view carries a stable, immutable **id** (its key in the collection)
#' distinct from its editable display **name**. This mirrors the id / name
#' separation used for blocks (an immutable id keys the collection;
#' [blockr.core::block_name()] is an editable label). The id is minted once
#' when the view is created and never changes — rename only rewrites the
#' name attribute, never the key. Use `view_name()` / `view_name<-()` to
#' read and write a view's display name, `view_names()` for all names in a
#' collection, and `view_members()` for a `dock_view`'s ordered panel-id
#' set.
#'
#' Multi-view boards are defined by passing a named list to
#' `new_dock_board()`'s `layouts` argument: each **name is the view's id**
#' (the container's key, like a block id — minted when absent), each value
#' is the panel arrangement inside that view (a [dock_layout()], or a raw
#' list of block / extension IDs), split into the structure and grid
#' slots at construction. The display name is set on the layout via
#' `dock_layout(name = )`; with none set, a label is derived from the id.
#' The initially-active view is chosen by `new_dock_board(active = )` (a
#' view id), defaulting to the first; it is a property of the collection,
#' never of an individual layout. View CRUD is enabled unless the dock is
#' locked (see `is_dock_locked()`).
#'
#' A stored grid must reference only panels in its view's membership
#' (`grid ⊆ membership`); the board is valid with no grid at
#' all. Users do not normally construct these objects directly; they pass a
#' plain named list to `new_dock_board(layouts = ...)`, which resolves,
#' validates and splits it.
#'
#' @return `board_views()` returns a `dock_views`, `board_grids()` a
#'   `dock_grids` or `NULL`, and their setters the modified board
#'   invisibly. `view_members()` returns a character vector. `is_dock_view()`
#'   / `is_dock_views()` / `is_dock_grids()` / `is_dock_layouts()`
#'   return a boolean; `validate_dock_views()`, `validate_dock_grids()`
#'   and `validate_dock_layouts()` return their (validated) input and throw
#'   on error. `active_view()` returns the active view's id, or `NULL` when
#'   no view is active, and `active_view<-()` the modified collection (or
#'   `dock_board`) invisibly. `view_name()` returns a view's explicit
#'   display name (or `NULL`), `view_name<-()` the modified object, and
#'   `view_names()` a character vector of display labels keyed by view id
#'   (derived from the id where a view has no explicit name).
#'
#' @examples
#' brd <- new_dock_board(
#'   blocks = c(
#'     dataset_1 = blockr.core::new_dataset_block(),
#'     head_1 = blockr.core::new_head_block()
#'   ),
#'   layouts = list(
#'     analysis = dock_layout("dataset_1", "head_1", name = "Analysis"),
#'     overview = dock_layout("dataset_1", name = "Overview")
#'   ),
#'   active = "overview"
#' )
#' view_names(board_views(brd))
#' view_members(board_views(brd)[["analysis"]])
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

#' @export
str_value.dock_layouts <- function(x, ...) {

  ids <- names(x)

  marks <- rep("", length(x))
  active <- active_view(x)

  if (not_null(active)) {
    marks[ids == active] <- " (active)"
  }

  lines <- paste0("  ", ids, ": ", chr_ply(x, str_value), marks)

  paste(
    c(paste0("<dock_layouts[", length(x), "]>"), lines),
    collapse = "\n"
  )
}

#' @importFrom utils str
#' @export
str.dock_layouts <- function(object, ...) {
  cat(str_value(object), "\n", sep = "")
  invisible(object)
}

#' @noRd
layout_ids <- function(x) {
  stopifnot(is.list(x))
  unique(unlist(x))
}

#' @param x An object appropriate to the function: a `dock_view` /
#'   `dock_layout` (for `view_name()`, `view_members()`), a `dock_views` /
#'   `dock_layouts` collection (for `view_names()`, `active_view()`), a
#'   `dock_grids`, or a `dock_board` (for the board accessors).
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
  stopifnot(is_dock_layout(x) || is_dock_view(x), is_string(value))
  attr(x, "view_name") <- value
  x
}

#' @rdname view
#' @export
view_names <- function(x) {
  stopifnot(is_dock_layouts(x) || is_dock_views(x))
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
  active_view(board_views(x))
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
  stopifnot(is_dock_layouts(x) || is_dock_views(x))
  !is_dock_locked()
}

#' @rdname view
#' @export
`board_layouts<-` <- function(x, value) {
  stopifnot(is_dock_board(x))
  parts <- decompose_layouts(validate_dock_layouts(value))
  board_views(x) <- parts[["views"]]
  board_grids(x) <- parts[["grids"]]
  invisible(x)
}

# The board stores structure (`board_views()`) and geometry
# (`board_grids()`) as separate slots; `board_layouts()` composes the
# grid-bearing `dock_layouts` the update lifecycle still reads. A bridge until
# #294 rewires the flow onto the two slots directly.
#' @rdname view
#' @export
board_layouts <- function(x) {
  stopifnot(is_dock_board(x))
  compose_layouts(board_views(x), board_grids(x))
}

#' @param ... Generic consistency
#' @rdname view
#' @export
as_dock_layouts <- function(x, ...) {
  UseMethod("as_dock_layouts")
}

#' @export
as_dock_layouts.dock_layouts <- function(x, ...) {
  validate_dock_layouts(x)
}

#' @export
as_dock_layouts.dock_layout <- function(x, blocks = NULL, extensions = NULL,
                                        ...) {
  if (not_null(blocks) || not_null(extensions)) {
    x <- resolve_dock_layout(coal(blocks, list()), coal(extensions, list()), x)
  }
  dock_layouts(x)
}

#' @export
as_dock_layouts.list <- function(x, blocks = NULL, extensions = NULL, ...) {

  # Multi-view: a list keyed by view id, or one holding `dock_layout`s
  # (keyless — ids minted). A `grid`-keyed list (dockview's internal
  # single-view form) or a bare panel-id list is a single view.
  multi_view <- !("grid" %in% names(x)) &&
    (not_null(names(x)) || any(lgl_ply(x, is_dock_layout)))

  if (multi_view) {
    return(
      resolve_views(
        x,
        as_blocks(coal(blocks, list())),
        as_dock_extensions(coal(extensions, list()))
      )
    )
  }

  # Single view: `coerce_view_spec()` turns the grid-list or bare panel-id
  # list into a `dock_layout`, which the `dock_layout` method then resolves
  # and wraps.
  as_dock_layouts(coerce_view_spec(x), blocks = blocks, extensions = extensions)
}

#' @export
as_dock_layouts.default <- function(x, ...) {
  blockr_abort(
    "Cannot coerce a {class(x)[[1L]]} object to a `dock_layouts`.",
    class = "dock_layouts_coerce_invalid"
  )
}

# Wrap an id-keyed list of views as a `dock_layouts`, recording the active
# view as a single field on the container. Active defaults to the first
# view, written through the `active_view<-()` setter; callers choose a
# different one that way (or, at board construction,
# `new_dock_board(active = )`). A view never carries its own active marker
# — the container is the sole owner.
finalize_layouts_active <- function(views) {

  res <- structure(views, class = "dock_layouts")

  if (length(res)) {
    active_view(res) <- names(res)[1L]
  }

  res
}
