#' Dock views: structure and grid
#'
#' A `dock_board` stores its views as two independent slots. **Structure**
#' -- which panels belong to each view, plus the view names, ids and the
#' active view -- is a `dock_views` collection of `dock_view` objects, read
#' with `board_views()`. **Grid** -- the geometry of each view (nesting, tab
#' groups, sizes) -- is a separate, `NULL`-valid `dock_grids` slot, read with
#' `board_grids()`. Single-page boards are a degenerate case: one auto-named
#' "Page" view. Blocks and extensions are shared across views via the board's
#' DAG; view membership is a layout concern only.
#'
#' Each view carries a stable, immutable **id** (its key in the collection)
#' distinct from its editable display **name**. This mirrors the id / name
#' separation used for blocks (an immutable id keys the collection;
#' [blockr.core::block_name()] is an editable label). The id is minted once
#' when the view is created and never changes -- rename only rewrites the
#' name attribute, never the key. Use `dock_view()` to construct a view,
#' `view_name()` / `view_name<-()` to read and write its display name,
#' `view_names()` for all names in a collection, and `view_members()` for a
#' `dock_view`'s ordered panel-id set.
#'
#' Multi-view boards are defined by passing `views` (and optionally `grids`)
#' to `new_dock_board()`: `views` is a named list keyed by view id (minted
#' when absent), each value a [dock_view()], a bare character vector of member
#' panel ids, or a list of panel ids. `grids` is a named list keyed by view id
#' whose values are [dock_grid()]s; it is optional -- a view with no grid entry
#' falls back to a default grid over its members wherever placement geometry
#' is needed. Bare block / extension ids in either slot are resolved to
#' canonical panel ids against the board's blocks and extensions. The
#' initially-active view is chosen by `new_dock_board(active = )` (a view id),
#' defaulting to the first; it is a property of the collection, never of an
#' individual view. View CRUD is enabled unless the dock is locked (see
#' `is_dock_locked()`).
#'
#' Structure and grid are related by total semantics, not containment: a
#' member with no grid entry is an un-landed intent, a grid entry with no
#' membership an inert ghost. Both are legal on a committed board and
#' reconciled only where placement is read (`view_grid()` prunes ghosts and
#' shows un-landed members via a default) -- the board is valid with no grid
#' at all. Referential integrity still holds: every member must reference a
#' block or extension on the board.
#'
#' @param members Ordered character vector of panel ids.
#' @param name Optional display name for the view.
#'
#' @return `board_views()` returns a `dock_views`, `board_grids()` a
#'   `dock_grids` or `NULL`, and their setters the modified board
#'   invisibly. `dock_view()` returns a `dock_view` and `view_members()` a
#'   character vector. `is_dock_view()` / `is_dock_views()` /
#'   `is_dock_grids()` return a boolean; `validate_dock_view()`,
#'   `validate_dock_views()` and `validate_dock_grids()` return their
#'   (validated) input and throw on error. `active_view()` returns the active
#'   view's id, or `NULL` when no view is active, and `active_view<-()` the
#'   modified collection (or `dock_board`) invisibly. `view_name()` returns a
#'   view's explicit display name (or `NULL`), `view_name<-()` the modified
#'   view, and `view_names()` a character vector of display labels keyed by
#'   view id (derived from the id where a view has no explicit name).
#'   `as_dock_view()` returns a `dock_view`: identity on a `dock_view`, or a
#'   view whose members are a [dock_layout][dock-layout]'s panel ids.
#'
#' @examples
#' brd <- new_dock_board(
#'   blocks = c(
#'     dataset_1 = blockr.core::new_dataset_block(),
#'     head_1 = blockr.core::new_head_block()
#'   ),
#'   views = list(
#'     analysis = dock_view(c("dataset_1", "head_1"), name = "Analysis"),
#'     overview = "dataset_1"
#'   ),
#'   active = "overview"
#' )
#' view_names(board_views(brd))
#' view_members(board_views(brd)[["analysis"]])
#'
#' @rdname view
#' @export
dock_view <- function(members = character(), name = NULL) {
  new_dock_view(members, name)
}

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
validate_dock_view <- function(x) {

  if (!is_dock_view(x)) {
    blockr_abort(
      "Expecting a `dock_view` object.",
      class = "dock_view_structure_invalid"
    )
  }

  if (!is.character(view_members(x))) {
    blockr_abort(
      "A view's members must be a character vector of panel ids.",
      class = "dock_view_members_invalid"
    )
  }

  name <- view_name(x)

  if (not_null(name) && !is_string(name)) {
    blockr_abort(
      "A view's name must be a string or `NULL`.",
      class = "dock_view_name_invalid"
    )
  }

  invisible(x)
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

finalize_views_active <- function(views) {

  res <- structure(views, class = "dock_views")

  if (length(res)) {
    active_view(res) <- names(res)[1L]
  }

  res
}

# Assemble a keyed list of views into a `dock_views`, minting an id for each
# keyless entry (like an unnamed block) with blockr.core's `rand_names()` --
# the same generator block / stack / link ids use. `reserved` lists ids
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

#' @param x An object appropriate to the function: a `dock_view` (for
#'   `view_name()`, `view_members()`), a `dock_views` collection (for
#'   `view_names()`, `active_view()`), a `dock_grids`, or a `dock_board`
#'   (for the board accessors).
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
    validate_dock_view(v)
  }

  validate_active_attr(x, names(x))

  x
}

# Members are panel ids; each must reference a block or extension on the board
# (referential integrity, `members ⊆ blocks`). Ghost arrangement entries are
# exempt -- inert and pruned at the boundary -- but a member with no backing
# object is a dangling panel, so it is rejected.
validate_view_membership <- function(views, ok_panels) {

  for (id in names(views)) {

    bad <- setdiff(view_members(views[[id]]), ok_panels)

    if (length(bad)) {
      blockr_abort(
        "View {id} member{?s} {bad} reference no block or extension.",
        class = "dock_view_membership_unknown"
      )
    }
  }

  invisible(views)
}

# Ids key the container and become DOM / namespace ids, so they must be safe
# identifiers (no whitespace); free-form display labels live on the view as a
# name.
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

  stopifnot(is_dock_view(x), is_string(value))

  attr(x, "view_name") <- value
  x
}

#' @rdname view
#' @export
view_names <- function(x) {
  stopifnot(is_dock_views(x))
  set_names(chr_mply(view_label, x, names(x)), names(x))
}

# A view's display label: its explicit name, or -- when unset -- one derived
# from the id, the same way blockr.core derives a default block name from its
# class (underscores to spaces, capitalise the first letter). The id is the
# collection key, so this needs both.
view_label <- function(view, id) {
  coal(view_name(view), name_from_id(id), fail_all = FALSE)
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
active_view.dock_views <- function(x) {

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
`active_view<-.dock_board` <- function(x, value) {
  views <- board_views(x)
  active_view(views) <- value
  board_views(x) <- views
  invisible(x)
}

views_can_crud <- function(x) {
  stopifnot(is_dock_views(x))
  !is_dock_locked()
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

# resolves ergonomic `views` / `grids` inputs. Extension keys and class ids
# both resolve to the extension's panel id.
panel_id_map <- function(blocks, extensions) {

  blocks <- as_blocks(blocks)
  ext_coll <- as_dock_extensions(extensions)

  ext_pid <- as_ext_panel_id(ext_coll)
  ext_key <- ext_alias_ids(ext_coll)
  ext_cls <- names(ext_coll)

  aliased <- ext_key != ext_cls

  set_names(
    c(ext_pid, ext_pid[aliased], as_block_panel_id(blocks)),
    c(ext_key, ext_cls[aliased], names(blocks))
  )
}

# Map a vector of ids to canonical panel ids. Only bare object ids are
# rewritten: already-canonical panel ids (not keys of the map) pass through,
# and a mix is left untouched.
resolve_panel_ids <- function(ids, id_map) {

  bare <- length(ids) && all(ids %in% names(id_map)) && any(!ids %in% id_map)

  if (!bare) {
    return(ids)
  }

  unname(id_map[ids])
}

coerce_dock_views <- function(views, id_map) {

  if (is_dock_views(views)) {
    return(validate_dock_views(views))
  }

  built <- lapply(views, coerce_one_view, id_map = id_map)

  validate_dock_views(new_dock_views(mint_view_ids(built)))
}

coerce_one_view <- function(view, id_map) {

  if (is_dock_view(view)) {
    return(
      new_dock_view(
        resolve_panel_ids(view_members(view), id_map), view_name(view)
      )
    )
  }

  if (is.character(view)) {
    return(new_dock_view(resolve_panel_ids(view, id_map)))
  }

  if (is.list(view)) {
    return(new_dock_view(resolve_panel_ids(chr_ply(view, identity), id_map)))
  }

  blockr_abort(
    "Each `views` entry must be a `dock_view`, a character vector, or a list.",
    class = "dock_views_element_invalid"
  )
}

# Drop view members with no backing block or extension. The block / extension
# set is authoritative, so a panel that references neither (e.g. a block
# dropped since the board was saved) is silently pruned from membership at
# construction rather than rejected -- keeping restore of a stale board
# robust. Each view's name and the collection's active view are preserved.
drop_unknown_members <- function(views, ok_panels) {

  for (id in names(views)) {

    members <- view_members(views[[id]])
    keep <- intersect(members, ok_panels)

    if (length(keep) < length(members)) {
      views[[id]] <- new_dock_view(keep, view_name(views[[id]]))
    }
  }

  views
}
