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

  set_names(
    c(as_ext_panel_id(ext_coll), as_block_panel_id(blocks)),
    c(names(ext_coll), names(blocks))
  )
}

# Map bare object ids to canonical panel ids, per element: a key of the map is
# rewritten, an already-canonical panel id (or an unknown id) passes through.
# Resolving element-wise lets a `blk()` / `ext()` ref (already canonical) sit
# beside a bare id in the same view.
resolve_panel_ids <- function(ids, id_map) {

  hit <- ids %in% names(id_map)
  ids[hit] <- unname(id_map[ids[hit]])

  ids
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

  if (is_panel_ref(view)) {
    view <- as_panel_string(view)
  }

  if (is.character(view)) {
    return(new_dock_view(resolve_panel_ids(view, id_map)))
  }

  if (is.list(view)) {
    return(
      new_dock_view(
        resolve_panel_ids(chr_ply(view, as_panel_string), id_map)
      )
    )
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

#' View navigation dropdown UI and dependency.
#' @noRd
view_binding_dep <- function() {
  htmltools::htmlDependency(
    "blockr-view",
    pkg_version(),
    src = pkg_file("assets", "js"),
    script = "view-binding.js"
  )
}

#' @noRd
view_nav_ui <- function(id, views) {

  ns <- NS(id)
  nav_id <- ns("view_nav")
  active <- active_view(views)
  active_nm <- unname(view_names(views)[active])
  can_crud <- views_can_crud(views)

  items <- map(
    view_item_ui,
    names(views),
    view_names(views),
    MoreArgs = list(active_id = active, can_crud = can_crud)
  )

  add_btn <- NULL
  if (can_crud) {
    add_btn <- tags$button(
      class = "dropdown-item blockr-view-add",
      bsicons::bs_icon("plus-lg"),
      "New page"
    )
  }

  div(
    class = "blockr-view-dropdown dropdown",
    view_binding_dep(),
    tags$button(
      class = "blockr-navbar-icon-btn dropdown-toggle blockr-view-toggle",
      type = "button",
      `data-bs-toggle` = "dropdown",
      `aria-expanded` = "false",
      bsicons::bs_icon("journals"),
      tags$span(class = "blockr-view-toggle-label", active_nm)
    ),
    div(
      class = "dropdown-menu blockr-view-nav",
      id = nav_id,
      items,
      if (can_crud) tags$hr(class = "dropdown-divider"),
      add_btn
    )
  )
}

#' @noRd
view_item_ui <- function(view_id, view_name, active_id = NULL,
                         can_crud = FALSE) {

  cls <- paste("dropdown-item blockr-view-item",
               if (identical(view_id, active_id)) "active" else "")

  actions <- NULL
  if (can_crud) {
    actions <- tags$span(
      class = "blockr-view-item-actions",
      tags$span(
        class = "blockr-view-action blockr-view-edit",
        role = "button",
        title = "Rename",
        bsicons::bs_icon("pencil")
      ),
      tags$span(
        class = "blockr-view-action blockr-view-remove",
        role = "button",
        title = "Remove",
        bsicons::bs_icon("x-lg")
      )
    )
  }

  tags$div(
    class = cls,
    `data-view-id` = view_id,
    tags$span(class = "blockr-view-item-name", view_name),
    actions
  )
}

# The update lifecycle owns membership, the settled-echo mirror owns geometry.
# A `views$mod` value carrying a grid would smuggle geometry into a panel-op
# batch, so it is refused at the boundary with a pointer to the mirror.
reject_geometry_in_mod <- function(mod) {

  for (v in names(mod)) {

    if (is_dock_grid(mod[[v]])) {
      blockr_abort(
        paste0(
          "`views$mod$", v, "` must be a list of panel-op verbs, not a grid: ",
          "view geometry is owned by the settled-echo grid mirror, not the ",
          "update lifecycle."
        ),
        class = "dock_views_mod_geometry_rejected"
      )
    }
  }

  invisible(mod)
}

# Normalise an inbound `views` delta to be keyed purely by stable view id.
# Existing views are addressed by id only — `mod` / `rm` / `active` carry
# ids (the one unique, stable handle; the display name is just sugar) and
# pass through untouched. The exception is `add`: a new view has no id
# yet, so its key is the desired display name and a fresh id is minted.
# `active` may also forward-reference a view added in the same delta by its
# `add` key, which is resolved here to the minted id (see below).
normalize_views_delta <- function(views, board) {

  if (length(views$add)) {

    add_keys <- names(views$add)
    views$add <- mint_added_view_ids(views$add, names(board_views(board)))

    # "Add a view and make it active" in one delta: a new view has no id
    # to name in `active` until it is minted just above, so `active` may
    # carry its `add` key (the desired display name) instead. Resolve that
    # to the minted id here, before `validate_views_delta()` checks it
    # against the post-state ids. An `active` already naming an existing
    # view passes through; add keys resolve first, so the (vanishingly
    # unlikely) clash between an add key and an existing id settles
    # deterministically in favour of the freshly added view.
    if (is_string(views$active) && views$active %in% add_keys) {
      views$active <- names(views$add)[match(views$active, add_keys)]
    }
  }

  views
}

# Mint a stable id for each added view that still needs one — unique against
# the board's existing ids — carrying the key (the desired display name) onto
# the view as its name. A view that already has a name (and thus an id key,
# from an earlier pass) is left untouched, which keeps augment idempotent:
# the update lifecycle re-runs augment until it stops changing the payload, so
# re-minting on every pass would loop a view add forever.
mint_added_view_ids <- function(add, reserved) {
  needs <- lgl_ply(add, function(x) is.null(view_name(x)))
  if (!any(needs)) {
    return(add)
  }
  add[needs] <- map(name_added_view, add[needs], names(add)[needs])
  ids <- names(add)
  ids[needs] <- rand_names(c(reserved, ids[!needs]), n = sum(needs))
  set_names(add, ids)
}

# Carry the desired display name onto an added-view entry as the attribute
# `view_name()` reads. A `dock_grid` add (seeding geometry) has no `view_name<-`
# method, so the attribute is written directly for both entry forms.
name_added_view <- function(entry, name) {
  attr(entry, "view_name") <- name
  entry
}

# Structural + cross-reference check for one added view. An add entry is a
# `dock_view` (membership only) or a `dock_grid` (seeding geometry, members
# derived as its panel ids); either way every referenced panel must resolve to
# a block or extension in the post-state.
validate_added_view <- function(entry, ok_panels, view_id) {

  if (is_dock_grid(entry)) {
    validate_dock_grid(entry)
    panels <- layout_panel_ids(entry)
  } else if (is_dock_view(entry)) {
    validate_dock_view(entry)
    panels <- view_members(entry)
  } else {
    blockr_abort(
      "`views$add${view_id}` must be a `dock_view` or a `dock_grid`.",
      class = "dock_views_delta_view_invalid"
    )
  }

  bad <- setdiff(panels, as.character(ok_panels))

  if (length(bad)) {
    blockr_abort(
      paste0(
        "Panel ID(s) in view `", view_id, "` do not resolve to current ",
        "blocks or extensions: ", paste(bad, collapse = ", "), "."
      ),
      class = "dock_views_delta_panel_ref_invalid"
    )
  }

  invisible(entry)
}

# Structural + cross-reference checks for the `views` slice. Runs after
# in-core augmentation, so panel-ID xrefs resolve against the post-state
# block set derived from `upd`.
validate_views_delta <- function(views, board, upd) {

  if (!is.list(views)) {
    blockr_abort(
      paste(
        "`views` must be a list with optional",
        "`add`/`mod`/`rm`/`active`/`rename`."
      ),
      class = "dock_views_delta_invalid"
    )
  }

  unknown_keys <- setdiff(
    names(views), c("add", "mod", "rm", "active", "rename", "grid")
  )
  if (length(unknown_keys)) {
    blockr_abort(
      "Unknown `views` slice key{?s}: {unknown_keys}.",
      class = "dock_views_delta_invalid"
    )
  }

  add_ids <- names(views$add) %||% character()
  mod_ids <- names(views$mod) %||% character()
  rm_ids <- views$rm %||% character()
  rename_ids <- names(views$rename) %||% character()
  active <- views$active

  add_unnamed <- length(views$add) &&
    (is.null(names(views$add)) || any(!nzchar(add_ids)))

  if (add_unnamed) {
    blockr_abort(
      "All entries of `views$add` must carry a view id.",
      class = "dock_views_delta_unnamed"
    )
  }

  mod_unnamed <- length(views$mod) &&
    (is.null(names(views$mod)) || any(!nzchar(mod_ids)))

  if (mod_unnamed) {
    blockr_abort(
      "All entries of `views$mod` must carry a view id.",
      class = "dock_views_delta_unnamed"
    )
  }

  if (length(rm_ids) && !is.character(rm_ids)) {
    blockr_abort(
      "`views$rm` must be a character vector of view ids.",
      class = "dock_views_delta_invalid"
    )
  }

  rename_unnamed <- length(views$rename) &&
    (is.null(names(views$rename)) || any(!nzchar(rename_ids)) ||
       !all(lgl_ply(views$rename, is_string)))

  if (rename_unnamed) {
    blockr_abort(
      "`views$rename` must map view ids to single display names.",
      class = "dock_views_delta_invalid"
    )
  }

  clash <- intersect(add_ids, rm_ids)
  if (length(clash)) {
    blockr_abort(
      "View{?s} {clash} cannot appear in both `views$add` and `views$rm`.",
      class = "dock_views_delta_add_rm_clash"
    )
  }

  mod_in_rm <- intersect(mod_ids, rm_ids)
  if (length(mod_in_rm)) {
    blockr_abort(
      "View{?s} {mod_in_rm} cannot appear in both `views$mod` and `views$rm`.",
      class = "dock_views_delta_mod_rm_clash"
    )
  }

  mod_in_add <- intersect(mod_ids, add_ids)
  if (length(mod_in_add)) {
    blockr_abort(
      paste(
        "View{?s} {mod_in_add} cannot appear in both `views$mod` and",
        "`views$add`; fold the layout into `add`."
      ),
      class = "dock_views_delta_mod_add_clash"
    )
  }

  current_views <- names(board_views(board))
  unknown_mod <- setdiff(mod_ids, current_views)
  if (length(unknown_mod)) {
    blockr_abort(
      "View{?s} {unknown_mod} in `views$mod` do not exist on the board.",
      class = "dock_views_delta_mod_unknown"
    )
  }

  unknown_rm <- setdiff(rm_ids, current_views)
  if (length(unknown_rm)) {
    blockr_abort(
      "View{?s} {unknown_rm} in `views$rm` do not exist on the board.",
      class = "dock_views_delta_rm_unknown"
    )
  }

  unknown_rename <- setdiff(rename_ids, current_views)
  if (length(unknown_rename)) {
    blockr_abort(
      "View{?s} {unknown_rename} in `views$rename` do not exist on the board.",
      class = "dock_views_delta_rename_unknown"
    )
  }

  grid_ids <- names(views$grid) %||% character()
  unknown_grid <- setdiff(grid_ids, current_views)
  if (length(unknown_grid)) {
    blockr_abort(
      paste(
        "View{?s} {unknown_grid} in `views$grid` do not exist on",
        "the board."
      ),
      class = "dock_views_delta_arrange_unknown"
    )
  }

  add_clash <- intersect(add_ids, current_views)
  if (length(add_clash)) {
    blockr_abort(
      "View{?s} {add_clash} in `views$add` already exist on the board.",
      class = "dock_views_delta_add_existing"
    )
  }

  post_views <- setdiff(c(current_views, add_ids), rm_ids)

  if (!length(post_views)) {
    blockr_abort(
      "Cannot remove all views: at least one view must remain.",
      class = "dock_views_delta_remove_all"
    )
  }

  if (!is.null(active)) {

    if (!is_string(active)) {
      blockr_abort(
        "`views$active` must be a single view id.",
        class = "dock_views_delta_active_invalid"
      )
    }

    if (!active %in% post_views) {
      blockr_abort(
        "Active view {active} does not resolve to an existing view.",
        class = "dock_views_delta_active_invalid"
      )
    }
  }

  post_block_ids <- setdiff(
    c(board_block_ids(board), names(upd$blocks$add) %||% character()),
    upd$blocks$rm %||% character()
  )

  ok_panels <- c(
    as_block_panel_id(post_block_ids),
    as_ext_panel_id(dock_ext_ids(board))
  )

  for (v in add_ids) {
    validate_added_view(views$add[[v]], ok_panels, v)
  }

  cur_views <- board_views(board)

  for (v in mod_ids) {
    validate_view_mod(
      views$mod[[v]], v, view_members(cur_views[[v]]), ok_panels
    )
  }

  invisible(views)
}

# The inner grammar of a `views$mod` entry: panel-op verbs on the panels of one
# view, mirroring the user gesture set (`add` / `rm` / `move` / `select`).
# Validation threads membership through the batch's application order (rm -> add
# -> move -> select): `rm` targets a current member, `add` a non-member that
# resolves to a block or extension, and `move` / `select` a member of the
# post-add view. Hint refs (`near`) resolve against the same running membership.
view_mod_verbs <- function() {
  c("rm", "add", "move", "select")
}

valid_panel_sides <- function() {
  c("within", "left", "right", "above", "below")
}

# The `views$mod` currency is typed refs (`blk()` / `ext()`) and bare id sugar;
# the prefix strings are the wire encoding. Augment canonicalizes each view's
# mod into the internal named-hint form the reducer and delivery observers
# consume: `add` / `move` become `list(<panel-id> = <hint>)`, `rm` a panel-id
# vector, `select` a panel id. Bare strings resolve block-first (`rm` / `move` /
# `select` / `near` against membership, `add` against the post-state block /
# extension set); a true cross-namespace clash errors and demands a typed ref.
# Idempotent -- an already-canonical mod (a re-augment pass, or the internal
# gestures / block-removal cascade) passes through untouched.
resolve_views_mod <- function(mod, board, upd) {

  post_block_ids <- setdiff(
    c(board_block_ids(board), names(upd$blocks$add) %||% character()),
    upd$blocks$rm %||% character()
  )

  ext_ids <- dock_ext_ids(board)
  views <- board_views(board)

  for (v in names(mod)) {

    if (!v %in% names(views)) {
      next
    }

    mod[[v]] <- resolve_view_mod(
      mod[[v]], v, view_members(views[[v]]), post_block_ids, ext_ids
    )
  }

  mod
}

resolve_view_mod <- function(mod, view_id, members, post_block_ids, ext_ids) {

  if (!is.list(mod)) {
    return(mod)
  }

  post_blk <- function(x) x %in% post_block_ids
  post_ext <- function(x) x %in% ext_ids
  mem_blk <- function(x) as.character(as_block_panel_id(x)) %in% members
  mem_ext <- function(x) as.character(as_ext_panel_id(x)) %in% members

  resolve_near <- function(n) {

    if (is.null(n)) {
      return(NULL)
    }

    if (is_panel_ref(n)) {

      if (length(panel_ref_hint(n))) {
        blockr_abort(
          "A `near` anchor in `views$mod${view_id}` cannot carry a hint.",
          class = "dock_views_mod_hint_invalid"
        )
      }

      return(as.character(n))
    }

    resolve_panel_str(n, mem_blk, mem_ext, view_id)
  }

  if (not_null(mod[["add"]])) {
    mod[["add"]] <- resolve_placement_verb(
      mod[["add"]], view_id, "add", post_blk, post_ext, resolve_near,
      c("near", "side", "size")
    )
  }

  if (not_null(mod[["move"]])) {
    mod[["move"]] <- resolve_placement_verb(
      mod[["move"]], view_id, "move", mem_blk, mem_ext, resolve_near,
      c("near", "side")
    )
  }

  if (not_null(mod[["rm"]])) {
    mod[["rm"]] <- resolve_plain_verb(
      mod[["rm"]], view_id, "rm", mem_blk, mem_ext
    )
  }

  if (not_null(mod[["select"]])) {

    sel <- resolve_plain_verb(
      mod[["select"]], view_id, "select", mem_blk, mem_ext
    )

    if (length(sel) != 1L) {
      blockr_abort(
        "`views$mod${view_id}$select` must be a single panel.",
        class = "dock_views_mod_invalid"
      )
    }

    mod[["select"]] <- sel
  }

  mod
}

as_operand_list <- function(x) {

  if (is_panel_ref(x)) {
    return(list(x))
  }

  if (is.character(x)) {
    return(as.list(x))
  }

  as.list(x)
}

is_ref_or_str <- function(x) {
  is_panel_ref(x) || is_string(x)
}

# Canonicalize a bare id / already-canonical string to a panel id, block-first,
# using the context predicates. A canonical string passes through; a true clash
# (the id lives in both namespaces) errors and demands a typed ref; an unknown
# bare id defaults to the block namespace and is caught by validate_view_mod.
resolve_panel_str <- function(x, is_blk, is_ext, view_id) {

  if (maybe_block_panel_id(x) || maybe_ext_panel_id(x)) {
    return(x)
  }

  if (is_blk(x) && is_ext(x)) {
    blockr_abort(
      paste0(
        "Id `", x, "` in `views$mod$", view_id, "` names both a block and an ",
        "extension -- disambiguate with blk() or ext()."
      ),
      class = "dock_views_mod_ref_clash"
    )
  }

  if (is_ext(x) && !is_blk(x)) {
    return(as.character(as_ext_panel_id(x)))
  }

  as.character(as_block_panel_id(x))
}

# A placement verb (`add` / `move`): an unnamed list of refs / bare ids, each
# canonicalized with its hint (only the verb's own hint fields), deduped by
# panel with a loud error on a conflicting duplicate. An already-canonical named
# hint map (values are hints, not refs) passes through.
resolve_placement_verb <- function(ops, view_id, verb, is_blk, is_ext,
                                   resolve_near, allowed) {

  ops <- as_operand_list(ops)

  if (!length(ops) || !all(lgl_ply(ops, is_ref_or_str))) {
    return(ops)
  }

  entries <- lapply(
    ops,
    resolve_placement_operand, view_id, verb, is_blk, is_ext, resolve_near,
    allowed
  )

  dedup_hint_entries(entries, view_id, verb)
}

resolve_placement_operand <- function(op, view_id, verb, is_blk, is_ext,
                                      resolve_near, allowed) {

  if (is_panel_ref(op)) {
    pid <- as.character(op)
    hint <- panel_ref_hint(op)
  } else {
    pid <- resolve_panel_str(op, is_blk, is_ext, view_id)
    hint <- list()
  }

  bad <- setdiff(names(hint), allowed)

  if (length(bad)) {
    blockr_abort(
      paste0(
        "`", verb, "` in `views$mod$", view_id, "` does not accept hint(s): ",
        paste(bad, collapse = ", "), "."
      ),
      class = "dock_views_mod_hint_invalid"
    )
  }

  if (not_null(hint[["near"]])) {
    hint[["near"]] <- resolve_near(hint[["near"]])
  }

  list(pid = pid, hint = hint)
}

dedup_hint_entries <- function(entries, view_id, verb) {

  out <- list()

  for (e in entries) {

    if (e[["pid"]] %in% names(out)) {

      if (!identical(out[[e[["pid"]]]], e[["hint"]])) {
        blockr_abort(
          paste0(
            "Panel `", e[["pid"]], "` appears twice in `views$mod$", view_id,
            "$", verb, "` with conflicting hints."
          ),
          class = "dock_views_mod_duplicate"
        )
      }

    } else {
      out[[e[["pid"]]]] <- e[["hint"]]
    }
  }

  out
}

# A plain verb (`rm` / `select`): refs / bare ids, no hints, canonicalized to a
# unique panel-id vector. A hinted ref here is a loud error.
resolve_plain_verb <- function(ops, view_id, verb, is_blk, is_ext) {

  pids <- chr_ply(
    as_operand_list(ops),
    function(op) {

      if (is_panel_ref(op)) {

        if (length(panel_ref_hint(op))) {
          blockr_abort(
            paste0(
              "`", verb, "` in `views$mod$", view_id, "` takes plain refs; ",
              "a placement hint is not meaningful here."
            ),
            class = "dock_views_mod_hint_invalid"
          )
        }

        return(as.character(op))
      }

      resolve_panel_str(op, is_blk, is_ext, view_id)
    }
  )

  unique(pids)
}

validate_view_mod <- function(mod, view_id, members, ok_panels) {

  if (!is.list(mod)) {
    blockr_abort(
      "`views$mod${view_id}` must be a list of panel-op verbs.",
      class = "dock_views_mod_invalid"
    )
  }

  unknown <- setdiff(names(mod), view_mod_verbs())

  if (length(unknown)) {
    blockr_abort(
      paste0(
        "Unknown panel-op verb(s) in `views$mod$", view_id, "`: ",
        paste(unknown, collapse = ", "), "."
      ),
      class = "dock_views_mod_verb_unknown"
    )
  }

  rm_ids <- mod$rm %||% character()

  if (!is.character(rm_ids)) {
    blockr_abort(
      "`views$mod${view_id}$rm` must be a character vector of panel ids.",
      class = "dock_views_mod_invalid"
    )
  }

  bad_rm <- setdiff(rm_ids, members)

  if (length(bad_rm)) {
    blockr_abort(
      paste0(
        "Panel(s) in `views$mod$", view_id, "$rm` are not view members: ",
        paste(bad_rm, collapse = ", "), "."
      ),
      class = "dock_views_mod_rm_unknown"
    )
  }

  post_rm <- setdiff(members, rm_ids)

  add_ids <- validate_mod_map(mod$add, view_id, "add")

  bad_add <- setdiff(add_ids, as.character(ok_panels))

  if (length(bad_add)) {
    blockr_abort(
      paste0(
        "Panel ID(s) in `views$mod$", view_id, "$add` do not resolve to ",
        "current blocks or extensions: ", paste(bad_add, collapse = ", "), "."
      ),
      class = "dock_views_delta_panel_ref_invalid"
    )
  }

  member_add <- intersect(add_ids, post_rm)

  if (length(member_add)) {
    blockr_abort(
      paste0(
        "Panel(s) in `views$mod$", view_id, "$add` are already view members: ",
        paste(member_add, collapse = ", "), "."
      ),
      class = "dock_views_mod_add_existing"
    )
  }

  post_add <- c(post_rm, add_ids)

  # An add's `near` anchors against a pre-existing member (post-rm), never a
  # sibling add: delivery walks the add map in payload order, so a forward
  # reference to a not-yet-added panel would reach dockview as a dangling
  # `referencePanel`. A move's `near` may target any post-add member (moves
  # apply after every add).
  for (pid in add_ids) {
    validate_panel_hint(mod$add[[pid]], view_id, post_rm)
  }

  move_ids <- validate_mod_map(mod$move, view_id, "move")

  bad_move <- setdiff(move_ids, post_add)

  if (length(bad_move)) {
    blockr_abort(
      paste0(
        "Panel(s) in `views$mod$", view_id, "$move` are not view members: ",
        paste(bad_move, collapse = ", "), "."
      ),
      class = "dock_views_mod_move_unknown"
    )
  }

  for (pid in move_ids) {
    validate_panel_hint(mod$move[[pid]], view_id, setdiff(post_add, pid))
  }

  sel <- mod$select

  if (!is.null(sel)) {

    if (!is_string(sel)) {
      blockr_abort(
        "`views$mod${view_id}$select` must be a single panel id.",
        class = "dock_views_mod_invalid"
      )
    }

    if (!sel %in% post_add) {
      blockr_abort(
        "Panel {sel} in `views$mod${view_id}$select` is not a view member.",
        class = "dock_views_mod_select_unknown"
      )
    }
  }

  invisible(mod)
}

# A keyed panel-op map (`add` / `move`): a named list whose keys are panel ids
# and whose values are placement hints. Returns the panel ids.
validate_mod_map <- function(map, view_id, verb) {

  if (is.null(map)) {
    return(character())
  }

  nms <- names(map)

  if (!is.list(map) || (length(map) && (is.null(nms) || any(!nzchar(nms))))) {
    blockr_abort(
      "`views$mod${view_id}${verb}` must be a list keyed by panel id.",
      class = "dock_views_mod_invalid"
    )
  }

  nms %||% character()
}

# Validate a resolved placement hint's values. The keys are already constrained
# -- they come from the `blk()` / `ext()` constructor and the verb's contextual
# whitelist in resolution -- so this checks only that `near` resolves to a
# member (the running `anchors` set), `side` is in dockview's drop vocabulary,
# and `size` is a ratio in (0, 1). No hint at all falls back to
# `determine_panel_pos()` at delivery.
validate_panel_hint <- function(hint, view_id, anchors) {

  if (is.null(hint) || !length(hint)) {
    return(invisible())
  }

  near <- hint[["near"]]

  if (not_null(near) && !near %in% anchors) {
    blockr_abort(
      "Hint `near` {near} in `views$mod${view_id}` is not a view member.",
      class = "dock_views_mod_hint_invalid"
    )
  }

  side <- hint[["side"]]

  if (not_null(side) && !isTRUE(side %in% valid_panel_sides())) {
    blockr_abort(
      paste0(
        "Hint `side` ", side, " in `views$mod$", view_id, "` must be one of: ",
        paste(valid_panel_sides(), collapse = ", "), "."
      ),
      class = "dock_views_mod_hint_invalid"
    )
  }

  size <- hint[["size"]]

  if (not_null(size) && !(is_number(size) && size > 0 && size < 1)) {
    blockr_abort(
      "Hint `size` in `views$mod${view_id}` must be a ratio in (0, 1).",
      class = "dock_views_mod_hint_invalid"
    )
  }

  invisible()
}

# Merge the block-removal cleanup into a user-supplied `views$mod`: each removed
# block's panel, wherever it is still a view member, joins that view's `rm`
# verb. The removal cascade is an `rm`, composing by key with whatever the user
# batched -- per-view, per-verb maps merge rather than clobber.
merge_views_mod <- function(user_mod, views, rm_block_ids,
                            skip_views = character()) {

  out <- user_mod %||% list()

  if (!length(rm_block_ids)) {
    return(out)
  }

  rm_panels <- as.character(as_block_panel_id(rm_block_ids))

  for (v in setdiff(names(views), skip_views)) {

    present <- intersect(view_members(views[[v]]), rm_panels)

    if (length(present)) {
      out[[v]][["rm"]] <- union(out[[v]][["rm"]] %||% character(), present)
    }
  }

  out
}

# Write a view's membership as a set. Geometry is untouched: a shrink can leave
# an inert ghost in the arrangement and a grow an un-landed member, both legal
# under total semantics and reconciled only at the compose / restore boundary.
set_view_membership <- function(board, view_id, members) {

  views <- board_views(board)

  views[[view_id]] <- new_dock_view(
    as.character(members), view_name(views[[view_id]])
  )

  board_views(board) <- views

  board
}

apply_views_rm <- function(rm_ids, board) {

  views <- board_views(board)
  old_active <- active_view(views)
  surviving <- setdiff(names(views), rm_ids)

  for (v in rm_ids) {
    views[[v]] <- NULL
  }

  if (length(surviving) &&
        (is.null(old_active) || !old_active %in% surviving)) {
    active_view(views) <- surviving[[1L]]
  }

  arr <- board_grids(board)

  if (not_null(arr)) {
    for (v in rm_ids) {
      arr[[v]] <- NULL
    }
  }

  board_views(board) <- views
  board_grids(board) <- arr

  board
}

apply_views_add <- function(add_views, board) {

  views <- board_views(board)
  grids <- as.list(board_grids(board) %||% list())
  seeded <- FALSE

  # Adding a view never changes which one is active: that travels on the
  # delta's `active` slot (resolved to the new view's id in
  # `normalize_views_delta()` when it names an add) and is applied by
  # `apply_views_active()` once the view exists. An entry is either a
  # `dock_view` (membership only -- geometry then follows from the client echo
  # through the settled-echo mirror, rendering flat until then) or a `dock_grid`
  # seeding initial geometry, whose membership is its panel ids and whose grid
  # seeds `board_grids()` so the view renders that arrangement immediately.
  for (v in names(add_views)) {

    entry <- add_views[[v]]

    if (is_dock_grid(entry)) {

      views[[v]] <- new_dock_view(layout_panel_ids(entry), view_name(entry))

      attr(entry, "view_name") <- NULL
      grids[[v]] <- entry
      seeded <- TRUE

    } else {

      views[[v]] <- entry
    }
  }

  board_views(board) <- views

  if (seeded) {
    board_grids(board) <- new_dock_grids(grids)
  }

  board
}

# Apply the panel-op batch for each modified view. Only `add` and `rm` are slot
# writers -- they grow / shrink membership (rm then add, so a re-place works);
# `move` and `select` write nothing (geometry and the active tab are client-
# owned, captured by the settled-echo mirror). Placement hints are consumed at
# delivery, never stored.
apply_views_mod <- function(mod_views, board) {

  for (v in names(mod_views)) {
    board <- apply_view_mod(board, v, mod_views[[v]])
  }

  board
}

apply_view_mod <- function(board, view_id, mod) {

  if (is.null(mod[["add"]]) && is.null(mod[["rm"]])) {
    return(board)
  }

  members <- view_members(board_views(board)[[view_id]])

  members <- setdiff(members, mod[["rm"]] %||% character())
  members <- union(members, names(mod[["add"]]) %||% character())

  set_view_membership(board, view_id, members)
}

# Apply the grid mirror's echo: store each view's settled layout verbatim in
# canonical form, eliding a plain default to NULL. No membership projection --
# containment is boundary hygiene, not a write-time concern. The mirror is the
# sole producer of this slice.
apply_views_grid <- function(grid, board) {

  arr <- as.list(board_grids(board) %||% list())
  views <- board_views(board)

  for (v in names(grid)) {

    if (!v %in% names(views)) {
      next
    }

    ly <- grid[[v]]

    if (is.null(ly)) {
      arr[[v]] <- NULL
      next
    }

    arr[[v]] <- as_dock_grid(ly)
  }

  board_grids(board) <- new_dock_grids(arr)

  board
}

# Rename one or more views: a name-attribute write keyed by stable id.
# `rename` is a named list mapping view id to its new display name. The
# id is untouched, so the dock module, DOM element and registry key all
# survive -- no rebuild, no re-keying.
apply_views_rename <- function(rename, board) {

  views <- board_views(board)

  for (id in names(rename)) {
    if (id %in% names(views)) {
      view_name(views[[id]]) <- rename[[id]]
    }
  }

  board_views(board) <- views

  board
}

apply_views_active <- function(active, board) {

  views <- board_views(board)
  active_view(views) <- active
  board_views(board) <- views

  board
}

# Runtime switch to view `active` (by id): activate its dock in the DOM,
# move the block / ext UIs across, and point active_dock and the nav at
# it. The board's active marker is set by the caller; this drives the
# live session state.
switch_active_view <- function(active, docks, active_dock, client_active,
                               brd, session) {

  old <- isolate(client_active())

  if (identical(old, active)) {
    return(invisible())
  }

  if (active %in% names(docks)) {

    # switch-view must fire before show_view_ui so the target dock carries
    # blockr-view-dock-active; otherwise move-element drops DOM moves into an
    # inactive dock.
    session$sendCustomMessage(
      "switch-view",
      list(id = session$ns(as_view_handle_id(active)))
    )

    hide_view_ui(old, docks)

    # Build the target view's cards if this is its first visit, before
    # show_view_ui moves them into the now-active dock.
    active_blocks <- board_blocks(brd)[
      view_block_ids(board_views(brd)[[active]])
    ]
    ensure_block_ui(
      session$ns(NULL), brd, active_blocks, active_dock$visibility,
      session = session
    )

    show_view_ui(active, docks)
    update_active_dock(active_dock, docks[[active]])
  }

  client_active(active)

  session$sendInputMessage(
    "view_nav",
    list(value = active)
  )

  invisible()
}
