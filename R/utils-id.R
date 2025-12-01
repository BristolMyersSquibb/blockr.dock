#' ID utilities
#'
#' Objects, such as blocks and dock extensions carry their own IDs. These
#' can be converted into other ID types, such as panel IDs or "handle" IDs.
#' Panel IDs are used to refer to dock panels, while handle IDs provide
#' "handles" for DOM manipulations. All such IDs inherit from `dock_id` and
#' panel IDs additionally inherit from `dock_panel_id`, while handle IDs
#' inherit from `dock_handle_id`. For panel IDs, depending on whether the panel
#' is showing a block or an extension, the inheritance structure additionally
#' contains `block_panel_id` or `ext_panel_id`, respectively. Similarly, for
#' handle IDs, we have `block_handle_id` and `ext_handle_id`. All `dock_id`
#' objects can be converted back to native IDs, by calling `as_obj_id()`.
#' The utility function `dock_id()` returns a (possibly namespaced) ID of the
#' `dock` instance that is used to manage all visible panels.
#'
#' @param ns Namespace prefix
#'
#' @examples
#' blks <- c(
#'   a = blockr.core::new_dataset_block(),
#'   b = blockr.core::new_head_block()
#' )
#'
#' ext <- new_edit_board_extension()
#'
#' as_dock_panel_id(blks)
#' as_dock_panel_id(ext)
#'
#' identical(names(blks), as_obj_id(as_block_panel_id(blks)))
#'
#' as_dock_handle_id(blks)
#' as_dock_handle_id(ext)
#'
#' identical(names(blks), as_obj_id(as_block_handle_id(blks)))
#'
#' @return Coercion functions `as_block_panel_id()`, `as_ext_panel_id()`,
#' `as_block_handle_id()` and `as_ext_handle_id()` return objects that inherit
#' from `block_panel_id`, `ext_panel_id`, `block_handle_id` and
#' `ext_handle_id` as classed character vectors. The less specific coercion
#' functions `as_dock_panel_id()` and `as_dock_handle_id()` return objects
#' that inherit from `dock_panel_id` and `dock_handle_id`, in addition to
#' a sub-class such as `block_panel_id` or `ext_panel_id` (in the case of
#' `as_dock_panel_id()`). If a mix of sub-classes is returned, this will be
#' represented by a list of classed character vectors. Finally, `as_obj_id()`
#' returns a character vector, as does `dock_id()`.
#' @rdname ids
#' @export
dock_id <- function(ns = NULL) {

  res <- "dock"

  if (is.null(ns)) {
    return(res)
  }

  ns(res)
}

new_dock_id <- function(x, class = character()) {
  stopifnot(is.character(x), is.character(class))
  structure(x, class = c(class, "dock_id"))
}

is_dock_id <- function(x) {
  inherits(x, "dock_id")
}

new_dock_panel_id <- function(x, class = character()) {
  new_dock_id(x, c(class, "dock_panel_id"))
}

is_dock_panel_id <- function(x) {
  inherits(x, "dock_panel_id")
}

new_dock_handle_id <- function(x, class = character()) {
  new_dock_id(x, c(class, "dock_handle_id"))
}

is_dock_handle_id <- function(x) {
  inherits(x, "dock_handle_id")
}

#' @param x Object
#' @rdname ids
#' @export
as_dock_panel_id <- function(x) {
  if (length(x)) {
    UseMethod("as_dock_panel_id")
  } else {
    character()
  }
}

#' @export
as_dock_panel_id.character <- function(x) {

  if (length(x) > 1L) {
    return(lapply(x, as_dock_panel_id))
  }

  stopifnot(is_string(x))

  if (maybe_block_panel_id(x)) {
    new_block_panel_id(x)
  } else if (maybe_ext_panel_id(x)) {
    new_ext_panel_id(x)
  } else {
    blockr_abort(
      "Cannot convert ID {x} to a `dock_panel_id` object.",
      class = "invalid_dock_panel_id_coercion"
    )
  }
}

#' @export
as_dock_panel_id.board <- function(x) {
  c(
    lapply(board_block_ids(x), as_block_panel_id),
    lapply(dock_ext_ids(x), as_ext_panel_id)
  )
}

#' @export
as_dock_panel_id.dock_layout <- function(x) {
  lapply(layout_panel_ids(x), as_dock_panel_id)
}

#' @export
as_dock_panel_id.blocks <- function(x) {
  as_block_panel_id(names(x))
}

#' @export
as_dock_panel_id.dock_extensions <- function(x) {
  as_ext_panel_id(names(x))
}

#' @export
as_dock_panel_id.dock_extension <- function(x) {
  as_ext_panel_id(extension_id(x))
}

#' @param x Object
#' @rdname ids
#' @export
as_obj_id <- function(x) {
  if (length(x)) {
    UseMethod("as_obj_id")
  } else {
    character()
  }
}

#' @export
as_obj_id.dock_panel_id <- function(x) {
  blockr_abort(
    "Conversion to `obj_id` not implemented for class{?es} {class(x)}.",
    class = "invalid_obj_id_coercion"
  )
}

#' @export
as_obj_id.character <- function(x) x

#' @export
as_obj_id.list <- function(x) {
  chr_ply(x, as_obj_id)
}

new_block_panel_id <- function(x) {
  new_dock_panel_id(x, "block_panel_id")
}

is_block_panel_id <- function(x) {
  inherits(x, "block_panel_id")
}

maybe_block_panel_id <- function(x) {
  grepl("^block_panel-", x)
}

#' @rdname ids
#' @export
as_block_panel_id <- function(x) {
  if (length(x)) {
    UseMethod("as_block_panel_id")
  } else {
    character()
  }
}

#' @export
as_block_panel_id.block_panel_id <- function(x) x

#' @export
as_block_panel_id.dock_id <- function(x) {
  blockr_abort(
    "Cannot convert an ID with class{?es} {class(x)} to `block_panel_id`.",
    class = "invalid_block_panel_id_coercion"
  )
}

#' @export
as_block_panel_id.character <- function(x) {

  warn <- maybe_block_panel_id(x)

  if (any(warn)) {
    blockr_warn(
      "Potentially converting ID{?s} {x[warn]} again.",
      class = "maybe_multiple_block_id_conversion"
    )
  }

  new_block_panel_id(paste0("block_panel-", x))
}

#' @export
as_block_panel_id.blocks <- function(x) {
  as_block_panel_id(names(x))
}

#' @export
as_block_panel_id.block_handle_id <- function(x) {
  as_block_panel_id(as_obj_id(x))
}

#' @export
as_block_panel_id.list <- function(x) {
  stopifnot(all(lgl_ply(x, is_block_panel_id)))
  as_block_panel_id(chr_ply(x, as_obj_id))
}

#' @export
as_obj_id.block_panel_id <- function(x) {
  unclass(sub("^block_panel-", "", x))
}

new_ext_panel_id <- function(x) {
  new_dock_panel_id(x, "ext_panel_id")
}

is_ext_panel_id <- function(x) {
  inherits(x, "ext_panel_id")
}

maybe_ext_panel_id <- function(x) {
  grepl("^ext_panel-", x)
}

#' @rdname ids
#' @export
as_ext_panel_id <- function(x) {
  if (length(x)) {
    UseMethod("as_ext_panel_id")
  } else {
    character()
  }
}

#' @export
as_ext_panel_id.ext_panel_id <- function(x) x

#' @export
as_ext_panel_id.dock_id <- function(x) {
  blockr_abort(
    "Cannot convert an ID with class{?es} {class(x)} to `ext_panel_id`.",
    class = "invalid_ext_panel_id_coercion"
  )
}

#' @export
as_ext_panel_id.character <- function(x) {

  warn <- maybe_ext_panel_id(x)

  if (any(warn)) {
    blockr_warn(
      "Potentially converting ID{?s} {x[warn]} again.",
      class = "maybe_multiple_ext_id_conversion"
    )
  }

  new_ext_panel_id(paste0("ext_panel-", x))
}

#' @export
as_ext_panel_id.dock_extension <- function(x) {
  as_ext_panel_id(extension_id(x))
}

#' @export
as_ext_panel_id.dock_extensions <- function(x) {
  as_ext_panel_id(chr_ply(x, extension_id))
}

#' @export
as_ext_panel_id.ext_handle_id <- function(x) {
  as_ext_panel_id(as_obj_id(x))
}

#' @export
as_ext_panel_id.list <- function(x) {
  stopifnot(all(lgl_ply(x, is_ext_panel_id)))
  as_ext_panel_id(chr_ply(x, as_obj_id))
}

#' @export
as_obj_id.ext_panel_id <- function(x) {
  unclass(sub("^ext_panel-", "", x))
}

new_block_handle_id <- function(x) {
  new_dock_handle_id(x, "block_handle_id")
}

is_block_handle_id <- function(x) {
  inherits(x, "block_handle_id")
}

maybe_block_handle_id <- function(x) {
  grepl("^block_handle-", x)
}

#' @rdname ids
#' @export
as_dock_handle_id <- function(x) {
  if (length(x)) {
    UseMethod("as_dock_handle_id")
  } else {
    character()
  }
}

#' @export
as_dock_handle_id.character <- function(x) {

  if (length(x) > 1L) {
    return(lapply(x, as_dock_handle_id))
  }

  stopifnot(is_string(x))

  if (maybe_block_handle_id(x)) {
    new_block_handle_id(x)
  } else if (maybe_ext_handle_id(x)) {
    new_ext_handle_id(x)
  } else {
    blockr_abort(
      "Cannot convert ID {x} to a `dock_handle_id` object.",
      class = "invalid_dock_handle_id_coercion"
    )
  }
}

#' @export
as_dock_handle_id.board <- function(x) {
  c(
    lapply(board_block_ids(x), as_block_handle_id),
    lapply(dock_ext_ids(x), as_ext_handle_id)
  )
}

#' @export
as_dock_handle_id.dock_layout <- function(x) {
  lapply(layout_panel_ids(x), as_dock_handle_id)
}

#' @export
as_dock_handle_id.blocks <- function(x) {
  as_block_handle_id(names(x))
}

#' @export
as_dock_handle_id.dock_extensions <- function(x) {
  as_ext_handle_id(names(x))
}

#' @export
as_dock_handle_id.dock_extension <- function(x) {
  as_ext_handle_id(extension_id(x))
}

#' @rdname ids
#' @export
as_block_handle_id <- function(x) {
  if (length(x)) {
    UseMethod("as_block_handle_id")
  } else {
    character()
  }
}

#' @export
as_block_handle_id.block_handle_id <- function(x) x

#' @export
as_block_handle_id.dock_id <- function(x) {
  blockr_abort(
    "Cannot convert an ID with class{?es} {class(x)} to `block_handle_id`.",
    class = "invalid_block_handle_id_coercion"
  )
}

#' @export
as_block_handle_id.character <- function(x) {

  warn <- maybe_block_handle_id(x)

  if (any(warn)) {
    blockr_warn(
      "Potentially converting ID{?s} {x[warn]} again.",
      class = "maybe_multiple_block_id_conversion"
    )
  }

  new_block_handle_id(paste0("block_handle-", x))
}

#' @export
as_block_handle_id.blocks <- function(x) {
  as_block_handle_id(names(x))
}

#' @export
as_block_handle_id.block_panel_id <- function(x) {
  as_block_handle_id(as_obj_id(x))
}

#' @export
as_block_handle_id.list <- function(x) {
  stopifnot(all(lgl_ply(x, is_block_handle_id)))
  as_block_handle_id(chr_ply(x, as_obj_id))
}

#' @export
as_obj_id.block_handle_id <- function(x) {
  unclass(sub("^block_handle-", "", x))
}

new_ext_handle_id <- function(x) {
  new_dock_handle_id(x, "ext_handle_id")
}

is_ext_handle_id <- function(x) {
  inherits(x, "ext_handle_id")
}

maybe_ext_handle_id <- function(x) {
  grepl("^ext_handle-", x)
}

#' @rdname ids
#' @export
as_ext_handle_id <- function(x) {
  if (length(x)) {
    UseMethod("as_ext_handle_id")
  } else {
    character()
  }
}

#' @export
as_ext_handle_id.ext_handle_id <- function(x) x

#' @export
as_ext_handle_id.ext_panel_id <- function(x) {
  as_ext_handle_id(as_obj_id(x))
}

#' @export
as_ext_handle_id.dock_id <- function(x) {
  blockr_abort(
    "Cannot convert an ID with class{?es} {class(x)} to `ext_handle_id`.",
    class = "invalid_ext_handle_id_coercion"
  )
}

#' @export
as_ext_handle_id.character <- function(x) {

  warn <- maybe_ext_handle_id(x)

  if (any(warn)) {
    blockr_warn(
      "Potentially converting ID{?s} {x[warn]} again.",
      class = "maybe_multiple_block_id_conversion"
    )
  }

  new_ext_handle_id(paste0("ext_handle-", x))
}

#' @export
as_ext_handle_id.dock_extension <- function(x) {
  as_ext_handle_id(extension_id(x))
}

#' @export
as_ext_handle_id.block_panel_id <- function(x) {
  as_ext_handle_id(as_obj_id(x))
}

#' @export
as_ext_handle_id.list <- function(x) {
  stopifnot(all(lgl_ply(x, is_ext_handle_id)))
  chr_ply(x, as_ext_handle_id)
}

#' @export
as_obj_id.ext_handle_id <- function(x) {
  unclass(sub("^ext_handle-", "", x))
}
