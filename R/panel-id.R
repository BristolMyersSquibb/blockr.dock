#' Panel IDs
#'
#' A grid names the panel of a block by its panel ID, which is the block's ID
#' with the prefix `block_panel-`, as [blk()] writes it.
#' Panel IDs inherit from `dock_id` and `dock_panel_id`, and the ID of a
#' block's panel from `block_panel_id` as well.
#' The function `as_obj_id()` converts a panel ID back to the block's ID.
#' For a character vector, `panel_obj_ids()` converts each ID that names a
#' block's panel back to the block's ID and leaves the others as they are.
#'
#' @examples
#' blks <- c(
#'   a = blockr.core::new_dataset_block(),
#'   b = blockr.core::new_head_block()
#' )
#'
#' as_dock_panel_id(blks)
#'
#' identical(names(blks), as_obj_id(as_block_panel_id(blks)))
#'
#' panel_obj_ids(c("block_panel-a", "b"))
#'
#' @return The coercion `as_block_panel_id()` returns a classed character
#' vector that inherits from `block_panel_id`, and so does
#' `as_dock_panel_id()` for one ID, while it returns a list of such vectors
#' for more than one.
#' Both `as_obj_id()` and `panel_obj_ids()` return a character vector.
#' The predicates `is_dock_panel_id()` and `is_block_panel_id()` return a
#' boolean, testing whether an object inherits from `dock_panel_id` and from
#' `block_panel_id`.
#' @rdname ids
#' @export
is_dock_panel_id <- function(x) {
  inherits(x, "dock_panel_id")
}

new_dock_id <- function(x, class = character()) {
  stopifnot(is.character(x), is.character(class))
  structure(x, class = c(class, "dock_id"))
}

new_dock_panel_id <- function(x, class = character()) {
  new_dock_id(x, c(class, "dock_panel_id"))
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

  coerce_one <- function(id) {
    if (maybe_block_panel_id(id)) {
      new_block_panel_id(id)
    } else {
      blockr_abort(
        "Cannot convert ID {id} to a `dock_panel_id` object.",
        class = "invalid_dock_panel_id_coercion"
      )
    }
  }

  # Single string: return a single classed ID.
  # Multiple strings: return a list of classed IDs.
  if (length(x) == 1L) {
    coerce_one(x)
  } else {
    lapply(x, coerce_one)
  }
}

#' @export
as_dock_panel_id.blocks <- function(x) {
  as_block_panel_id(names(x))
}

#' @export
as_dock_panel_id.dock_panel_id <- function(x) {
  x
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

#' @rdname ids
#' @export
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
as_block_panel_id.list <- function(x) {
  stopifnot(all(lgl_ply(x, is_block_panel_id)))
  as_block_panel_id(chr_ply(x, as_obj_id))
}

#' @export
as_obj_id.block_panel_id <- function(x) {
  unclass(sub("^block_panel-", "", x))
}

#' @param ids Character vector of panel IDs.
#' @rdname ids
#' @export
panel_obj_ids <- function(ids) {
  is_blk <- maybe_block_panel_id(ids)
  ids[is_blk] <- as_obj_id(new_block_panel_id(ids[is_blk]))
  ids
}
