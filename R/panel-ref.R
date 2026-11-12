#' Typed panel references
#'
#' The function `blk()` constructs a reference to a block's panel for the
#' functions that write a grid, such as [dock_grid()][layout] and [panels()].
#' A caller names a block by its **id**, never by the `block_panel-` prefix of
#' its panel id: the ref is the codec, and `as.character()` on one yields the
#' canonical panel id.
#' Bare id strings are accepted wherever a ref is.
#'
#' @param id A block id, not the prefixed panel id.
#' @param x An object.
#' @param ... Ignored.
#'
#' @return The constructor `blk()` returns a `panel_ref`.
#'   Calling `as.character()` on one returns its canonical panel id, and
#'   `is_panel_ref()` returns a boolean.
#'
#' @examples
#' blk("my_block")
#' as.character(blk("my_block"))
#'
#' @rdname panel-ref
#' @export
blk <- function(id) {
  new_panel_ref(id, "block")
}

new_panel_ref <- function(id, type) {

  stopifnot(is_string(id))

  structure(list(id = id, type = type), class = "panel_ref")
}

#' @rdname panel-ref
#' @export
is_panel_ref <- function(x) {
  inherits(x, "panel_ref")
}

#' @rdname panel-ref
#' @export
as.character.panel_ref <- function(x, ...) {
  as.character(as_block_panel_id(x[["id"]]))
}

#' @export
print.panel_ref <- function(x, ...) {
  cat("<panel_ref>", as.character(x), "\n", sep = " ")
  invisible(x)
}

# A ref used in the functions that write a grid contributes its canonical
# panel id, and a bare string passes through.
as_panel_string <- function(x) {

  if (!is_panel_ref(x)) {
    return(x)
  }

  as.character(x)
}
