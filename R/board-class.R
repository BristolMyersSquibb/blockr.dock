#' Dock board
#'
#' Using the docking layout manager provided by dockViewR, a `dock_board`
#' extends [blockr.core::new_board()]. In addition to the attributes contained
#' in a core board, this also includes dock extensions (as `extensions`)
#' and the panel arrangement (as `layout`).
#'
#' @inheritParams blockr.core::new_board
#' @param extensions Dock extensions
#' @param grid Dock layout
#'
#' @examples
#' brd <- new_dock_board(c(a = blockr.core::new_dataset_block()))
#' str(dock_layout(brd), max.level = 2)
#'
#' @return The constructor `new_dock_board()` returns a `board` object, as does
#' the coercion function `as_dock_board()`. Inheritance can be checked using
#' `is_dock_board()`, which returns a boolean. Getters `dock_layout()` and
#' `dock_extensions()` return `dock_layout` and `dock_extension` objects while
#' setters `dock_layout<-()` and `dock_extensions<-()` return the updated board
#' object (invisibly). A character vector of IDs is returned by `dock_ext_ids()`
#' and `dock_board_options()` returns a `board_options` object.
#'
#' @rdname dock
#' @export
new_dock_board <- function(blocks = list(), ...,
                           extensions = new_dock_extensions(),
                           grid = default_grid(blocks, extensions),
                           options = dock_board_options(),
                           ctor = NULL, pkg = NULL, class = character()) {

  new_board(
    blocks = as_blocks(blocks),
    ...,
    extensions = as_dock_extensions(extensions),
    layout = create_dock_layout(blocks, extensions, grid),
    options = as_board_options(options),
    ctor = forward_ctor(ctor),
    pkg = pkg,
    class = c(class, "dock_board")
  )
}

#' @export
validate_board.dock_board <- function(x) {

  x <- NextMethod()

  validate_dock_layout(x[["layout"]], board_block_ids(x))
  validate_extensions(x[["extensions"]])

  x
}

#' @param x Board object
#' @rdname dock
#' @export
is_dock_board <- function(x) {
  inherits(x, "dock_board")
}

#' @rdname dock
#' @export
as_dock_board <- function(x, ...) {
  UseMethod("as_dock_board")
}

#' @export
as_dock_board.dock_board <- function(x, ...) {
  x
}

#' @export
as_dock_board.board <- function(x, ...) {
  new_dock_board(
    board_blocks(x),
    board_links(x),
    board_stacks(x),
    ...
  )
}

#' @rdname dock
#' @export
dock_layout <- function(x) {
  stopifnot(is_dock_board(x))
  validate_dock_layout(x[["layout"]], board_block_ids(x))
}

#' @param value Replacement value
#' @rdname dock
#' @export
`dock_layout<-` <- function(x, value) {
  stopifnot(is_dock_board(x))
  x[["layout"]] <- validate_dock_layout(value, board_block_ids(x))
  invisible(x)
}

#' @rdname dock
#' @export
dock_extensions <- function(x) {
  stopifnot(is_dock_board(x))
  validate_extensions(x[["extensions"]])
}

#' @param value Replacement value
#' @rdname dock
#' @export
`dock_extensions<-` <- function(x, value) {
  stopifnot(is_dock_board(x))
  x[["extensions"]] <- validate_extensions(value)
  invisible(x)
}

#' @rdname dock
#' @export
dock_ext_ids <- function(x) {
  chr_ply(dock_extensions(x), extension_id)
}

#' @rdname dock
#' @export
dock_board_options <- function() {
  new_board_options(
    new_board_name_option()
  )
}

#' @export
rm_blocks.dock_board <- function(x, rm, ...) {

  # for now, b/c dock$layout(), passed in ... is not updated in time, we can
  # only clear the layout to not run into validation issues
  dock_layout(x) <- new_dock_layout()

  NextMethod(object = x)
}
