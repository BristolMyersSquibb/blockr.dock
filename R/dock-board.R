#' Dock board
#'
#' Using the docking layout manager provided by dockViewR, a `dock_board`
#' extends [blockr.core::new_board()].
#'
#' @inheritParams blockr.core::new_board
#'
#' @return A `board` object
#'
#' @export
new_dock_board <- function(..., class = character()) {
	new_board(..., class = c(class, "dock_board"))
}

#' @param x Board object
#' @rdname new_dock_board
#' @export
is_dock_board <- function(x) {
  inherits(x, "dock_board")
}
