#' Dock extensions
#'
#' Functionality of a `dock_board` can be extended by supplying one or more
#' `dock_extension` objects, which essentially provide UI shown in a dock panel
#' that allows for manipulating the board state.
#'
#' @param server A function returning [shiny::moduleServer()]
#' @param ui A function with a single argument (`ns`) returning a `shiny.tag`
#' @param class Extension subclass
#'
#' @rdname extension
#' @export
new_dock_extension <- function(server, ui, class, ...) {
	structure(
    list(server = server, ui = ui),
    class = c(class, "dock_extension")
  )
}

#' @rdname extension
#' @export
is_dock_extenstion <- function(x) {
  inherits(x, "dock_extension")
}
