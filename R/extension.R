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
	validate_extension(
    structure(
      list(server = server, ui = ui),
      class = c(class, "dock_extension")
    )
  )
}

#' @param x Extension object
#' @rdname extension
#' @export
is_dock_extenstion <- function(x) {
  inherits(x, "dock_extension")
}

#' @rdname extension
#' @export
validate_extension <- function(x, ...) {
  UseMethod("validate_extension")
}

#' @export
validate_extension.dock_extension <- function(x, ...) {

  if (!is_dock_extenstion(x) || !length(class(x)) == 2L) {
    blockr_abort(
      "Expecting extensions to inherit from `dock_extension` and one ",
      "additional class.",
      class = "dock_extension_invalid"
    )
  }

  id <- extension_id(x)

  if (!is_string(id) || id == "dock_extension" || !grepl("_extension$", x)) {
    blockr_abort(
      "Malformed extension subclass.",
      class = "dock_extension_invalid"
    )
  }

  ui <- extension_ui(x)

  if (!is.function(ui)) {
    blockr_abort(
      "Expecting a block extension UI to be specified by a function.",
      class = "dock_extension_invalid"
    )
  }

  srv <- extension_ui(x)

  if (!is.function(srv)) {
    blockr_abort(
      "Expecting a block extension server to be a function.",
      class = "dock_extension_invalid"
    )
  }

  x
}

#' @rdname extension
#' @export
extension_ui <- function(x) {
  stopifnot(is_dock_extenstion(x))
  x[["ui"]]
}

#' @rdname extension
#' @export
extension_server <- function(x) {
  stopifnot(is_dock_extenstion(x))
  x[["server"]]
}

#' @rdname extension
#' @export
extension_id <- function(x) {
  stopifnot(is_dock_extenstion(x))
  class(x)[1L]
}
