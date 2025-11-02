#' Dock extensions
#'
#' Functionality of a `dock_board` can be extended by supplying one or more
#' `dock_extension` objects, which essentially provide UI shown in a dock panel
#' that allows for manipulating the board state.
#'
#' @param server A function returning [shiny::moduleServer()]
#' @param ui A function with a single argument (`ns`) returning a `shiny.tag`
#' @param name Name for extension
#' @param class Extension subclass
#' @param ctor Constructor function name
#' @param pkg Package to look up `ctor`
#' @param options Board options supplied by an extension
#' @param ... Further attributes
#'
#' @rdname extension
#' @export
new_dock_extension <- function(server, ui, name, class, ctor = sys.parent(),
                               pkg = NULL, options = new_board_options(), ...) {
  validate_extension(
    structure(
      list(
        server = server,
        ui = ui,
        options = as_board_options(options),
        ...
      ),
      name = name,
      ctor = resolve_ctor(ctor, pkg),
      class = c(class, "dock_extension")
    )
  )
}

#' @param x Extension object
#' @rdname extension
#' @export
is_dock_extension <- function(x) {
  inherits(x, "dock_extension")
}

#' @rdname extension
#' @export
validate_extension <- function(x, ...) {
  UseMethod("validate_extension")
}

#' @export
validate_extension.dock_extension <- function(x, ...) {

  if (!is_dock_extension(x) || !length(class(x)) == 2L) {
    blockr_abort(
      "Expecting extensions to inherit from `dock_extension` and one ",
      "additional class.",
      class = "dock_extension_invalid"
    )
  }

  id <- extension_id(x)

  if (!is_string(id) || id == "dock_extension" || !grepl("_extension$", id)) {
    blockr_abort(
      "Malformed extension subclass.",
      class = "dock_extension_invalid"
    )
  }

  nme <- extension_name(x)

  if (!is_string(nme)) {
    blockr_abort(
      "Expecting a string as extension name",
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

  validate_board_options(extension_options(x))

  x
}

#' @param id Namespace ID
#' @rdname extension
#' @export
extension_ui <- function(x, id, ...) {

  stopifnot(is_dock_extension(x))

  fun <- x[["ui"]]

  if (missing(id) && !...length()) {
    return(fun)
  }

  div(
    id = NS(id, as_ext_handle_id(x)),
    fun(NS(id, extension_id(x)), ...)
  )
}

#' @rdname extension
#' @export
extension_server <- function(x, ...) {

  stopifnot(is_dock_extension(x))

  fun <- x[["server"]]

  if (!...length()) {
    return(fun)
  }

  do.call(fun, c(list(id = extension_id(x)), ...))
}

#' @rdname extension
#' @export
extension_id <- function(x) {
  stopifnot(is_dock_extension(x))
  class(x)[1L]
}

#' @rdname extension
#' @export
extension_name <- function(x) {
  stopifnot(is_dock_extension(x))
  attr(x, "name")
}

#' @rdname extension
#' @export
extension_ctor <- function(x) {
  stopifnot(is_dock_extension(x))
  attr(x, "ctor")
}

#' @rdname extension
#' @export
extension_options <- function(x) {
  stopifnot(is_dock_extension(x))
  x[["options"]]
}
