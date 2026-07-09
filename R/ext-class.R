#' Dock extensions
#'
#' Functionality of a `dock_board` can be extended by supplying one or more
#' `dock_extension` objects, which essentially provide UI shown in a dock panel
#' that allows for manipulating the board state. A set of dock extensions
#' can be combined into a `dock_extensions` object.
#'
#' @param server A function returning [shiny::moduleServer()]. Beyond
#' `id`, it is called with the board handles `board`, `update`,
#' `view_data` and `actions`, plus `extensions` -- an environment
#' exposing every extension's server result keyed by ID (each carrying
#' its `state`), so one extension can read another's state via
#' `extensions[[id]]`. `view_data` is a reactive holding the live
#' all-views layout (the committed board is `board`); it is `NULL`
#' until every view has reported its layout once, so `req()` it.
#' @param ui A function with a single argument (`ns`) returning a `shiny.tag`
#' @param name Name for extension
#' @param class Extension subclass
#' @param description Optional free-text description of the extension, surfaced
#' as consumer-neutral metadata (e.g. to the AI assistant)
#' @param ctor Constructor function name
#' @param pkg Package to look up `ctor`
#' @param options Board options supplied by an extension
#' @param external_ctrl Set up external control (experimental). `FALSE`
#' (the default) opts out; `TRUE` exposes every constructor input as
#' externally controllable; a character vector names a subset of them.
#' @param ... Further attributes
#'
#' @examples
#' ext <- new_edit_board_extension()
#' is_dock_extension(ext)
#'
#' @return The constructors `new_dock_extension()` and `new_dock_extension()`,
#' as do the coercion function `as_dock_extension()` and `as_dock_extension()`,
#' return objects that inherit from `dock_extension` and `dock_extensions`
#' respectively. This inheritance structure can be checked using
#' `is_dock_extension()` and `is_dock_extensions()`, which both return a
#' boolean. A `dock_extension` can be validated using `validate_extension()`
#' and a `dock_extensions` object using `validate_extensions()`, which return
#' the input object invisibly and throw errors as side-effects. Several getter
#' functions return extension attributes, including `extension_ui()` (a
#' function), `extension_server()` (a function), `extension_id()` (a string),
#' `extension_name()` (a string), `extension_description()` (a string or
#' `NULL`) and `extension_ctor()` (an object that inherits from `blockr_ctor`).
#'
#' @rdname extension
#' @export
new_dock_extension <- function(server, ui, name, class,
                               description = NULL, ctor = sys.parent(),
                               pkg = NULL, options = new_board_options(),
                               external_ctrl = FALSE, ...) {

  stopifnot(is_bool(external_ctrl) || is.character(external_ctrl))

  validate_extension(
    structure(
      list(
        server = server,
        ui = ui,
        options = as_board_options(options),
        ...
      ),
      name = name,
      description = description,
      ctor = resolve_ctor(ctor, pkg),
      external_ctrl = external_ctrl,
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
      class = "dock_extension_inheritance_invalid"
    )
  }

  if (!is.list(x) || !all(c("ui", "server") %in% names(x))) {
    blockr_abort(
      "Expecting list with components `ui` and `server`.",
      class = "dock_extension_structure_invalid"
    )
  }

  id <- extension_id(x)

  if (!is_string(id) || id == "dock_extension" || !grepl("_extension$", id)) {
    blockr_abort(
      "Malformed extension subclass.",
      class = "dock_extension_id_invalid"
    )
  }

  nme <- extension_name(x)

  if (!is_string(nme)) {
    blockr_abort(
      "Expecting a string as extension name.",
      class = "dock_extension_name_invalid"
    )
  }

  desc <- extension_description(x)

  if (!is.null(desc) && !is_string(desc)) {
    blockr_abort(
      "Expecting extension description to be a string or `NULL`.",
      class = "dock_extension_description_invalid"
    )
  }

  ui <- extension_ui(x)

  if (!is.function(ui)) {
    blockr_abort(
      "Expecting a block extension UI to be specified by a function.",
      class = "dock_extension_ui_invalid"
    )
  }

  srv <- extension_server(x)

  if (!is.function(srv)) {
    blockr_abort(
      "Expecting a block extension server to be a function.",
      class = "dock_extension_server_invalid"
    )
  }

  validate_board_options(board_options(x))

  x
}

#' @param id Namespace ID
#' @param key Container-assigned extension id (list key)
#' @rdname extension
#' @export
extension_ui <- function(x, key, id, ...) {

  stopifnot(is_dock_extension(x))

  fun <- x[["ui"]]

  if (missing(id) && !...length()) {
    return(fun)
  }

  div(
    id = NS(id, as_ext_handle_id(key)),
    fun(NS(id, paste0("ext_", key)), ...)
  )
}

#' @rdname extension
#' @export
extension_server <- function(x, key, ...) {

  stopifnot(is_dock_extension(x))

  fun <- x[["server"]]

  if (!...length()) {
    return(fun)
  }

  validate_ext_srv_result(
    coal(
      do.call(fun, c(list(id = paste0("ext_", key)), ...)),
      list(state = list())
    ),
    x
  )
}

validate_ext_srv_result <- function(res, x) {

  if (!is.list(res)) {
    blockr_abort(
      "Expecting an extension server to return a list.",
      class = "extension_server_return_invalid"
    )
  }

  if (!"state" %in% names(res)) {
    blockr_abort(
      "Expecting an extension server to return a component `state`.",
      class = "extension_server_return_missing_state"
    )
  }

  if (!is.list(res[["state"]])) {
    blockr_abort(
      "Expecting an extension server to return list-valued `state`.",
      class = "extension_server_return_state_invalid"
    )
  }

  if (length(res[["state"]]) && is.null(names(res[["state"]]))) {
    blockr_abort(
      "Expecting an extension server to return named `state`.",
      class = "extension_server_return_state_invalid"
    )
  }

  vars <- external_ctrl_vars(x)

  miss <- setdiff(vars, names(res[["state"]]))

  if (length(miss)) {
    blockr_abort(
      "Externally controllable variable{?s} {miss} absent from extension ",
      "`state`.",
      class = "extension_ctrl_var_absent"
    )
  }

  not_rv <- !lgl_ply(res[["state"]][vars], inherits, "reactiveVal")

  if (any(not_rv)) {
    blockr_abort(
      "Externally controllable extension state must be `reactiveVal`.",
      class = "extension_ctrl_var_not_rv"
    )
  }

  res
}

#' @rdname extension
#' @export
extension_id <- function(x) {
  stopifnot(is_dock_extension(x))
  class(x)[1L]
}

# The type-derived default key: the container names an unnamed extension by its
# class with the `_extension` suffix stripped (`dag_extension` -> `dag`). An
# explicit list name in `new_dock_board(extensions = )` overrides it. This is
# the addressing identity mirror of a block's list key -- owned by the
# container, not the object.
extension_key <- function(x) {
  sub("_extension$", "", extension_id(x))
}

#' @rdname extension
#' @export
extension_name <- function(x) {
  stopifnot(is_dock_extension(x))
  attr(x, "name")
}

#' @rdname extension
#' @export
extension_description <- function(x) {
  stopifnot(is_dock_extension(x))
  attr(x, "description")
}

#' @rdname extension
#' @export
extension_ctor <- function(x) {
  stopifnot(is_dock_extension(x))
  attr(x, "ctor")
}

ext_ctor_inputs <- function(x) {
  setdiff(names(formals(extension_ctor(x))), "...")
}

#' @export
external_ctrl_vars.dock_extension <- function(x) {

  res <- attr(x, "external_ctrl")

  if (isTRUE(res)) {
    ext_ctor_inputs(x)
  } else if (isFALSE(res) || is.null(res)) {
    character()
  } else {
    stopifnot(is.character(res), all(res %in% ext_ctor_inputs(x)))
    res
  }
}

#' @export
board_options.dock_extension <- function(x, ...) {
  x[["options"]]
}

#' @export
str_value.dock_extension <- function(x, ...) {

  out <- paste0("<", class(x)[1L], ">")

  args <- ext_ctor_inputs(x)

  if (length(args)) {
    ctrl <- external_ctrl_vars(x)
    args <- ifelse(args %in% ctrl, paste0(args, "*"), args)
    out <- paste0(out, " ", paste0(args, collapse = ", "))
  }

  out
}

#' @importFrom utils str
#' @export
str.dock_extension <- function(object, ...) {
  cat(" ", str_value(object), "\n", sep = "")
  invisible(object)
}

#' @rdname extension
#' @export
new_dock_extensions <- function(x = list()) {
  validate_extensions(
    structure(stamp_extension_keys(x), class = "dock_extensions")
  )
}

stamp_extension_keys <- function(x) {

  if (!length(x)) {
    return(x)
  }

  keys <- names(x) %||% rep("", length(x))

  set_names(x, ifelse(nzchar(keys), keys, chr_ply(x, extension_key)))
}

#' @rdname extension
#' @export
is_dock_extensions <- function(x) {
  inherits(x, "dock_extensions")
}

#' @rdname extension
#' @export
validate_extensions <- function(x) {

  if (!is_dock_extensions(x) || !is.list(x)) {
    blockr_abort(
      "Expecting extensions to inherit from `dock_extensions` and be ",
      "represented by a list.",
      class = "dock_extensions_structure_invalid"
    )
  }

  dups <- unique(names(x)[duplicated(names(x))])

  if (length(dups)) {
    blockr_abort(
      paste0(
        "Extension ids must be unique; duplicated: ",
        paste(dups, collapse = ", "),
        ". Name the extensions in the `extensions` list to disambiguate."
      ),
      class = "dock_extensions_names_invalid"
    )
  }

  for (ext in x) {
    validate_extension(ext)
  }

  x
}

#' @rdname extension
#' @export
as_dock_extensions <- function(x, ...) {
  UseMethod("as_dock_extensions")
}

#' @rdname extension
#' @export
as_dock_extensions.dock_extensions <- function(x, ...) {
  x
}

#' @rdname extension
#' @export
as_dock_extensions.dock_extension <- function(x, ...) {
  new_dock_extensions(list(x))
}

#' @rdname extension
#' @export
as_dock_extensions.list <- function(x, ...) {
  new_dock_extensions(x)
}

#' @export
`[.dock_extensions` <- function(x, i, ...) {
  new_dock_extensions(NextMethod())
}

#' @export
as.list.dock_extensions <- function(x, ...) {
  set_names(unclass(x), names(x))
}

#' @export
str_value.dock_extensions <- function(x, ...) {

  items <- if (length(x)) {
    paste0("  ", names(x), ": ", chr_ply(x, str_value))
  }

  paste(
    c(paste0("<dock_extensions[", length(x), "]>"), items),
    collapse = "\n"
  )
}

#' @importFrom utils str
#' @export
str.dock_extensions <- function(object, ...) {
  cat(str_value(object), "\n", sep = "")
  invisible(object)
}

#' @rdname extension
#' @export
extension_block_callback <- function(x, ...) {
  UseMethod("extension_block_callback", x)
}

#' @export
extension_block_callback.dock_extension <- function(x, ...) {
  NULL
}
