#' Board actions
#'
#' Logic including a modal-based UI for board actions such as "append block"
#' or "edit stack" can be specified using `action` objects, which essentially
#' are classed functions that can either be called to return a shiny module
#' `as_module = TRUE` or a function `as_module = FALSE` which injects code
#' (passed as `expr`) into a shiny server context.
#'
#' An action is a function that can be called with arguments `trigger` and
#' `as_module` to return another function. The action trigger may either be a
#' string (referring to an `input`), a function (that will be called with a
#' single argument `input`) or a [shiny::reactive()] object. The flag
#' `as_module` controls the behavior of the returned function: if `TRUE`, it
#' is a function (inheriting from `action_module`) with arguments `board`,
#' `update`, `...` and `domain`, which, when called, again returns a function
#' with arguments `input`, `output` and `session`, suitable as argument to
#' [shiny::moduleServer()]. If `FALSE` is passed instead, a function
#' (inheriting from `action_function`) with arguments `board`, `update`, `...`
#' and `domain` is returned.
#'
#' The expression `expr`, passed when instantiating an `action` object will be
#' evaluated in a context, where the following bindings exist: `board`,
#' `update`, `domain`, `input`, `output` and `session`. In the case of
#' `as_module = FALSE`, `domain` is an alias for `session`.
#'
#' @param func A function which will be evaluated (with modified formals) in a
#' shiny server context
#' @param id Action ID
#'
#' @return The constructor `new_action` returns a classed function that
#' inherits from `action`. Inheritance can be checked with functions
#' `is_action()`, `is_action_module()` and `is_action_function()`, which all
#' return scalar logicals.
#'
#' @rdname action
#' @export
new_action <- function(func, id) {

  stopifnot(
    identical(names(formals(func)), c("input", "output", "session")),
    is_string(id)
  )

  structure(func, id = id, class = "action")
}

#' @param x Object
#' @rdname action
#' @export
is_action <- function(x) {
  inherits(x, "action")
}

#' @rdname action
#' @export
is_action_generator <- function(x) {
  is.function(x) &&
    all(c("trigger", "...") %in% names(formals(x))) &&
    is_action(x())
}

#' @rdname action
#' @export
action_id <- function(x) {
  UseMethod("action_id")
}

#' @export
action_id.action <- function(x) {
  attr(x, "id")
}

#' @export
action_id.function <- function(x) {
  stopifnot(is_action_generator(x))
  action_id(x())
}

#' @rdname action
#' @export
board_actions <- function(x, ...) {
  UseMethod("board_actions")
}

#' @export
board_actions.dock_extension <- function(x, ...) {
  list()
}

#' @export
board_actions.dock_board <- function(x, ...) {
  list(
    add_block_action,
    append_block_action,
    remove_block_action,
    add_link_action,
    remove_link_action,
    add_stack_action,
    edit_stack_action,
    remove_stack_action
  )
}

action_triggers <- function(x) {

  ids <- chr_ply(x, action_id)

  stopifnot(length(unique(ids)) == length(x))

  lapply(
    set_names(nm = ids),
    reactiveVal
  )
}

register_actions <- function(actions, triggers, board, update, args,
                             session = get_session()) {

  ids <- chr_ply(actions, action_id)

  stopifnot(
    length(unique(ids)) == length(actions),
    setequal(names(triggers), ids)
  )

  map(
    register_action,
    ids,
    actions,
    trigger = triggers[ids],
    MoreArgs = c(
      list(board = board, update = update),
      args,
      list(session = session)
    )
  )

  invisible(NULL)
}

register_action <- function(id, action, ..., session = get_session()) {

  res <- moduleServer(id, action(...), session = session)

  if (!is.null(res)) {
    blockr_abort(
      "Expecting an action server to return `NULL`",
      class = "action_server_invalid_return"
    )
  }

  invisible()
}
