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
#'
#' @return The constructor `new_action` returns a classed function that
#' inherits from `action`. Inheritance can be checked with functions
#' `is_action()`, `is_action_module()` and `is_action_function()`, which all
#' return scalar logicals.
#'
#' @rdname action
#' @export
new_action <- function(func) {

  stopifnot(
    identical(names(formals(func)), c("input", "output", "session"))
  )

  structure(func, class = "action")
}

#' @param x Object
#' @rdname action
#' @export
is_action <- function(x) {
  inherits(x, "action")
}

#' @rdname action
#' @export
board_action_triggers <- function(x, ...) {
  UseMethod("board_action_triggers")
}

#' @export
board_action_triggers.dock_extension <- function(x, ...) {
  list()
}

#' @export
board_action_triggers.dock_board <- function(x, ...) {
  list(
    add_block_action = reactiveVal(),
    append_block_action = reactiveVal(),
    remove_block_action = reactiveVal(),
    add_link_action = reactiveVal(),
    remove_link_action = reactiveVal(),
    add_stack_action = reactiveVal(),
    edit_stack_action = reactiveVal(),
    remove_stack_action = reactiveVal()
  )
}

dock_actions <- function() {
  list(
    add_block_action = add_block_action,
    append_block_action = append_block_action,
    remove_block_action = remove_block_action,
    add_link_action = add_link_action,
    remove_link_action = remove_link_action,
    add_stack_action = add_stack_action,
    edit_stack_action = edit_stack_action,
    remove_stack_action = remove_stack_action
  )
}

register_actions <- function(actions, triggers, board, update,
                             session = get_session()) {

  if (!setequal(names(triggers), names(actions))) {
    blockr_abort(
      "Expecting matching sets of actions and actions triggers.",
      class = "action_trigger_mismatch"
    )
  }

  map(
    register_action,
    names(actions),
    actions,
    triggers[names(actions)],
    MoreArgs = list(board = board, update = update, session = session)
  )

  invisible(NULL)
}

register_action <- function(id, action, trigger, ..., session = get_session()) {

  res <- moduleServer(id, action(trigger, ...), session = session)

  if (!is.null(res)) {
    blockr_abort(
      "Expecting an action server to return `NULL`",
      class = "action_server_invalid_return"
    )
  }

  invisible()
}
