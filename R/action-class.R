#' Board actions
#'
#' Logic including a modal-based UI for board actions such as "append block"
#' or "edit stack" can be specified using `action` objects, which essentially
#' are classed shiny server functions.
#'
#' An action is a function that can be called with arguments `input`,
#' `output` and `session`, behaving as one would expect from a shiny server
#' module function. Actions are typically created by action generator
#' functions, they each have a unique ID and a [shiny::reactiveVal()]-based
#' trigger object (inheriting from `action_trigger`). Action trigger objects
#' implement their own counter-based invalidation mechanism (on top of how
#' reactive values behave).
#'
#' @param func A function which will be used to create a
#' [shiny::moduleServer()].
#' @param id Action ID
#'
#' @return The constructor `new_action` returns a classed function that
#' inherits from `action`. Inheritance can be checked with functions
#' `is_action()`, `is_action_generator()` checks whether an objects is a
#' function that returns an `action` object. String-value action IDs can be
#' retrieved with `action_id()` and the set of actions associated with a board
#' can be enumerated via `board_actions()`. Finally, `action_triggers()` returns
#' a named list of objects suitable for use as action triggers.
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
    prepend_block_action,
    remove_block_action,
    add_link_action,
    remove_link_action,
    add_stack_action,
    edit_stack_action,
    remove_stack_action
  )
}

#' @rdname action
#' @export
action_triggers <- function(x) {
  ids <- chr_ply(x, action_id)

  stopifnot(length(unique(ids)) == length(x))

  set_names(
    replicate(length(ids), new_trigger()),
    ids
  )
}

register_actions <- function(
  actions,
  triggers,
  board,
  update,
  args,
  session = get_session()
) {
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

new_trigger <- function(value = NULL) {
  rv <- reactiveVal(list(value = value, counter = 0L))

  structure(
    function(new) {
      cur <- rv()

      if (missing(new)) {
        return(cur[["value"]])
      }

      rv(list(value = new, counter = cur[["counter"]] + 1L))

      invisible(new)
    },
    class = c("action_trigger", "function")
  )
}
