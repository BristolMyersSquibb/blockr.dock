new_function <- function(formals = NULL, body = NULL, env = parent.frame()) {

  res <- function() {}

  if (!is.null(formals)) {
    formals(res) <- formals
  }

  if (!is.null(body)) {
    body(res) <- body
  }

  environment(res) <- env

  res
}

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
#' `update` and `proxy`, which, when called, again returns a function with
#' arguments `input`, `output` and `session`, suitable as argument to
#' [shiny::moduleServer()]. If `FALSE` is passed instead, a function
#' (inheriting from `action_function`) with arguments `board`, `update` and
#' `proxy` is returned.
#'
#' The expression `expr`, passed when instantiating an `action` object will be
#' evaluated in a context, where the following bindings exist: `board`,
#' `update`, `proxy`, `input`, `output` and `session`.
#'
#' @param expr An expression which will be evaluated in a shiny server context
#'
#' @return The constructor `new_action` returns a classed function that
#' inherits from `action`. Inheritance can be checked with functions
#' `is_action()`, `is_action_module()` and `is_action_function()`, which all
#' return scalar logicals.
#'
#' @rdname action
#' @export
new_action <- function(expr) {

  proc_calls <- function(x) {
    if (is.call(x) && identical(x[[1L]], as.symbol("{"))) {
      as.list(x)[-1L]
    } else {
      list(x)
    }
  }

  combine_exprs <- function(x) {
    do.call(
      as.call,
      list(c(as.name("{"), unlst(lapply(x, proc_calls)))),
      quote = TRUE
    )
  }

  body <- list( # nolint: object_usage_linter.
    quote(
      {
        if (!is.reactive(trigger)) {
          if (is_string(trigger)) {
            trigr_q <- bquote(req(input[[.(trg)]]), list(trg = trigger))
          } else if (is.function(trigger)) {
            trigr_q <- bquote(.(fun)(input), list(fun = trigger))
          } else {
            blockr_abort(
              "An action trigger should be a string, function or reactive ",
              "object",
              class = "invalid_action_trigger"
            )
          }
          trigger <- reactive(trigr_q, quoted = TRUE)
        }
        stopifnot(is.reactive(trigger))
      }
    ),
    substitute(expr),
    quote(
      {
        invisible(NULL)
      }
    )
  )

  structure(
    function(trigger, as_module = TRUE) {

      if (isTRUE(as_module)) {

        structure(
          function(board, update, proxy) {
            new_function(
              alist(input = , output = , session = ),
              combine_exprs(body)
            )
          },
          class = "action_module"
        )

      } else {

        body <- c(
          quote(
            {
              session <- proxy$session
              input <- session$input
              output <- session$output
            }
          ),
          body
        )

        structure(
          new_function(
            alist(board = , update = , proxy = ),
            combine_exprs(body)
          ),
          class = "action_function"
        )
      }
    },
    class = "action"
  )
}

#' @param x Object
#' @rdname action
#' @export
is_action <- function(x) {
  inherits(x, "action")
}

#' @rdname action
#' @export
is_action_module <- function(x) {
  inherits(x, "action_module")
}

#' @rdname action
#' @export
is_action_function <- function(x) {
  inherits(x, "action_function")
}
