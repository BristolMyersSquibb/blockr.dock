add_stack_action <- function(trigger, board, update, ...) {

  new_action(
    function(input, output, session) {

      observeEvent(
        trigger(),
        ignoreInit = TRUE,
        show_sidebar(new_sidebar("create_stack", list(mode = "create")))
      )

      # Note: stack_confirm handler is in sidebar_server (board namespace)

      NULL
    },
    id = "add_stack_action"
  )
}

edit_stack_action <- function(trigger, board, update, ...) {

  new_action(
    function(input, output, session) {

      observeEvent(
        trigger(),
        ignoreInit = TRUE,
        {
          stack_id <- trigger()
          req(stack_id)
          ctx <- list(mode = "edit", stack_id = stack_id)
          show_sidebar(new_sidebar("edit_stack", ctx))
        }
      )

      # Note: edit_stack_confirm handler is in sidebar_server (board namespace)

      NULL
    },
    id = "edit_stack_action"
  )
}

remove_stack_action <- function(trigger, board, update, ...) {
  new_action(
    function(input, output, session) {
      observeEvent(
        trigger(),
        ignoreInit = TRUE,
        {
          stack_id <- trigger()
          req(stack_id)
          update(list(stacks = list(rm = stack_id)))
        }
      )
      NULL
    },
    id = "remove_stack_action"
  )
}
