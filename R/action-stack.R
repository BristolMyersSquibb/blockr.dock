add_stack_action <- function(trigger, board, update, ...) {

  new_action(
    function(input, output, session) {

      observeEvent(
        trigger(),
        ignoreInit = TRUE,
        show_sidebar("create_stack")
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
          set_sidebar_context(stack_id = stack_id)
          show_sidebar("edit_stack")
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
