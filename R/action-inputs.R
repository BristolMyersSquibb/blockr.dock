# Edit inputs had a sidebar form; the link's menu and Connect cover what it
# did (#544). The action stays registered so an older blockr.dag, whose
# right-click menu still offers it, says where to go instead of failing.
edit_inputs_action <- function(trigger, board, update, ...) {
  new_action(
    function(input, output, session) {
      observeEvent(trigger(), {
        notify(
          paste(
            "Edit inputs is gone: right-click a link to change it,",
            "or use Connect to on the block."
          ),
          type = "message", session = session
        )
      })
      NULL
    },
    id = "edit_inputs_action"
  )
}
