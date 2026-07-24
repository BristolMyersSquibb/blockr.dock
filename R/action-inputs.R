edit_inputs_action <- function(trigger, board, update, ...) {
  new_action(
    function(input, output, session) {

      sidebar_id <- NS(isolate(board$board_id), "actions_sidebar")

      # The menu validates each commit and returns a ready-to-apply core
      # update. The row list is a `uiOutput` bound to the board, so applying an
      # update re-renders it in place - the panel stays open across edits, and
      # an "Add input" pick simply shows up as a new row.
      committed <- edit_inputs_menu_server(
        "menu",
        board = reactive(board$board),
        block_id = reactive(trigger())
      )

      menu_ui <- function() {
        edit_inputs_menu_ui(session$ns("menu"), board$board, trigger())
      }

      sidebar_title <- function() paste0("Edit inputs ", trigger())

      observeEvent(trigger(), {
        show_sidebar(sidebar_id, title = sidebar_title(), ui = menu_ui())
      })

      # Close the sidebar the moment the block leaves the board (removed
      # elsewhere). Guarded on an edit being in progress and the sidebar open.
      observeEvent(board$board, {
        id <- trigger()
        if (length(id) == 1L && !is.na(id) && nzchar(id) &&
              !id %in% board_block_ids(board$board) &&
              isTRUE(sidebar_state(sidebar_id)$open)) {
          hide_sidebar(sidebar_id)
        }
      }, ignoreInit = TRUE)

      observeEvent(committed(), {
        upd <- committed()$update
        if (length(upd)) {
          update(upd)
        }
      })

      NULL
    },
    id = "edit_inputs_action"
  )
}
