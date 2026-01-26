add_link_action <- function(trigger, board, update, ...) {

  new_action(
    function(input, output, session) {

      observeEvent(
        trigger(),
        ignoreInit = TRUE,
        {
          source_block <- trigger()
          req(source_block)
          sidebar <- new_sidebar("add_link", list(source_block = source_block))
          show_sidebar(sidebar)
        }
      )

      # Note: link_card_click and link_card_confirm handlers are in
      # sidebar_server (board namespace)

      NULL
    },
    id = "add_link_action"
  )
}

remove_link_action <- function(trigger, board, update, ...) {
  new_action(
    function(input, output, session) {
      observeEvent(
        trigger(),
        ignoreInit = TRUE,
        {
          link_id <- trigger()
          req(link_id)
          update(list(links = list(rm = link_id)))
        }
      )
      NULL
    },
    id = "remove_link_action"
  )
}
