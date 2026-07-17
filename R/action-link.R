add_link_action <- function(trigger, board, update, ...) {

  new_action(
    function(input, output, session) {

      sidebar_id <- NS(isolate(board$board_id), "actions_sidebar")
      # Pass the board + anchor as reactives: the menu validates the
      # committed link id itself and keeps an open panel in sync with the
      # board (a freed-up target reappears, a removed block's card drops),
      # so this handler is a thin adapter - no dock-side validator, no
      # manual pool-update.
      committed <- link_menu_server(
        "menu",
        board = reactive(board$board),
        anchor = reactive(trigger())
      )

      menu_ui <- function() {
        link_menu_ui(
          session$ns("menu"), board$board, anchor = trigger()
        )
      }

      # The sidebar title carries the anchor so users always see which
      # block they're connecting; the wording reads correctly whichever
      # direction the card lives in (OUTGOING / INCOMING).
      sidebar_title <- function() paste0("Connect ", trigger())

      observeEvent(trigger(), {
        # The link menu owns its own empty-state, so there's no pre-flight
        # NULL-check / notification here anymore: a block that can't be
        # linked still opens the sidebar with an in-place empty message.
        show_sidebar(
          sidebar_id, title = sidebar_title(), ui = menu_ui()
        )
      })

      observeEvent(committed(), {
        # The menu returns a ready-to-apply `links` object (id-keyed, with
        # the target port already resolved), so just add it.
        update(list(links = list(add = committed())))

        # Close after a single link unless the user pinned the sidebar.
        # When pinned, the menu's own board observer drops the just-wired
        # card in place (no re-render) so the user can add another link.
        session$onFlushed(
          function() {
            isolate(
              if (!isTRUE(sidebar_state(sidebar_id)$pinned)) {
                hide_sidebar(sidebar_id)
              }
            )
          },
          once = TRUE
        )
      })

      NULL
    },
    id = "add_link_action"
  )
}

edit_link_action <- function(trigger, board, update, ...) {
  new_action(
    function(input, output, session) {

      sidebar_id <- NS(isolate(board$board_id), "actions_sidebar")

      # The menu validates its own commit (block eligibility, acyclicity,
      # input-name uniqueness) and returns only the fields that changed, so
      # this handler is a thin adapter over a `links$mod` update.
      committed <- edit_link_menu_server(
        "menu",
        board = reactive(board$board),
        link_id = reactive(trigger())
      )

      menu_ui <- function() {
        edit_link_menu_ui(
          session$ns("menu"), board$board, link_id = trigger()
        )
      }

      sidebar_title <- function() paste0("Edit link ", trigger())

      observeEvent(trigger(), {
        show_sidebar(
          sidebar_id, title = sidebar_title(), ui = menu_ui()
        )
      })

      # Close the sidebar the moment the edited link leaves the board
      # (removed elsewhere): editing a link that no longer exists makes no
      # sense, so don't wait for an "Update" click. Guarded on the sidebar
      # being open and on an edit actually being in progress.
      observeEvent(board$board, {
        id <- trigger()
        if (length(id) == 1L && !is.na(id) && nzchar(id) &&
              !id %in% board_link_ids(board$board) &&
              isTRUE(sidebar_state(sidebar_id)$open)) {
          hide_sidebar(sidebar_id)
        }
      }, ignoreInit = TRUE)

      observeEvent(committed(), {
        id <- trigger()

        # Safety net for a race (board change not yet observed when the user
        # clicks): committing for a link that's gone would error, so bail and
        # close unless `id` is still a present link.
        if (!(length(id) == 1L && !is.na(id) && nzchar(id) &&
                id %in% board_link_ids(board$board))) {
          hide_sidebar(sidebar_id)
          return()
        }

        # The menu returns the edited link keyed by its (unchanged) id, or
        # NULL when nothing changed. Commit it as a remove + re-add of that
        # id in one update, which keeps the id and re-applies every field.
        link <- committed()$link

        if (!is.null(link)) {
          update(list(links = list(rm = id, add = link)))
        }

        session$onFlushed(
          function() {
            isolate(
              keep_or_hide_sidebar(
                sidebar_id, title = sidebar_title(), ui = menu_ui()
              )
            )
          },
          once = TRUE
        )
      })

      NULL
    },
    id = "edit_link_action"
  )
}

remove_link_action <- function(trigger, board, update, ...) {
  new_action(
    function(input, output, session) {
      observeEvent(
        trigger(),
        update(list(links = list(rm = trigger())))
      )
      NULL
    },
    id = "remove_link_action"
  )
}
