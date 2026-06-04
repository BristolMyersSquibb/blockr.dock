add_link_action <- function(trigger, board, update, ...) {

  new_action(
    function(input, output, session) {

      sidebar_id <- NS(isolate(board$board_id), "actions_sidebar")
      # Pass the board + anchor as reactives: the menu validates the
      # committed link id itself and keeps an open panel in sync with the
      # board (a freed-up target reappears, a removed block's card drops),
      # so this handler is a thin adapter - no dock-side validator, no
      # manual pool-update.
      committed <- blockr.ui::link_menu_server(
        "menu",
        board = reactive(board$board),
        anchor = reactive(trigger())
      )

      menu_ui <- function() {
        blockr.ui::link_menu_ui(
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
        blockr.ui::show_sidebar(
          sidebar_id, title = sidebar_title(), ui = menu_ui()
        )
      })

      observeEvent(committed(), {
        spec <- committed()

        links <- board_links(board$board)
        trg_blk <- board_blocks(board$board)[[spec$target]]

        # The menu only renders a port picker for finite-arity targets
        # with > 1 free slot, so `spec$block_input` arrives NULL for
        # arity-1 and variadic targets. Resolve the slot ourselves in
        # those cases: the first free named input, or - for variadic
        # targets - a freshly generated slot, both via the same helper.
        chosen_input <- spec$block_input
        if (is.null(chosen_input) || !nzchar(chosen_input)) {
          chosen_input <- block_input_select(
            trg_blk, spec$target, links, mode = "inputs"
          )[1L]
        }

        new_lnk <- new_link(
          from = spec$source,
          to = spec$target,
          input = chosen_input
        )

        update(
          list(
            links = list(
              add = as_links(set_names(list(new_lnk), spec$link_id))
            )
          )
        )

        # Close after a single link unless the user pinned the sidebar.
        # When pinned, the menu's own board observer drops the just-wired
        # card in place (no re-render) so the user can add another link.
        session$onFlushed(
          function() {
            isolate(
              if (!isTRUE(blockr.ui::sidebar_state(sidebar_id)$pinned)) {
                blockr.ui::hide_sidebar(sidebar_id)
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
