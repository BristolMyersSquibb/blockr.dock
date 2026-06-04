add_stack_action <- function(trigger, board, update, ...) {
  new_action(
    function(input, output, session) {
      sidebar_id <- NS(isolate(board$board_id), "actions_sidebar")
      # Pass the board as a reactive: the menu validates the committed
      # spec (id / name / colour) itself and keeps an open panel in sync
      # with the board (cards added / removed live), so this handler is a
      # thin adapter - no dock-side validators, no manual refresh.
      committed <- blockr.ui::stack_menu_server(
        "menu", board = reactive(board$board)
      )

      menu_ui <- function() {
        blockr.ui::stack_menu_ui(session$ns("menu"), board$board)
      }

      observeEvent(trigger(), {
        blockr.ui::show_sidebar(
          sidebar_id, title = "Create new stack", ui = menu_ui()
        )
      })

      observeEvent(committed(), {
        spec <- committed()

        new_stk <- new_dock_stack(
          blocks = as.character(spec$blocks %||% character()),
          name = spec$name,
          color = spec$color
        )

        update(list(
          stacks = list(add = as_stacks(set_names(list(new_stk), spec$id)))
        ))

        # `update()` only sets a reactiveVal; the board state is mutated
        # on the next reactive flush, so defer the sidebar rebuild until
        # after the flush. Otherwise `menu_ui()` reads `board$board` from
        # the pre-add snapshot and re-suggests the just-used stack id.
        # `onFlushed` runs outside a reactive context, hence `isolate()`.
        session$onFlushed(
          function() {
            isolate(
              blockr.ui::keep_or_hide_sidebar(
                sidebar_id, title = "Create new stack", ui = menu_ui()
              )
            )
          },
          once = TRUE
        )
      })

      NULL
    },
    id = "add_stack_action"
  )
}

edit_stack_action <- function(trigger, board, update, ...) {
  new_action(
    function(input, output, session) {
      sidebar_id <- NS(isolate(board$board_id), "actions_sidebar")
      committed <- blockr.ui::stack_menu_server(
        "menu",
        board = reactive(board$board),
        target = reactive(trigger())
      )

      menu_ui <- function() {
        blockr.ui::stack_menu_ui(
          session$ns("menu"), board$board, target = trigger()
        )
      }

      # The sidebar title carries the stack identifier so users always
      # see which stack they're editing (the menu body no longer prints
      # a subtitle of its own).
      sidebar_title <- function() paste0("Edit stack ", trigger())

      observeEvent(trigger(), {
        blockr.ui::show_sidebar(
          sidebar_id, title = sidebar_title(), ui = menu_ui()
        )
      })

      observeEvent(committed(), {
        spec <- committed()
        id <- trigger()

        # `mod` entries are now partial-arg deltas applied via
        # `update_stack()` on the blockr.core side. Reserved key
        # `blocks` replaces the member block IDs; every other key
        # updates the named attribute. Passing a full `stacks` object
        # here trips `board_update_stacks_mod_entry_invalid`.
        update(list(
          stacks = list(
            mod = set_names(
              list(list(
                blocks = as.character(spec$blocks %||% character()),
                name = spec$name,
                color = spec$color
              )),
              id
            )
          )
        ))

        # Defer the sidebar rebuild to the next reactive flush so
        # `menu_ui()` reads the merged board state. Without this, the
        # rebuilt form shows the pre-edit selection (e.g. a block the
        # user just removed still appears selected).
        session$onFlushed(
          function() {
            isolate(
              blockr.ui::keep_or_hide_sidebar(
                sidebar_id, title = sidebar_title(), ui = menu_ui()
              )
            )
          },
          once = TRUE
        )
      })

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
        update(list(stacks = list(rm = trigger())))
      )
      NULL
    },
    id = "remove_stack_action"
  )
}
