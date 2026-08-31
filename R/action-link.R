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
        hide_unless_pinned(sidebar_id)
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
      # sense, so don't wait for an "Update" click. Guarded on an edit
      # actually being in progress and on this action still owning the
      # open panel.
      observeEvent(board$board, {
        id <- trigger()
        if (length(id) == 1L && !is.na(id) && nzchar(id) &&
              !id %in% board_link_ids(board$board) &&
              owns_open_sidebar(sidebar_id)) {
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

        # The menu returns just the changed fields (or an empty delta when
        # nothing changed). Apply them as a `links$mod` on the unchanged id;
        # core merges the delta onto the current link via `update_link()`.
        delta <- committed()$delta

        if (length(delta)) {
          update(list(links = list(mod = set_names(list(delta), id))))
        }

        # The endpoint pickers and the input-slot control are `uiOutput`s on
        # the board, so a pinned panel re-renders itself against the merged
        # state - nothing here has to rebuild it.
        hide_unless_pinned(sidebar_id)
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

# Triggered with a link id like the other actions in this file, though what
# it commits is a block: an insert is scoped to the wire it splits, and the
# wire is what the user gestures at.
insert_block_action <- function(trigger, board, update, ...) {
  new_action(
    function(input, output, session) {

      sidebar_id <- NS(isolate(board$board_id), "actions_sidebar")

      # The catalogue is registry-based (any block that can receive from the
      # link's source), but the panel's context names the wire's two ends, so
      # the body is rendered per open rather than pre-rendered once.
      added <- block_browser_server(
        "browser",
        board = reactive(board$board),
        target = reactive(insert_into(trigger()))
      )

      browser_ui <- function() {
        block_browser_ui(
          session$ns("browser"), board$board, insert_into(trigger())
        )
      }

      observeEvent(trigger(), {
        show_sidebar(
          sidebar_id, title = "Insert new block", ui = browser_ui()
        )
      })

      # Splitting a link that has since been removed makes no sense, so close
      # rather than wait for a commit that could not be applied.
      observeEvent(board$board, {
        id <- trigger()
        if (length(id) == 1L && !is.na(id) && nzchar(id) &&
              !id %in% board_link_ids(board$board) &&
              owns_open_sidebar(sidebar_id)) {
          hide_sidebar(sidebar_id)
        }
      }, ignoreInit = TRUE)

      observeEvent(added(), {

        res <- added()

        # The menu yields no links when the split link has gone between
        # opening the panel and committing. Applying the block alone would
        # leave it stranded off the graph, so bail.
        if (!length(res$links)) {
          notify(
            "That link is no longer on the board.",
            type = "warning", session = session
          )
          hide_sidebar(sidebar_id)
          return()
        }

        # One update: `modify_board_links()` drops the split link before it
        # adds, so the far end's slot is free by the time the second new link
        # claims it.
        update(
          list(
            blocks = list(add = res$blocks),
            links = list(add = res$links, rm = trigger())
          )
        )

        # Closed even when pinned, unlike the append flow: a pin keeps a
        # panel open to repeat the gesture, and this gesture consumed its own
        # subject. Re-opening on a dead link id would render a panel that
        # cannot commit.
        hide_sidebar(sidebar_id)
      })

      NULL
    },
    id = "insert_block_action"
  )
}
