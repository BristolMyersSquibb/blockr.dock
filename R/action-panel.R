# Panel placement ops: add (with a position hint), remove and select a panel
# in a live view, driven through the action registry so an extension or API
# caller can place panels the way the add-panel modal and a tab drag already
# do. Unlike every other board action these write nothing to the board: the op
# mutates the live dock only, and the view's membership fold plus the settled
# grid mirror capture the result afterwards -- a server-initiated placement is
# indistinguishable, to the board, from the user having dragged the panels
# there. A position hint rides the op, is consumed once at insertion and is
# never stored. `dock` is the active view's dock; `docks` resolves a named
# target view, defaulting to the active one.

add_panel_action <- function(trigger, board, update, dock, docks = NULL, ...) {
  new_action(
    function(input, output, session) {

      observeEvent(
        trigger(),
        {
          op <- trigger()

          add_dock_panel(
            op[["panel"]],
            board$board,
            resolve_op_dock(dock, docks, op[["view"]]),
            position = op[["position"]]
          )
        }
      )

      NULL
    },
    id = "add_panel_action"
  )
}

remove_panel_action <- function(trigger, board, update, dock, docks = NULL,
                                ...) {
  new_action(
    function(input, output, session) {

      observeEvent(
        trigger(),
        {
          op <- trigger()

          remove_dock_panel(
            op[["panel"]],
            resolve_op_dock(dock, docks, op[["view"]])
          )
        }
      )

      NULL
    },
    id = "remove_panel_action"
  )
}

select_panel_action <- function(trigger, board, update, dock, docks = NULL,
                                ...) {
  new_action(
    function(input, output, session) {

      observeEvent(
        trigger(),
        {
          op <- trigger()

          select_dock_panel(
            op[["panel"]],
            resolve_op_dock(dock, docks, op[["view"]])
          )
        }
      )

      NULL
    },
    id = "select_panel_action"
  )
}

resolve_op_dock <- function(active, docks, view = NULL) {

  if (is.null(view) || is.null(docks) || !view %in% names(docks)) {
    return(active)
  }

  docks[[view]]
}

add_dock_panel <- function(pid, board, dock, position = NULL) {

  pid <- as_dock_panel_id(pid)

  if (is.null(position)) {
    position <- determine_panel_pos(dock)
  }

  if (is_block_panel_id(pid)) {
    show_block_panel(
      board_blocks(board)[as_obj_id(pid)],
      add_panel = position,
      dock = dock
    )
  } else {
    show_ext_panel(
      as.list(dock_extensions(board))[[as_obj_id(pid)]],
      add_panel = position,
      dock = dock
    )
  }

  invisible(NULL)
}

remove_dock_panel <- function(pid, dock) {

  pid <- as_dock_panel_id(pid)

  if (is_block_panel_id(pid)) {
    hide_block_panel(pid, rm_panel = TRUE, dock = dock)
  } else {
    hide_ext_panel(pid, rm_panel = TRUE, dock = dock)
  }

  invisible(NULL)
}

select_dock_panel <- function(pid, dock) {

  pid <- as_dock_panel_id(pid)

  if (is_block_panel_id(pid)) {
    show_block_panel(pid, add_panel = FALSE, dock = dock)
  } else {
    show_ext_panel(pid, add_panel = FALSE, dock = dock)
  }

  invisible(NULL)
}
