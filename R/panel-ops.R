# Deliver a view's panel-op batch to its live dock. Server-initiated ops land
# here through the applied `views$mod` payload (the `set_panel_title`
# precedent), the mirror image of the membership fold: `add` / `rm` write the
# board and this places / removes the panel, `move` / `select` write nothing and
# exist only as this delivery. Every op is idempotent against the live panel
# set, so the fold's own capture echo and a re-augmented payload are no-ops.
# Application order matches the reducer: rm -> add -> move -> select. Panels
# whose backing block is removed in the same update (`rm_blocks`) are left to
# core's block-removal path, which owns the active dock -- delivering them here
# too would double-remove and throw client-side.
deliver_panel_ops <- function(mod, dock, board, rm_blocks = character()) {

  skip <- as.character(as_block_panel_id(rm_blocks))

  for (pid in setdiff(mod[["rm"]] %||% character(), skip)) {
    op_remove_panel(pid, dock)
  }

  for (pid in names(mod[["add"]])) {
    op_add_panel(pid, mod[["add"]][[pid]], dock, board)
  }

  for (pid in names(mod[["move"]])) {
    op_move_panel(pid, mod[["move"]][[pid]], dock, board)
  }

  if (not_null(mod[["select"]])) {
    op_select_panel(mod[["select"]], dock)
  }

  invisible()
}

# The guard that makes every op idempotent: a target the live dock already
# reflects (a captured gesture, a re-augmented payload) is skipped. Keyed on the
# authoritative server-side membership set, not the lagging browser echo.
panel_is_live <- function(pid, dock) {
  as.character(pid) %in% as.character(isolate(dock$live_panels()))
}

op_add_panel <- function(pid, hint, dock, board) {

  pid <- as_dock_panel_id(pid)

  if (panel_is_live(pid, dock)) {
    return(invisible())
  }

  pos <- hint_to_position(hint, dock)
  obj <- as_obj_id(pid)

  if (is_block_panel_id(pid)) {

    if (!obj %in% board_block_ids(board)) {
      return(invisible())
    }

    show_block_panel(board_blocks(board)[obj], add_panel = pos, dock = dock)

  } else {

    if (!obj %in% dock_ext_ids(board)) {
      return(invisible())
    }

    show_ext_panel(
      as.list(dock_extensions(board))[[obj]], add_panel = pos, dock = dock
    )
  }

  invisible()
}

op_remove_panel <- function(pid, dock) {

  pid <- as_dock_panel_id(pid)

  if (!panel_is_live(pid, dock)) {
    return(invisible())
  }

  if (is_block_panel_id(pid)) {
    hide_block_panel(pid, rm_panel = TRUE, dock = dock)
  } else {
    hide_ext_panel(pid, rm_panel = TRUE, dock = dock)
  }

  invisible()
}

# A first-class move awaits cynkra/dockViewR#85; until then a move decomposes
# into remove + add-with-hint. The block / extension card is parked in the
# offcanvas and re-homed by the remove / add pair, so its server state and
# rendered output survive -- only the dockview panel wrapper is re-created.
op_move_panel <- function(pid, hint, dock, board) {

  if (!panel_is_live(as_dock_panel_id(pid), dock)) {
    return(invisible())
  }

  op_remove_panel(pid, dock)
  op_add_panel(pid, hint, dock, board)

  invisible()
}

op_select_panel <- function(pid, dock) {

  pid <- as_dock_panel_id(pid)

  if (!panel_is_live(pid, dock)) {
    return(invisible())
  }

  if (is_block_panel_id(pid)) {
    select_block_panel(pid, dock$proxy)
  } else {
    select_ext_panel(pid, dock$proxy)
  }

  invisible()
}

# Translate a grammar placement hint (`near` panel + `side`) into a dockview
# panel position (`referencePanel` + `direction`). Dockview addresses a group
# through a member panel, so `near` is a panel id, not a group. No hint -> the
# view's own default spot.
hint_to_position <- function(hint, dock) {

  if (is.null(hint) || !length(hint)) {
    return(determine_panel_pos(dock))
  }

  near <- hint[["near"]]

  drop_nulls(
    list(
      referencePanel = if (not_null(near)) as.character(as_dock_panel_id(near)),
      direction = hint[["side"]]
    )
  )
}
