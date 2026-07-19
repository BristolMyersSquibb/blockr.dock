# Build a `views` update that adds panels to a view -- the payload the add-panel
# modal emits at the click, in place of mutating the dock directly. Every panel
# takes the same placement: `within` the group the user clicked `+` on, resolved
# once to a member panel (`near`); an empty hint (empty dock) lets the apply
# side fall back to the view's default spot.
add_panel_delta <- function(view_id, panel_ids, near = NULL) {

  hint <- if (not_null(near)) list(near = near, side = "within") else list()

  add <- set_names(rep_len(list(hint), length(panel_ids)), panel_ids)

  list(views = list(mod = set_names(list(list(add = add)), view_id)))
}

# Build a `views` update that removes a panel from a view -- the payload the tab
# close (`x`) emits, in place of hiding the panel directly.
remove_panel_delta <- function(view_id, panel_id) {
  list(views = list(mod = set_names(list(list(rm = panel_id)), view_id)))
}

# Apply a view's panel-op batch to its live dock. Server-initiated ops land
# here through the applied `views$mod` payload (the `set_panel_title`
# precedent), the mirror image of the membership fold: `add` / `rm` write the
# board and this places / removes the panel, `move` / `select` write nothing and
# exist only as this client-side apply. Every op is idempotent against the live
# panel set, so the fold's own capture echo and a re-augmented payload are
# no-ops. Application order matches the reducer: rm -> add -> move -> select.
#
# `active` marks whether this dock is the active view. A block / extension card
# is a single board-level element, shown in whichever view is active, so only
# the active dock moves cards (`show_*` / `hide_*`); an inactive dock touches
# its dockview panel wrappers alone (`add_*_panel` / `remove_*_panel`) and
# leaves the card where it is -- activation re-parks it via `show_view_ui`.
# Core's own block-removal cleans only the active dock, so the same-update
# block-removal skip is active-only: an inactive dock applies the cascade `rm`
# to clear the wrapper core never reaches, and only the active dock defers to
# core (applying it there too would double-remove and throw client-side).
apply_panel_ops <- function(mod, dock, board, rm_blocks = character(),
                            active = TRUE) {

  skip <- if (active) {
    as.character(as_block_panel_id(rm_blocks))
  } else {
    character()
  }

  for (pid in setdiff(mod[["rm"]] %||% character(), skip)) {
    op_remove_panel(pid, dock, active)
  }

  for (pid in names(mod[["add"]])) {
    op_add_panel(pid, mod[["add"]][[pid]], dock, board, active)
  }

  for (pid in names(mod[["move"]])) {
    op_move_panel(pid, mod[["move"]][[pid]], dock)
  }

  for (pid in names(mod[["resize"]])) {
    op_resize_panel(pid, mod[["resize"]][[pid]], dock)
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

op_add_panel <- function(pid, hint, dock, board, active = TRUE) {

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

    block <- board_blocks(board)[obj]

    if (active) {
      # The block may have lived only in an off-screen view, so its card was
      # never built; build it before the show moves it into the panel. The
      # inactive branch only wraps a panel (no card move), so it needs none.
      ensure_block_ui(
        dock_board_ns(dock)(NULL), board, block,
        dock$visibility, plugins = dock$plugins, session = dock$proxy$session
      )
      show_block_panel(block, add_panel = pos, dock = dock)
    } else {
      add_block_panel(block, position = pos, dock = dock)
    }

  } else {

    if (!obj %in% dock_ext_ids(board)) {
      return(invisible())
    }

    ext <- dock_extensions(board)[obj]

    if (active) {
      show_ext_panel(ext, add_panel = pos, dock = dock)
    } else {
      add_ext_panel(ext, position = pos, dock = dock)
    }
  }

  invisible()
}

op_remove_panel <- function(pid, dock, active = TRUE) {

  pid <- as_dock_panel_id(pid)

  if (!panel_is_live(pid, dock)) {
    return(invisible())
  }

  if (is_block_panel_id(pid)) {
    if (active) {
      hide_block_panel(pid, rm_panel = TRUE, dock = dock)
    } else {
      remove_block_panel(pid, dock)
    }
  } else {
    if (active) {
      hide_ext_panel(pid, rm_panel = TRUE, dock = dock)
    } else {
      remove_ext_panel(pid, dock)
    }
  }

  invisible()
}

# A first-class move: dockViewR relocates the panel next to the hint in one
# step, carrying its block / extension card along, in place of the former
# remove + add-with-hint decomposition. The move is server-driven, so the grid
# mirror's server-source skip ignores the `_state` echo it provokes.
op_move_panel <- function(pid, hint, dock) {

  pid <- as_dock_panel_id(pid)

  if (!panel_is_live(pid, dock)) {
    return(invisible())
  }

  move_dock_panel(pid, hint_to_position(hint, dock), dock$proxy)

  invisible()
}

# A resize sets the size of the panel's group along its splitview axis. Geometry
# is client-owned and captured by the grid mirror, so -- like move / select --
# it writes nothing to the board and exists only as this delivery, idempotent
# against the settled ratio.
op_resize_panel <- function(pid, hint, dock) {

  pid <- as_dock_panel_id(pid)

  if (!panel_is_live(pid, dock)) {
    return(invisible())
  }

  resize_dock_panel(pid, hint[["size"]], dock$proxy)

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
