#' @export
block_ui.dock_board <- function(id, x, edit_ui, blocks = NULL, ...,
                                ctrl_ui = NULL) {

  stopifnot(is_string(id))

  if (is.null(blocks)) {
    blocks <- board_blocks(x)
  }

  stopifnot(is_blocks(blocks))

  if (!is.null(ctrl_ui)) ctrl_ui <- plugin_ui(ctrl_ui)

  map(
    block_card,
    blocks,
    names(blocks),
    MoreArgs = list(
      plugin = edit_ui,
      board = x,
      board_ns = NS(id),
      ctrl = ctrl_ui
    )
  )
}

block_card <- function(blk, blk_id, plugin, board, board_ns, ctrl = NULL) {

  blk_srv_id <- board_ns(paste0("block_", blk_id))

  if (is_plugin(plugin)) {
    edit_ui <- plugin_ui(plugin)
    edit_ns <- NS(blk_srv_id, plugin_id(plugin))
  } else {
    edit_ui <- plugin
    edit_ns <- NS(blk_srv_id, "edit_block")
  }

  if (!is.null(ctrl) && has_external_ctrl(blk)) {
    ctrl_tag <- ctrl(NS(blk_srv_id, "ctrl_block"), blk)
    ctrl_meta <- list(
      label = ctrl_btn_label(ctrl),
      icon  = ctrl_btn_icon(ctrl),
      class = ctrl_btn_class(ctrl)
    )
  } else {
    ctrl_tag <- NULL
    ctrl_meta <- NULL
  }

  card_tag <- div(
    class = "card",
    width = "100%",
    id = board_ns(as_block_handle_id(blk_id)),
    edit_ui(
      edit_ns,
      blk,
      blk_id,
      expr_ui(blk_srv_id, blk),
      block_ui(blk_srv_id, blk),
      ctrl_ui = ctrl_tag,
      ctrl_meta = ctrl_meta
    )
  )

  tagAppendAttributes(card_tag, class = "border border-0 shadow-none")
}

has_external_ctrl <- function(x) {
  ctrl <- attr(x, "external_ctrl")
  isTRUE(ctrl) || (is.character(ctrl) && length(ctrl) > 0L)
}

# Whether a block contributes any controls, decided by rendering its expression
# UI and asking whether that produced markup: core's `new_block()` default is
# `function(id) tagList()`, so a block with nothing to configure (`rbind_block`,
# whose only variation is its link set) yields an empty document. Rendering
# rather than inspecting the tags also catches a UI carrying html dependencies
# alone.
has_expr_ui <- function(x) {
  markup <- htmltools::renderTags(expr_ui("block", x))[["html"]]
  nzchar(trimws(as.character(markup)))
}

show_block_dep <- function() {
  htmltools::htmlDependency(
    "show-block",
    pkg_version(),
    src = pkg_file("assets", "js"),
    script = "show-block.js"
  )
}

#' @export
remove_block_ui.dock_board <- function(id, x, blocks, dock, ...,
                                       session = get_session()) {

  stopifnot(is.character(blocks), all(blocks %in% board_block_ids(x)))

  for (blk in blocks) {
    if (as_block_panel_id(blk) %in% block_panel_ids(dock$proxy)) {
      hide_block_panel(blk, dock = dock)
    }

    removeUI(
      as_block_handle_id(id),
      immediate = TRUE,
      session = session
    )
  }

  invisible(x)
}

#' @export
insert_block_ui.dock_board <- function(id, x, blocks, dock, ...,
                                       edit_ui, ctrl_ui = NULL,
                                       session = get_session()) {

  stopifnot(is_blocks(blocks))

  build_block_ui(
    id, x, blocks, dock$visibility, ...,
    edit_ui = edit_ui, ctrl_ui = ctrl_ui, session = session
  )

  for (i in names(blocks)) {
    add_block_panel(blocks[i], position = determine_panel_pos(dock),
                    dock = dock)
  }

  show_block_ui(names(blocks), dock$proxy$session,
                board_ns = dock_board_ns(dock))

  invisible(x)
}

# The dock's build ledger is core's `visible` axis: a per-block reactiveVal,
# logical (NA never built / FALSE built off screen / TRUE painted now), so
# `built_cards()` reads `!is.na(visible)`. It is written only where the dock
# builds, paints or parks a card, which is what keeps it a ledger: evaluation
# demand travels the `sustain` claim instead, where a peer owner (core's "Show
# code") can hold a block the dock never carded without that reading back as
# built. Block removal needs no dock write -- core drops the slot, dropping the
# card from the ledger too.
built_cards <- function(visibility) {
  ids <- ls(visibility$visible)
  ids[lgl_ply(ids, slot_built, visibility$visible)]
}

slot_built <- function(id, visible) {
  !is.na(isolate(visible[[id]]()))
}

# Record `new` cards as built off screen: `visible` FALSE enters them in the
# ledger (built, not yet painted). Slots pre-exist: core seeds every block's
# before any block plugin runs.
mark_cards_built <- function(visibility, new) {
  for (id in new) {
    visibility$visible[[id]](FALSE)
  }
}

# Park the cards that have left the screen: `visible` goes FALSE (built, off
# screen), never NA (never built), or their view blanks on the next visit.
mark_cards_hidden <- function(visibility, hidden) {
  for (id in hidden) {
    if (isTRUE(isolate(visibility$visible[[id]]()))) {
      visibility$visible[[id]](FALSE)
    }
  }
}

# Mark a view's on-screen blocks painted: `visible` TRUE -- the client-confirmed
# paint core's render gate (is_visible = isTRUE) waits for.
mark_cards_rendered <- function(visibility, on_screen) {
  for (id in on_screen) {
    if (!isTRUE(isolate(visibility$visible[[id]]()))) {
      visibility$visible[[id]](TRUE)
    }
  }
}

# Insert the not-yet-built cards in `blocks` and mark them built; already-built
# blocks are skipped, so this is safe to call ahead of any path that shows one.
build_block_ui <- function(id, x, blocks, visibility, ..., edit_ui,
                           ctrl_ui = NULL, session = get_session()) {

  new <- setdiff(names(blocks), built_cards(visibility))

  for (i in new) {
    insertUI(
      paste0("#", id, "-blocks_offcanvas"),
      "beforeEnd",
      block_ui(id, x, edit_ui, blocks[i], ..., ctrl_ui = ctrl_ui)[[1L]],
      immediate = TRUE,
      session = session
    )
  }

  mark_cards_built(visibility, new)

  invisible(new)
}

# build_block_ui for callers that hold the board but not the plugins (the view
# switch, the add-panel modal). Takes the served plugin set, since
# board_plugins(x) is only the board default and drops any served ctrl_block;
# serve() threads the real set via active_dock, falling back to that default.
ensure_block_ui <- function(id, x, blocks, visibility,
                            plugins = board_plugins(x),
                            session = get_session()) {

  if (all(names(blocks) %in% built_cards(visibility))) {
    return(invisible(character()))
  }

  build_block_ui(
    id,
    x,
    blocks,
    visibility,
    edit_ui = plugins[["edit_block"]],
    ctrl_ui = if ("ctrl_block" %in% names(plugins)) plugins[["ctrl_block"]],
    session = session
  )
}

show_block_panel <- function(block, add_panel = TRUE, dock, ...) {

  if (isTRUE(add_panel)) {
    add_block_panel(block, dock = dock)
  } else if (isFALSE(add_panel)) {
    select_block_panel(block, dock$proxy)
  } else {
    add_block_panel(block, position = add_panel, dock = dock)
  }

  show_block_ui(block, dock$proxy$session,
                board_ns = dock_board_ns(dock), ...)

  invisible(NULL)
}

hide_block_panel <- function(id, rm_panel = TRUE, dock, ...) {

  hide_block_ui(id, dock$proxy$session,
                board_ns = dock_board_ns(dock), ...)

  if (isTRUE(rm_panel)) {
    remove_block_panel(id, dock)
  }

  invisible(NULL)
}

hide_block_ui <- function(ids, session, board_ns = session$ns) {

  hid <- as_block_handle_id(ids)

  if (!length(hid)) {
    return(invisible(NULL))
  }

  bid <- board_ns(hid)
  oid <- paste0(board_ns("blocks_offcanvas"), " .offcanvas-body")

  log_debug("hiding {cli::qty(length(bid))}block{?s} {bid} in {oid}")

  move_dom_elements(paste0("#", bid), paste0("#", oid), session)
}

show_block_ui <- function(ids, session, board_ns = session$ns) {

  hid <- as_block_handle_id(ids)

  if (!length(hid)) {
    return(invisible(NULL))
  }

  # board_ns: board-level namespace for DOM element IDs (handles, offcanvas).
  # session$ns: dock-module namespace for dock panel IDs.
  # These differ when called from a nested dock module (views).
  bid <- board_ns(hid)
  pid <- paste(dock_id(session$ns), as_block_panel_id(ids), sep = "-")

  log_debug(
    "showing {cli::qty(length(bid))}block{?s} {bid} in ",
    "{cli::qty(length(pid))}panel{?s} {pid}"
  )

  move_dom_elements(paste0("#", bid), paste0("#", pid), session)
}
