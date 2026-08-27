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

# Record `new` cards as built off screen in core's build ledger -- the `visible`
# axis, read back with built_block_ids(). Core enters what board_ui painted and
# what it inserts itself, leaving this for the cards the dock builds on a first
# visit: `visible` FALSE enters them in the ledger (built, not yet painted),
# `required` FALSE holds no construction demand until a view switch or the
# report observer places them. The dock keeps writing `required[[id]]` FALSE
# off screen / TRUE on screen (construction demand); block removal needs no
# dock write -- core drops the slot, dropping the card from the ledger too.
# Slots pre-exist: core seeds every block's before any block plugin runs.
mark_cards_built <- function(visibility, new) {
  for (id in new) {
    visibility$visible[[id]](FALSE)
    visibility$required[[id]](FALSE)
  }
}

# Reconcile the required axis over `built` from an on-screen set: on screen ->
# TRUE, off screen -> FALSE. A card leaving the screen keeps its ledger entry --
# `visible` goes FALSE (built, off screen), never NA (never built), or its view
# blanks on the next visit. The arrange observer owns marking a card painted
# (visible TRUE). Seeds the required axis too, with `built` the active view's
# membership.
show_cards <- function(visibility, built, on_screen) {
  for (id in built) {
    on <- id %in% on_screen

    if (!identical(isolate(visibility$required[[id]]()), on)) {
      visibility$required[[id]](on)
    }

    if (!on && isTRUE(isolate(visibility$visible[[id]]()))) {
      visibility$visible[[id]](FALSE)
    }
  }
}

# Mark a view's on-screen blocks painted: `visible` TRUE -- the client-confirmed
# paint core's render gate (is_visible = isTRUE) waits for. Unlike show_cards()
# this walks the caller's set and dereferences each slot, so that set must
# already be reconciled against core's visibility slots -- see
# report_visible_observer().
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

  new <- setdiff(names(blocks), built_block_ids(visibility))

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

  if (all(names(blocks) %in% built_block_ids(visibility))) {
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
