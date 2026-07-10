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

  drop_cards(dock$visible, blocks)

  invisible(x)
}

#' @export
insert_block_ui.dock_board <- function(id, x, blocks, dock, ...,
                                       edit_ui, ctrl_ui = NULL,
                                       session = get_session()) {

  stopifnot(is_blocks(blocks))

  build_block_ui(
    id, x, blocks, dock$visible, ...,
    edit_ui = edit_ui, ctrl_ui = ctrl_ui, session = session
  )

  for (i in names(blocks)) {
    show_block_panel(blocks[i], determine_panel_pos(dock), dock)
  }

  invisible(x)
}

# The per-block visibility map lives on the `visible` reactiveVal core hands the
# board callback -- block id -> `parked` (built into the offcanvas pool, off
# screen) | `required` (on screen, not yet arranged) | `rendered` (arranged into
# its panel). A present key is a built card, so the dock reads this back as its
# build ledger (`built_cards()`); there is no second copy. Core keys its gates
# on required / rendered, so a `parked` entry reads as off screen there -- it is
# the dock's build-ledger bit that core retains as the single store.

# Recompute the map: every built card `parked`, except the on-screen fronts,
# which are `rendered` (once the dock has arranged) else `required`. `on_screen`
# is a subset of the built set (a shown card was built), so the intersect is a
# guard, not a filter.
card_visibility <- function(built, on_screen, arranged) {

  status <- set_names(rep("parked", length(built)), built)

  shown <- intersect(built, on_screen)

  if (length(shown)) {
    status[shown] <- if (arranged) "rendered" else "required"
  }

  status
}

# The blocks whose cards are built, read off the shared channel.
built_cards <- function(visible) {
  coal(names(isolate(visible())), character(), fail_all = FALSE)
}

# Record `new` cards as built -- `parked` until the report observer places them.
mark_cards_built <- function(visible, new) {

  cur <- coal(isolate(visible()), character(), fail_all = FALSE)
  cur[new] <- "parked"

  visible(cur)
}

# Drop cards whose blocks are gone from the map.
drop_cards <- function(visible, gone) {

  cur <- coal(isolate(visible()), character(), fail_all = FALSE)

  visible(cur[setdiff(names(cur), gone)])
}

# Insert the not-yet-built cards in `blocks` and record them on the channel;
# already-built blocks are skipped, so this is safe to call ahead of any path
# that shows one.
build_block_ui <- function(id, x, blocks, visible, ..., edit_ui, ctrl_ui = NULL,
                           session = get_session()) {

  new <- setdiff(names(blocks), built_cards(visible))

  for (i in new) {
    insertUI(
      paste0("#", id, "-blocks_offcanvas"),
      "beforeEnd",
      block_ui(id, x, edit_ui, blocks[i], ..., ctrl_ui = ctrl_ui)[[1L]],
      immediate = TRUE,
      session = session
    )
  }

  mark_cards_built(visible, new)

  invisible(new)
}

# build_block_ui for callers that hold the board but not the plugins (the view
# switch, the add-panel modal), deriving edit / ctrl UI the way board_ui does.
ensure_block_ui <- function(id, x, blocks, visible, session = get_session()) {

  if (all(names(blocks) %in% built_cards(visible))) {
    return(invisible(character()))
  }

  plugins <- board_plugins(x)

  build_block_ui(
    id,
    x,
    blocks,
    visible,
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

hide_block_ui <- function(id, session, board_ns = session$ns) {

  bid <- board_ns(as_block_handle_id(id))
  oid <- paste0(board_ns("blocks_offcanvas"), " .offcanvas-body")

  log_debug("hiding block {bid} in {oid}")

  move_dom_element(paste0("#", bid), paste0("#", oid), session)
}

show_block_ui <- function(id, session, board_ns = session$ns) {

  # board_ns: board-level namespace for DOM element IDs (handles, offcanvas).
  # session$ns: dock-module namespace for dock panel IDs.
  # These differ when called from a nested dock module (views).
  bid <- board_ns(as_block_handle_id(id))
  pid <- paste(dock_id(session$ns), as_block_panel_id(id), sep = "-")

  log_debug("showing block {bid} in panel {pid}")

  move_dom_element(paste0("#", bid), paste0("#", pid), session)
}
