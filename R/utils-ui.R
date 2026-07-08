off_canvas <- function(
  id,
  title,
  ...,
  width = "w-25",
  position = c("start", "top", "bottom", "end")
) {
  label <- paste0(id, "-title")

  div(
    class = glue("offcanvas offcanvas-{match.arg(position)} {width}"),
    tabindex = "-1",
    id = id,
    `aria-labelledby` = label,
    `data-bs-scroll` = "true",
    div(
      class = "offcanvas-header",
      h5(class = "offcanvas-title", id = label, title),
      tags$button(
        type = "button",
        class = "btn-close",
        `data-bs-dismiss` = "offcanvas",
        `aria-label` = "Close"
      )
    ),
    div(
      class = "offcanvas-body",
      ...
    )
  )
}

collapse_container <- function(id, ...) {
  tags$div(class = "collapse", id = id, ...)
}

move_dom_element <- function(from, to, session = get_session()) {
  session$sendCustomMessage(
    "move-element",
    list(
      from = from,
      to = to
    )
  )
}

determine_active_views <- function(layout) {

  if (is.null(layout)) {
    return(character())
  }

  # The dockView tree, keyed by group id: a compact `dock_grid` is expanded
  # (assigning ids), while a raw dockView `_state` echo already carries its
  # tree at `$grid`. A group's active view is its open tab.
  tree <- if (is_dock_grid(layout)) grid_to_tree(layout) else layout[["grid"]]

  root <- tree[["root"]]

  if (is.null(root)) {
    return(character())
  }

  xtr_leaf <- function(x) {

    if (identical(x[["type"]], "leaf")) {
      return(
        set_names(coal(x[["data"]][["activeView"]], "", fail_all = FALSE),
                  x[["data"]][["id"]])
      )
    }

    lapply(x[["data"]], xtr_leaf)
  }

  rapply(xtr_leaf(root), identity, "character")
}

visible_block_ids <- function(layout) {

  front_panels <- as.character(determine_active_views(layout))
  block_panels <- front_panels[maybe_block_panel_id(front_panels)]

  as_obj_id(new_block_panel_id(block_panels))
}

visible_exts <- function() {
  blockr_option("visible_extensions", "dag_extension")
}

determine_panel_pos <- function(dock) {

  active <- determine_active_views(dock$layout())

  keep_visible <- as_ext_panel_id(visible_exts())

  cands <- names(active)[!active %in% keep_visible]

  if (!length(cands)) {
    return(list(direction = "right"))
  }

  prev <- dock$prev_active_group()

  if (isTRUE(prev %in% cands)) {
    grp <- prev
  } else {
    grp <- last(cands)
  }

  list(referenceGroup = grp, direction = "within")
}

#' Panel utilities
#'
#' Utilities for revealing dock panels. `reveal_panel()` is a pure builder:
#' it composes a `views` update delta that brings a panel into view --
#' switching to a view that holds it (outer `active`) and selecting its tab
#' (inner `select`) -- for a caller to pass straight to `update()`. Revealing
#' is not a primitive verb but the composition of two, offered as a builder so
#' the grammar stays minimal. This is the supported way for a dock extension --
#' which no longer receives the live `dock` -- to open a block's panel: a DAG
#' node click becomes `update(reveal_panel(board, node))`. `show_panel()` is
#' the older live-mutation form, requiring the `dock` handle directly.
#'
#' @param id Object ID
#' @param board Board object
#' @param dock Object available as `dock` in extensions
#' @param type Either "block" or "extensions", depending on what kind of panel
#'   should be shown
#' @param panel Panel to reveal: a canonical panel id, or a bare block /
#'   extension id.
#'
#' @return `show_panel()` returns `NULL` invisibly, called for its effect on the
#'   live dock. `reveal_panel()` returns a `views` update delta, or `NULL` when
#'   no view holds the panel.
#'
#' @examples
#' brd <- new_dock_board(
#'   blocks = c(a = blockr.core::new_dataset_block()),
#'   views = list(one = "a", two = "a")
#' )
#' reveal_panel(brd, "a")
#'
#' @rdname panel
#' @export
reveal_panel <- function(board, panel) {

  stopifnot(is_dock_board(board), is_string(panel))

  pid <- as.character(resolve_reveal_panel(board, panel))

  views <- board_views(board)

  holder <- Find(
    function(v) pid %in% view_members(views[[v]]),
    c(active_view(views), names(views))
  )

  if (is.null(holder)) {
    return(NULL)
  }

  list(
    views = list(
      active = holder,
      mod = set_names(list(list(select = pid)), holder)
    )
  )
}

resolve_reveal_panel <- function(board, panel) {

  if (maybe_block_panel_id(panel) || maybe_ext_panel_id(panel)) {
    return(as_dock_panel_id(panel))
  }

  if (panel %in% board_block_ids(board)) {
    return(as_block_panel_id(panel))
  }

  if (panel %in% dock_ext_ids(board)) {
    return(as_ext_panel_id(panel))
  }

  blockr_abort(
    "Cannot reveal {panel}: no such panel, block or extension.",
    class = "dock_reveal_panel_unknown"
  )
}

#' @rdname panel
#' @export
show_panel <- function(id, board, dock, type = c("block", "extension")) {

  stopifnot(is_string(id))

  type <- match.arg(type)

  if (identical(type, "block")) {
    stopifnot(id %in% board_block_ids(board))
  } else {
    stopifnot(id %in% dock_ext_ids(board))
  }

  panels <- dock_panel_ids(dock$proxy)

  if (identical(type, "block")) {
    panels <- panels[lgl_ply(panels, is_block_panel_id)]
  } else {
    panels <- panels[lgl_ply(panels, is_ext_panel_id)]
  }

  panels <- as_obj_id(panels)

  if (id %in% panels) {
    if (identical(type, "block")) {
      select_block_panel(id, dock$proxy)
    } else {
      select_ext_panel(id, dock$proxy)
    }

    return(invisible())
  }

  pos <- determine_panel_pos(dock)

  if (identical(type, "block")) {
    blocks <- board_blocks(board)
    add_block_panel(blocks[id], position = pos, dock = dock)
    show_block_ui(id, dock$proxy$session, board_ns = dock_board_ns(dock))
  } else {
    exts <- dock_extensions(board)

    add_ext_panel(exts[[id]], position = pos, dock = dock)
    show_ext_ui(id, dock$proxy$session, board_ns = dock_board_ns(dock))
  }

  invisible()
}

#' @noRd
empty_dock_prompt <- function(ns) {
  div(
    class = "blockr-empty-dock-prompt",
    bsicons::bs_icon("plus-circle", size = "2em"),
    tags$p("Start by adding a panel"),
    actionButton(
      ns("empty_dock_add"),
      "Add panel",
      class = "btn-outline-primary btn-sm"
    )
  )
}

drop_nulls <- function(x) {
  x[!lgl_ply(x, is.null)]
}
