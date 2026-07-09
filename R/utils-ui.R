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

#' Show a panel
#'
#' `show_panel()` brings a block or extension panel into view in the live
#' `dock`. It is **deprecated**: opening a panel is now expressed through the
#' `views` update grammar, which an extension composes directly and passes to
#' `update()` -- switching to a view that holds the panel and selecting its tab,
#' `update(list(views = list(active = v, mod = list(v = list(select = p)))))`,
#' with a `mod` `add` entry first for an unplaced panel. That path needs no live
#' `dock` handle, which is no longer on the extension server surface.
#'
#' @param id Object ID
#' @param board Board object
#' @param dock Object available as `dock` in extensions
#' @param type Either "block" or "extension", the kind of panel to show
#'
#' @return `NULL`, invisibly.
#'
#' @rdname panel
#' @export
show_panel <- function(id, board, dock, type = c("block", "extension")) {

  .Deprecated(
    msg = paste(
      "`show_panel()` is deprecated; open a panel through the `views` update",
      "grammar instead (see `?show_panel`)."
    )
  )

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
