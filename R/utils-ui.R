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

#' @keywords internal
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

determine_panel_pos <- function(dock) {
  sess <- dock$proxy$session

  if (sess$input[[dock_input("n-groups")]] < 2L) {
    return(list(direction = "right"))
  }

  prev <- dock$prev_active_group()
  curr <- sess$input[[dock_input("active-group")]]

  if (is.null(prev) || identical(curr, prev)) {
    grp <- last(setdiff(dock_panel_groups(sess), curr))
  } else {
    grp <- prev
  }

  list(referenceGroup = grp, direction = "within")
}

#' UI utilities
#'
#' Exported utilities for manipulating dock panels (i.e. displaying panels)
#'
#' @param id Object ID
#' @param board Board object
#' @param dock Object available as `dock` in extensions
#' @param type Either "block" or "extensions", depending on what kind of panel
#'   should be shown
#'
#' @return `NULL`, invisibly
#'
#' @rdname panel
#' @export
show_panel <- function(id, board, dock, type = c("block", "extension")) {
  stopifnot(is_string(id))

  type <- match.arg(type)
  proxy <- dock$proxy

  if (identical(type, "block")) {
    stopifnot(id %in% board_block_ids(board))
  } else {
    stopifnot(id %in% dock_ext_ids(board))
  }

  panels <- dock_panel_ids(proxy)

  if (identical(type, "block")) {
    panels <- panels[lgl_ply(panels, is_block_panel_id)]
  } else {
    panels <- panels[lgl_ply(panels, is_ext_panel_id)]
  }

  panels <- as_obj_id(panels)

  if (id %in% panels) {
    if (identical(type, "block")) {
      select_block_panel(id, proxy)
    } else {
      select_ext_panel(id, proxy)
    }

    return(invisible())
  }

  pos <- determine_panel_pos(dock)

  if (identical(type, "block")) {
    add_block_panel(id, position = pos, proxy = proxy)
    show_block_ui(id, proxy$session)
  } else {
    exts <- dock_extensions(board)

    add_ext_panel(exts[[id]], position = pos, proxy = proxy)
    show_ext_ui(id, proxy$session)
  }

  invisible()
}

drop_nulls <- function(x) {
  x[!lgl_ply(x, is.null)]
}
