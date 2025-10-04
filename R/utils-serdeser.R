ser_deser_ui <- function(id, board) {

  import_btn <- htmltools::tagQuery(
    fileInput(
      NS(id, "restore"),
      buttonLabel = "Import",
      label = "",
      placeholder = "Select a board file"
    )
  )$find(".btn")$addClass("btn-sm")$reset()$find(".input-group")$addClass(
    "input-group-sm"
  )$allTags()

  div(
    class = "d-flex justify-content-center align-items-center gap-1",
    htmltools::tagAppendAttributes(
      import_btn,
      style = "margin-bottom: 0.5rem"
    ),
    downloadButton(
      NS(id, "serialize"),
      "Export",
      class = "btn-sm",
      icon = icon("file-export"),
    )
  )
}

#' @export
serialize_board.dock_board <- function(x, blocks, layout, ...,
                                       session = get_session()) {

  blocks <- lapply(
    lst_xtr(blocks, "server", "state"),
    lapply,
    reval_if
  )

  opts <- lapply(
    set_names(nm = names(as_board_options(x))),
    get_board_option_or_null,
    session
  )

  blockr_ser(
    x,
    blocks = blocks,
    options = opts,
    layout = layout()
  )
}

#' @export
blockr_ser.dock_board <- function(x, blocks = NULL, options = NULL,
                                  layout = NULL, ...) {

  res <- NextMethod()

  res[["layout"]] <- blockr_ser(as_dock_layout(layout))
  res[["version"]] <- c(as.character(pkg_version()), res[["version"]])

  res
}

#' @export
blockr_ser.dock_layout <- function(x, ...) {
  list(object = class(x), payload = unclass(x))
}

#' @export
blockr_deser.dock_board <- function(x, data, ...) {
  do.call(
    new_dock_board,
    lapply(
      data[!names(data) %in% c("version", "object")],
      blockr_deser
    )
  )
}

#' @export
blockr_deser.dock_layout <- function(x, data, ...) {
  as_dock_layout(data[["payload"]], ...)
}

#' @export
restore_board.dock_board <- function(x, new, result, ...,
                                     session = get_session()) {
  result(blockr_deser(new))
}
