#' @export
serialize_board.dock_board <- function(x, blocks, id = NULL, dock,
                                       ws_data = NULL, ...,
                                       session = get_session()) {

  state <- lapply(
    lst_xtr(blocks, "server", "state"),
    lapply,
    reval_if
  )

  visibility <- lapply(
    lst_xtr(blocks, "server", "visible"),
    reval_if
  )

  opts <- lapply(
    set_names(nm = names(as_board_options(x))),
    get_board_option_or_null,
    session
  )

  ly <- x[["layout"]]

  layout_data <- if (is_dock_workspaces(ly)) {
    ws_data()
  } else {
    as_dock_layout(dock$layout())
  }

  do.call(
    blockr_ser,
    c(
      list(
        x,
        board_id = id,
        blocks = Map(c, state, visible = lapply(visibility, list)),
        options = opts,
        layout = layout_data,
        extensions = lapply(
          list(...),
          function(x) lapply(x[["state"]], reval_if)
        )
      )
    )
  )
}

#' @export
blockr_ser.dock_layout <- function(x, data, ...) {
  list(object = class(x), payload = unclass(coal(data, x)))
}

#' @export
blockr_ser.dock_workspace <- function(x, data, ...) {
  list(
    object = class(x),
    payload = list(layout = unclass(workspace_layout(x)))
  )
}

#' @export
blockr_ser.dock_workspaces <- function(x, data, ...) {
  ws <- if (!missing(data) && is_dock_workspaces(data)) data else x
  list(
    object = class(ws),
    payload = list(
      active = active_workspace(ws),
      workspaces = lapply(ws, blockr_ser)
    )
  )
}

#' @export
blockr_ser.dock_extension <- function(x, data, ...) {
  list(
    object = class(x),
    payload = coal(data, list()),
    constructor = blockr_ser(extension_ctor(x))
  )
}

#' @export
blockr_ser.dock_extensions <- function(x, data, ...) {
  list(
    object = class(x),
    payload = map(blockr_ser, x, coal(data, list())[names(x)])
  )
}

#' @export
blockr_deser.dock_layout <- function(x, data, ...) {
  as_dock_layout(data[["payload"]], ...)
}

#' @export
blockr_deser.dock_workspace <- function(x, data, ...) {
  dock_workspace(layout = data[["payload"]][["layout"]])
}

#' @export
blockr_deser.dock_workspaces <- function(x, data, ...) {
  payload <- data[["payload"]]
  ws_list <- lapply(payload[["workspaces"]], blockr_deser)
  res <- dock_workspaces(ws_list)
  active_workspace(res) <- payload[["active"]]
  res
}

#' @export
blockr_deser.dock_extension <- function(x, data, ...) {

  stopifnot(
    all(c("constructor", "payload") %in% names(data))
  )

  payload <- data[["payload"]]
  ctor <- blockr_deser(data[["constructor"]])

  if (is.atomic(payload)) {
    payload <- list(payload)
  }

  args <- c(
    payload,
    list(
      ctor = coal(ctor_name(ctor), ctor_fun(ctor)),
      pkg = ctor_pkg(ctor)
    )
  )

  do.call(ctor_fun(ctor), args)
}

#' @export
blockr_deser.dock_extensions <- function(x, data, ...) {
  as_dock_extensions(lapply(data[["payload"]], blockr_deser))
}

#' @export
restore_board.dock_board <- function(x, new, result, ..., meta = NULL,
                                     session = get_session()) {

  des <- blockr_deser(new)

  # Preserve workspace layout if present in deserialized board
  extra <- list(
    extensions = dock_extensions(x),
    options = board_options(x)
  )

  ly <- des[["layout"]]
  if (is_dock_workspaces(ly)) {
    extra[["layout"]] <- ly
  }

  res <- do.call(as_dock_board, c(list(des), extra))

  if (is.null(meta)) {
    result(res)
  } else {
    result(list(board = res, meta = meta))
  }
}
