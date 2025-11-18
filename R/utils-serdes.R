#' @export
serialize_board.dock_board <- function(x, blocks, id = NULL, dock, ...,
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

  do.call(
    blockr_ser,
    c(
      list(
        x,
        board_id = id,
        blocks = Map(c, state, visible = lapply(visibility, list)),
        options = opts,
        layout = as_dock_layout(dock$layout()),
        extensions = lapply(
          list(...),
          function(x) if (is.list(x)) lapply(x, reval_if) else reval_if(x)
        )
      )
    )
  )
}

#' @export
blockr_ser.dock_layout <- function(x, data, ...) {
  list(object = class(x), payload = unclass(data))
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
    payload = map(blockr_ser, x, data[names(x)])
  )
}

#' @export
blockr_deser.dock_layout <- function(x, data, ...) {
  as_dock_layout(data[["payload"]], ...)
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
restore_board.dock_board <- function(x, new, result, ...,
                                     session = get_session()) {

  des <- blockr_deser(new)

  res <- as_dock_board(
    des,
    extensions = dock_extensions(x),
    options = board_options(x)
  )

  result(res)
}
