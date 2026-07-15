#' @export
serialize_board.dock_board <- function(x, blocks, id = NULL, dock,
                                       view_data = NULL, ...,
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

  # Deferred construction (core's needed-slot gate, background_construction_delay)
  # leaves off-screen blocks unbuilt, so `blocks` only carries the constructed
  # ones -- a PARTIAL snapshot. blockr_ser.blocks asserts one entry per board
  # block (`length(blocks) == length(x)`), so a partial snapshot aborts the whole
  # save with "length(blocks) == length(x) is not TRUE". Pad to every board block
  # keyed by id, exactly like core's serialize_board.board does: an unbuilt block
  # maps to NULL, which blockr_ser.block serializes from its constructor scope
  # (its restored state is preserved, not blanked).
  bid <- board_block_ids(x)
  blocks <- set_names(
    lapply(bid, function(i) {
      st <- state[[i]]
      if (is.null(st)) NULL else c(st, visible = list(visibility[[i]]))
    }),
    bid
  )

  opts <- lapply(
    set_names(nm = names(as_board_options(x))),
    get_board_option_or_null,
    session
  )

  # Structure and grid are board state now -- membership through the update
  # lifecycle, geometry through the settled-echo mirror -- so both slots
  # serialize straight from the last committed board, uniformly last-commit
  # fresh like board_links. The live layout is no longer read here.
  do.call(
    blockr_ser,
    c(
      list(
        x,
        board_id = id,
        blocks = blocks,
        options = opts,
        extensions = lapply(
          list(...),
          function(x) lapply(x[["state"]], reval_if)
        )
      )
    )
  )
}

#' @export
blockr_ser.dock_views <- function(x, data, ...) {
  vws <- if (!missing(data) && is_dock_views(data)) data else x
  list(
    object = class(vws),
    payload = list(
      active = active_view(vws),
      views = lapply(vws, blockr_ser)
    )
  )
}

#' @export
blockr_ser.dock_view <- function(x, data, ...) {
  view <- if (!missing(data) && is_dock_view(data)) data else x
  list(
    object = class(view),
    payload = as.list(view_members(view)),
    name = view_name(view)
  )
}

#' @export
blockr_ser.dock_grids <- function(x, data, ...) {
  arr <- if (!missing(data) && is_dock_grids(data)) data else x
  list(
    object = class(arr),
    payload = lapply(Filter(Negate(is.null), as.list(arr)), as.list)
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
    payload = set_names(
      map(blockr_ser, x, coal(data, list())[names(x)]),
      names(x)
    )
  )
}

#' @export
blockr_deser.dock_board <- function(x, data, ...) {

  stopifnot(
    all(c("constructor", "payload") %in% names(data))
  )

  ctor <- blockr_deser(data[["constructor"]])

  # Reimplemented rather than delegated to core's blockr_deser.board: it
  # drops extra args, so threading the producer version downward means
  # owning the payload deser here.
  des <- lapply(
    data[["payload"]],
    blockr_deser,
    producer_version = data[["constructor"]][["version"]]
  )

  # The split structure / grid slots feed the constructor's `views` / `grids`
  # arguments directly -- no fused round-trip.
  args <- c(
    des[setdiff(names(des), c("views", "grids"))],
    list(
      views = des[["views"]],
      grids = des[["grids"]],
      ctor = coal(ctor_name(ctor), ctor_fun(ctor)),
      pkg = ctor_pkg(ctor)
    )
  )

  res <- do.call(ctor_fun(ctor), args)

  attr(res, "id") <- data[["id"]]

  res
}

#' @export
blockr_deser.dock_views <- function(x, data, ...) {
  payload <- data[["payload"]]

  res <- reconstruct_dock_views(
    lapply(payload[["views"]], blockr_deser, ...)
  )

  if (is_string(payload[["active"]]) && payload[["active"]] %in% names(res)) {
    active_view(res) <- payload[["active"]]
  }

  res
}

#' @export
blockr_deser.dock_view <- function(x, data, ...) {
  name <- if (is_string(data[["name"]])) data[["name"]] else NULL
  new_dock_view(as.character(unlst(data[["payload"]])), name = name)
}

#' @export
blockr_deser.dock_grids <- function(x, data, ...) {
  new_dock_grids(lapply(data[["payload"]], as_dock_grid))
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
