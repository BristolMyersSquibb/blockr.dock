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

  opts <- lapply(
    set_names(nm = names(as_board_options(x))),
    get_board_option_or_null,
    session
  )

  layout_data <- view_data()

  do.call(
    blockr_ser,
    c(
      list(
        x,
        board_id = id,
        blocks = Map(c, state, visible = lapply(visibility, list)),
        options = opts,
        layouts = layout_data,
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
  payload <- if (!missing(data)) coal(data, x) else x
  payload <- as_dock_layout(payload)
  list(
    object = class(x),
    payload = grid_to_wire(payload[["grid"]]),
    active = is_active_view(payload)
  )
}

#' @export
blockr_ser.dock_layouts <- function(x, data, ...) {
  lys <- if (!missing(data) && is_dock_layouts(data)) data else x
  list(
    object = class(lys),
    payload = list(
      active = active_view(lys),
      views = lapply(lys, blockr_ser)
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
  payload <- data[["payload"]]
  active <- isTRUE(data[["active"]])

  # Legacy payloads (pre-decoupling) embed dockview's `grid` tree directly,
  # possibly alongside a now-redundant `panels` map. Discriminate by shape:
  # see blockr.dock#153 for the planned version-based routing once
  # blockr.core forwards `...` through `blockr_deser.list`.
  if ("grid" %in% names(payload)) {
    grid <- payload[["grid"]]
    active_group <- payload[["activeGroup"]] %||% payload[["active_group"]]
  } else {
    grid <- wire_to_grid(payload)
    active_group <- NULL
  }

  set_active_view(
    new_dock_layout(grid = grid, active_group = active_group),
    active
  )
}

#' @export
blockr_deser.dock_layouts <- function(x, data, ...) {
  payload <- data[["payload"]]
  v_list <- lapply(payload[["views"]], blockr_deser)
  res <- dock_layouts(v_list)
  active_view(res) <- payload[["active"]]
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

#
# Wire format converters
#
# The persisted layout shape is a recursive tree, deliberately distinct
# from dockview's internal grid format. The top-level object carries
# `orientation` and `root`. A node is either a branch (children plus
# optional sizes) or a leaf (panels plus optional active tab).
#
# Sizes are ratios summing to 1; omitted when the split is even. The
# active tab is omitted when the open tab is the first panel (the
# implicit default). Dockview's internal fields (type tags, per-branch
# size, leaf id, layout-level activeGroup) are not stored — they're
# regenerated on the way back in.
#
# Dockview emits absolute pixel sizes from live state after a user
# resize; grid_to_wire() normalises them to ratios on save. The opposite
# direction (wire_to_grid()) re-assigns the dockview-style fields needed
# by restore_dock().

grid_to_wire <- function(grid) {

  walk <- function(node) {

    if (identical(node[["type"]], "leaf")) {
      views <- unlist(node[["data"]][["views"]])
      out <- list(panels = as.list(views))

      active <- node[["data"]][["activeView"]]
      if (length(views) > 1L && !identical(active, views[[1L]])) {
        out[["active"]] <- active
      }

      return(out)
    }

    if (identical(node[["type"]], "branch")) {
      children <- lapply(node[["data"]], walk)

      raw_sizes <- vapply(node[["data"]], `[[`, numeric(1), "size")
      norm_sizes <- normalise_sizes(raw_sizes)

      out <- list(children = children)

      if (length(norm_sizes) && !sizes_are_even(norm_sizes)) {
        out[["sizes"]] <- norm_sizes
      }

      return(out)
    }

    blockr_abort(
      "Unknown grid node type: {.val {node[['type']]}}.",
      class = "dock_layout_wire_invalid"
    )
  }

  list(
    orientation = tolower(grid[["orientation"]] %||% "horizontal"),
    root = walk(grid[["root"]])
  )
}

wire_to_grid <- function(wire) {

  group_id <- 0L

  next_id <- function() {
    group_id <<- group_id + 1L
    as.character(group_id)
  }

  walk <- function(node, size = 1) {

    if (!is.null(node[["panels"]])) {
      views <- as.list(unlist(node[["panels"]]))
      active <- node[["active"]] %||% views[[1L]]

      return(
        list(
          type = "leaf",
          data = list(
            views = views,
            activeView = active,
            id = next_id()
          ),
          size = size
        )
      )
    }

    if (!is.null(node[["children"]])) {
      n <- length(node[["children"]])
      child_sizes <- node[["sizes"]] %||% (
        if (n) rep(1 / n, n) else numeric()
      )
      children <- Map(walk, node[["children"]], child_sizes)

      return(
        list(
          type = "branch",
          data = filter_empty(children),
          size = size
        )
      )
    }

    blockr_abort(
      "Wire node must have either `panels` or `children`.",
      class = "dock_layout_wire_invalid"
    )
  }

  list(
    root = walk(wire[["root"]]),
    orientation = toupper(wire[["orientation"]] %||% "horizontal")
  )
}

normalise_sizes <- function(sizes) {

  if (!length(sizes)) {
    return(numeric())
  }

  total <- sum(sizes)
  if (total > 0) sizes / total else sizes
}

sizes_are_even <- function(sizes) {

  n <- length(sizes)
  if (n < 2L) {
    return(TRUE)
  }

  isTRUE(all.equal(sizes, rep(1 / n, n)))
}

#' @export
restore_board.dock_board <- function(x, new, result, ..., meta = NULL,
                                     session = get_session()) {

  des <- blockr_deser(new)

  extra <- list(
    extensions = dock_extensions(x),
    options = board_options(x),
    layouts = des[["layouts"]]
  )

  res <- do.call(as_dock_board, c(list(des), extra))

  if (is.null(meta)) {
    result(res)
  } else {
    result(list(board = res, meta = meta))
  }
}
