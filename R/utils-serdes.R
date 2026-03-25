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

  # Rebuild workspace state from runtime data (captures runtime-created
  # workspaces and current layouts, not just the initial board definition)
  if (!is.null(dock$ws_map)) {
    x[["workspaces"]] <- build_runtime_workspaces(x, dock)
  }

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
          function(x) lapply(x[["state"]], reval_if)
        )
      )
    )
  )
}

# Build the current workspace structure from runtime state.
# This captures runtime-created workspaces and current DockView layouts
# (not just the initial board definition).
build_runtime_workspaces <- function(board, dock) {
  original_ws <- dock_workspaces(board)
  wm <- dock$ws_map()
  leaf_parent <- dock$leaf_parent

  # Reverse ws_map: block_id -> c(ws1, ws2) => ws -> block_ids
  ws_blocks <- list()
  for (bid in names(wm$blocks)) {
    for (ws in wm$blocks[[bid]]) {
      ws_blocks[[ws]] <- c(ws_blocks[[ws]], bid)
    }
  }

  # Reverse ws_map: ext_id -> c(ws1, ws2) => ws -> ext_ids
  ws_exts <- list()
  for (eid in names(wm$exts)) {
    for (ws in wm$exts[[eid]]) {
      ws_exts[[ws]] <- c(ws_exts[[ws]], eid)
    }
  }

  # Get all active proxy names (filter NULL entries from reactiveValues)
  proxy_list <- reactiveValuesToList(dock$proxies)
  active_names <- names(Filter(Negate(is.null), proxy_list))

  # Build leaf specs from runtime state
  build_leaf <- function(ws_name) {
    layout <- tryCatch(
      as_dock_layout(dockViewR::get_dock(dock$proxies[[ws_name]])),
      error = function(e) NULL
    )
    list(
      block_ids = ws_blocks[[ws_name]] %||% character(),
      ext_ids = ws_exts[[ws_name]] %||% character(),
      layout = layout,
      disabled = FALSE
    )
  }

  # Rebuild parent -> children structure from leaf_parent env
  parent_children <- list()
  for (nm in active_names) {
    parent <- get0(nm, envir = leaf_parent)
    if (!is.null(parent)) {
      parent_children[[parent]] <- c(parent_children[[parent]], nm)
    }
  }

  # Build workspace tree
  result <- list()

  # Add parent workspaces with their children
  for (parent in names(parent_children)) {
    children <- list()
    for (child in parent_children[[parent]]) {
      children[[child]] <- build_leaf(child)
    }
    result[[parent]] <- list(children = children)
  }

  # Add top-level leaves (not children of any parent)
  for (nm in active_names) {
    if (is.null(get0(nm, envir = leaf_parent))) {
      result[[nm]] <- build_leaf(nm)
    }
  }

  # Preserve disabled workspaces from the original definition
  if (!is.null(original_ws)) {
    orig_leaves <- ws_leaves(original_ws)
    for (nm in names(orig_leaves)) {
      if (isTRUE(orig_leaves[[nm]][["disabled"]]) && !(nm %in% active_names)) {
        # Find where the disabled leaf belongs (top-level or under a parent)
        parent <- NULL
        for (pnm in names(original_ws)) {
          if (is_ws_parent(original_ws[[pnm]])) {
            if (nm %in% names(original_ws[[pnm]][["children"]])) {
              parent <- pnm
              break
            }
          }
        }
        if (!is.null(parent) && !is.null(result[[parent]])) {
          result[[parent]][["children"]][[nm]] <- orig_leaves[[nm]]
        } else if (is.null(parent)) {
          result[[nm]] <- orig_leaves[[nm]]
        }
      }
    }
  }

  ws <- as_dock_workspaces(
    result,
    ws_create = attr(original_ws, "ws_create") %||% TRUE,
    ws_rename = attr(original_ws, "ws_rename") %||% TRUE,
    ws_delete = attr(original_ws, "ws_delete") %||% TRUE
  )
  attr(ws, "active_ws") <- dock$active_ws()
  ws
}

#' @export
blockr_ser.dock_layout <- function(x, data, ...) {
  list(object = class(x), payload = unclass(coal(data, x)))
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
blockr_ser.dock_workspaces <- function(x, data, ...) {
  ser_leaf <- function(ws) {
    layout <- ws[["layout"]]
    list(
      block_ids = ws[["block_ids"]],
      ext_ids = ws[["ext_ids"]],
      layout = if (!is.null(layout)) blockr_ser(layout, NULL),
      disabled = ws[["disabled"]]
    )
  }

  payload <- lapply(x, function(ws) {
    if (!is.null(ws[["children"]])) {
      list(children = lapply(ws[["children"]], ser_leaf))
    } else {
      ser_leaf(ws)
    }
  })

  list(
    object = class(x),
    payload = payload,
    ws_create = attr(x, "ws_create"),
    ws_rename = attr(x, "ws_rename"),
    ws_delete = attr(x, "ws_delete"),
    active_ws = attr(x, "active_ws")
  )
}

#' @export
blockr_deser.dock_workspaces <- function(x, data, ...) {
  deser_leaf <- function(ws) {
    ws[["block_ids"]] <- as.character(ws[["block_ids"]] %||% character())
    ws[["ext_ids"]] <- as.character(ws[["ext_ids"]] %||% character())
    ws[["disabled"]] <- isTRUE(ws[["disabled"]])
    if (!is.null(ws[["layout"]])) {
      ws[["layout"]] <- blockr_deser(ws[["layout"]])
    }
    ws
  }

  workspaces <- lapply(data[["payload"]], function(ws) {
    if (!is.null(ws[["children"]])) {
      list(children = lapply(ws[["children"]], deser_leaf))
    } else {
      deser_leaf(ws)
    }
  })

  ws <- as_dock_workspaces(
    workspaces,
    ws_create = data[["ws_create"]] %||% TRUE,
    ws_rename = data[["ws_rename"]] %||% TRUE,
    ws_delete = data[["ws_delete"]] %||% TRUE
  )
  attr(ws, "active_ws") <- data[["active_ws"]]
  ws
}

#' @export
restore_board.dock_board <- function(x, new, result, ..., meta = NULL,
                                     session = get_session()) {

  des <- blockr_deser(new)

  res <- as_dock_board(
    des,
    extensions = dock_extensions(x),
    options = board_options(x)
  )

  if (is.null(meta)) {
    result(res)
  } else {
    result(list(board = res, meta = meta))
  }
}
