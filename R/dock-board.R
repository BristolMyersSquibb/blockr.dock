#' Dock board
#'
#' Using the docking layout manager provided by dockViewR, a `dock_board`
#' extends [blockr.core::new_board()]. In addition to the attributes contained
#' in a core board, this also includes dock extensions (as `extensions`)
#' and the panel arrangement (as `layout`). The `layout` parameter accepts
#' either a grid specification (as before) or a [dock_workspaces()] object
#' for multi-workspace boards.
#'
#' @inheritParams blockr.core::new_board
#' @param extensions Dock extensions
#' @param layout Either a grid specification (list), a `dock_layout` or a
#'   [dock_workspaces()] object
#'
#' @examples
#' brd <- new_dock_board(c(a = blockr.core::new_dataset_block()))
#' str(dock_layout(brd), max.level = 2)
#'
#' @return The constructor `new_dock_board()` returns a `board` object, as does
#' the coercion function `as_dock_board()`. Inheritance can be checked using
#' `is_dock_board()`, which returns a boolean. Getters `dock_layout()` and
#' `dock_extensions()` return `dock_layout` and `dock_extension` objects while
#' setters `dock_layout<-()` and `dock_extensions<-()` return the updated board
#' object (invisibly). When `layout` is a `dock_workspaces` object,
#' `board_workspaces()` returns it and `dock_layout()` returns the active
#' workspace's layout. A character vector of IDs is returned by
#' `dock_ext_ids()` and `dock_board_options()` returns a `board_options`
#' object.
#'
#' @rdname dock
#' @export
new_dock_board <- function(blocks = list(), links = list(), stacks = list(),
                           ..., extensions = new_dock_extensions(),
                           layout = default_grid(blocks, extensions),
                           options = dock_board_options(),
                           ctor = NULL, pkg = NULL, class = character()) {

  extensions <- as_dock_extensions(extensions)
  blocks <- as_blocks(blocks)

  layout <- initialise_layout(layout, blocks, extensions)

  new_board(
    blocks = blocks,
    links = as_links(links),
    stacks = as_dock_stacks(stacks),
    ...,
    extensions = extensions,
    layout = layout,
    options = as_board_options(options),
    ctor = forward_ctor(ctor),
    pkg = pkg,
    class = c(class, "dock_board")
  )
}

# When layout is a dock_workspaces object, resolve each workspace's raw grid
# into a dock_layout. When layout is a raw grid, convert to dock_layout.
# Returns the resolved layout (dock_workspaces or dock_layout).
initialise_layout <- function(layout, blocks, extensions) {

  if (is_dock_workspaces(layout)) {

    for (ws_name in names(layout)) {
      ws <- layout[[ws_name]]
      ly <- workspace_layout(ws)

      if (!is_dock_layout(ly)) {
        if (is.list(ly) && all(c("grid", "panels") %in% names(ly))) {
          # Already a resolved layout (e.g. deserialized from JSON) —
          # just restore the S3 class
          workspace_layout(ws) <- as_dock_layout(ly)
        } else {
          ws_ids <- workspace_ids(ws)
          ws_blks <- blocks[intersect(ws_ids, names(blocks))]
          ext_list <- as.list(extensions)
          ws_exts <- as_dock_extensions(
            ext_list[intersect(ws_ids, names(ext_list))]
          )
          workspace_layout(ws) <- create_dock_layout(ws_blks, ws_exts, ly)
        }
        layout[[ws_name]] <- ws
      }
    }

    layout

  } else if (!is_dock_layout(layout)) {

    if (is.list(layout) && all(c("grid", "panels") %in% names(layout))) {
      as_dock_layout(layout)
    } else {
      create_dock_layout(blocks, extensions, layout)
    }

  } else {

    layout
  }
}

#' @export
validate_board.dock_board <- function(x) {

  x <- NextMethod()

  ly <- x[["layout"]]

  if (is_dock_workspaces(ly)) {
    validate_dock_workspaces(ly)
  } else {
    validate_dock_layout(ly, board_block_ids(x))
  }

  validate_extensions(x[["extensions"]])

  x
}

#' @param x Board object
#' @rdname dock
#' @export
is_dock_board <- function(x) {
  inherits(x, "dock_board")
}

#' @rdname dock
#' @export
as_dock_board <- function(x, ...) {
  UseMethod("as_dock_board")
}

#' @export
as_dock_board.dock_board <- function(x, ...) {
  x
}

#' @export
as_dock_board.board <- function(x, ...) {
  new_dock_board(
    board_blocks(x),
    board_links(x),
    board_stacks(x),
    ...
  )
}

#' @rdname dock
#' @export
dock_layout <- function(x) {
  stopifnot(is_dock_board(x))

  ly <- x[["layout"]]

  if (is_dock_workspaces(ly)) {
    ws <- ly[[active_workspace(ly)]]
    return(validate_dock_layout(workspace_layout(ws), board_block_ids(x)))
  }

  validate_dock_layout(ly, board_block_ids(x))
}

#' @param value Replacement value
#' @rdname dock
#' @export
`dock_layout<-` <- function(x, value) {
  stopifnot(is_dock_board(x))

  ly <- x[["layout"]]

  if (is_dock_workspaces(ly)) {
    ws_name <- active_workspace(ly)
    ws <- ly[[ws_name]]
    workspace_layout(ws) <- validate_dock_layout(value, board_block_ids(x))
    ly[[ws_name]] <- ws
    x[["layout"]] <- ly
  } else {
    x[["layout"]] <- validate_dock_layout(value, board_block_ids(x))
  }

  invisible(x)
}

#' @rdname dock
#' @export
dock_extensions <- function(x) {
  stopifnot(is_dock_board(x))
  validate_extensions(x[["extensions"]])
}

#' @param value Replacement value
#' @rdname dock
#' @export
`dock_extensions<-` <- function(x, value) {
  stopifnot(is_dock_board(x))
  x[["extensions"]] <- validate_extensions(value)
  invisible(x)
}

#' @rdname dock
#' @export
dock_ext_ids <- function(x) {
  chr_ply(dock_extensions(x), extension_id)
}

#' @rdname dock
#' @export
board_workspaces <- function(x) {
  stopifnot(is_dock_board(x))

  ly <- x[["layout"]]

  if (is_dock_workspaces(ly)) {
    ly
  } else {
    NULL
  }
}

#' @rdname dock
#' @export
dock_board_options <- function() {
  new_board_options(
    new_board_name_option()
  )
}

#' @export
rm_blocks.dock_board <- function(x, rm, ...) {

  # for now, b/c dock$layout(), passed in ... is not updated in time, we can
  # only clear the layout to not run into validation issues
  dock_layout(x) <- create_dock_layout(extensions = dock_extensions(x))

  NextMethod(object = x)
}
