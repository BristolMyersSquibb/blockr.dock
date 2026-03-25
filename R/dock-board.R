#' Dock board
#'
#' Using the docking layout manager provided by dockViewR, a `dock_board`
#' extends [blockr.core::new_board()]. In addition to the attributes contained
#' in a core board, this also includes dock extensions (as `extensions`)
#' and the panel arrangement (as `layout`).
#'
#' @inheritParams blockr.core::new_board
#' @param extensions Dock extensions
#' @param layout Dock layout
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
#' object (invisibly). A character vector of IDs is returned by `dock_ext_ids()`
#' and `dock_board_options()` returns a `board_options` object.
#'
#' @rdname dock
#' @export
new_dock_board <- function(blocks = list(), links = list(), stacks = list(),
                           ..., extensions = new_dock_extensions(),
                           layout = default_grid(blocks, extensions),
                           options = dock_board_options(),
                           workspaces = NULL,
                           ws_create = TRUE, ws_rename = TRUE,
                           ws_delete = TRUE,
                           ctor = NULL, pkg = NULL, class = character()) {

  if (!is.null(workspaces)) {
    # Preserve flags from already-classed workspaces (e.g. from deserialization)
    if (inherits(workspaces, "dock_workspaces")) {
      ws_create <- attr(workspaces, "ws_create") %||% ws_create
      ws_rename <- attr(workspaces, "ws_rename") %||% ws_rename
      ws_delete <- attr(workspaces, "ws_delete") %||% ws_delete
    }
    workspaces <- validate_workspaces(workspaces, blocks, extensions)
    workspaces <- as_dock_workspaces(workspaces, ws_create, ws_rename, ws_delete)
  }

  if (!is_dock_layout(layout)) {
    layout <- create_dock_layout(blocks, extensions, layout)
  }

  board_args <- list(
    blocks = as_blocks(blocks),
    links = as_links(links),
    stacks = as_dock_stacks(stacks),
    ...,
    extensions = as_dock_extensions(extensions),
    layout = layout,
    options = as_board_options(options),
    ctor = forward_ctor(ctor),
    pkg = pkg,
    class = c(class, "dock_board")
  )

  if (!is.null(workspaces)) {
    board_args[["workspaces"]] <- workspaces
  }

  do.call(new_board, board_args)
}

#' @export
validate_board.dock_board <- function(x) {

  x <- NextMethod()

  validate_dock_layout(x[["layout"]], board_block_ids(x))
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
  validate_dock_layout(x[["layout"]], board_block_ids(x))
}

#' @param value Replacement value
#' @rdname dock
#' @export
`dock_layout<-` <- function(x, value) {
  stopifnot(is_dock_board(x))
  x[["layout"]] <- validate_dock_layout(value, board_block_ids(x))
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
dock_board_options <- function() {
  new_board_options(
    new_board_name_option()
  )
}

#' @rdname dock
#' @export
dock_workspaces <- function(x) {
  stopifnot(is_dock_board(x))
  x[["workspaces"]]
}

#' @rdname dock
#' @export
has_workspaces <- function(x) {
  !is.null(dock_workspaces(x))
}

as_dock_workspaces <- function(x, ws_create = TRUE, ws_rename = TRUE,
                               ws_delete = TRUE) {
  attr(x, "ws_create") <- isTRUE(ws_create)
  attr(x, "ws_rename") <- isTRUE(ws_rename)
  attr(x, "ws_delete") <- isTRUE(ws_delete)
  structure(x, class = "dock_workspaces")
}

ws_can_create <- function(x) {
  isTRUE(attr(dock_workspaces(x), "ws_create") %||% TRUE)
}

ws_can_rename <- function(x) {
  isTRUE(attr(dock_workspaces(x), "ws_rename") %||% TRUE)
}

ws_can_delete <- function(x) {
  isTRUE(attr(dock_workspaces(x), "ws_delete") %||% TRUE)
}

validate_workspaces <- function(workspaces, blocks, extensions) {

  stopifnot(
    is.list(workspaces),
    !is.null(names(workspaces)),
    length(workspaces) >= 1L,
    all(nzchar(names(workspaces)))
  )

  all_blocks <- as_blocks(blocks)
  all_exts <- as.list(as_dock_extensions(extensions))

  blk_ids <- names(all_blocks)
  ext_ids <- names(all_exts)

  validate_ws_entry <- function(ws, nm) {
    stopifnot(is.list(ws))

    ws_blk_ids <- ws[["block_ids"]] %||% character()
    ws_ext_ids <- ws[["ext_ids"]] %||% ext_ids

    stopifnot(
      is.character(ws_blk_ids),
      is.character(ws_ext_ids),
      all(ws_blk_ids %in% blk_ids),
      all(ws_ext_ids %in% ext_ids)
    )

    ws[["ext_ids"]] <- ws_ext_ids
    ws[["disabled"]] <- isTRUE(ws[["disabled"]])

    ws_blocks <- all_blocks[ws_blk_ids]
    ws_exts <- new_dock_extensions(all_exts[ws_ext_ids])

    if (is.null(ws[["layout"]])) {
      ws[["layout"]] <- default_grid(ws_blocks, ws_exts)
    }

    if (!is_dock_layout(ws[["layout"]])) {
      ws[["layout"]] <- create_dock_layout(ws_blocks, ws_exts, ws[["layout"]])
    }

    ws
  }

  for (nm in names(workspaces)) {
    ws <- workspaces[[nm]]
    stopifnot(is.list(ws))

    if (!is.null(ws[["children"]])) {
      # Parent workspace: validate children, parent has no DockView
      children <- ws[["children"]]
      stopifnot(
        is.list(children),
        !is.null(names(children)),
        length(children) >= 1L,
        all(nzchar(names(children)))
      )
      for (cnm in names(children)) {
        children[[cnm]] <- validate_ws_entry(children[[cnm]], cnm)
      }
      workspaces[[nm]][["children"]] <- children
    } else {
      # Leaf workspace: validate directly
      workspaces[[nm]] <- validate_ws_entry(ws, nm)
    }
  }

  # Ensure all leaf names are unique
  leaf_names <- ws_leaf_names(workspaces)
  if (anyDuplicated(leaf_names)) {
    blockr_abort(
      "Workspace leaf names must be unique. Duplicated: {leaf_names[duplicated(leaf_names)]}.",
      class = "duplicate_workspace_names"
    )
  }

  workspaces
}

#' Get all leaf workspace names
#' @noRd
ws_leaf_names <- function(workspaces) {
  nms <- character()
  for (nm in names(workspaces)) {
    ws <- workspaces[[nm]]
    if (!is.null(ws[["children"]])) {
      nms <- c(nms, names(ws[["children"]]))
    } else {
      nms <- c(nms, nm)
    }
  }
  nms
}

#' Get flat list of leaf workspaces (name -> spec)
#' @noRd
ws_leaves <- function(workspaces) {
  leaves <- list()
  for (nm in names(workspaces)) {
    ws <- workspaces[[nm]]
    if (!is.null(ws[["children"]])) {
      for (cnm in names(ws[["children"]])) {
        leaves[[cnm]] <- ws[["children"]][[cnm]]
      }
    } else {
      leaves[[nm]] <- ws
    }
  }
  leaves
}

#' Check if a workspace entry is a parent (has children)
#' @noRd
is_ws_parent <- function(ws) {
  !is.null(ws[["children"]])
}

#' @export
rm_blocks.dock_board <- function(x, rm, ...) {

  # for now, b/c dock$layout(), passed in ... is not updated in time, we can
  # only clear the layout to not run into validation issues
  dock_layout(x) <- create_dock_layout(extensions = dock_extensions(x))

  NextMethod(object = x)
}
