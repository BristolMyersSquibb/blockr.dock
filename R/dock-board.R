#' Dock board
#'
#' Using the docking layout manager provided by dockViewR, a `dock_board`
#' extends [blockr.core::new_board()]. In addition to the attributes contained
#' in a core board, this also includes dock extensions (as `extensions`)
#' and the panel arrangement (as `layout`). The `layout` is always stored
#' internally as a [dock_layouts()] object (multi-view); single-page boards
#' are a degenerate case with one auto-named "Page" view.
#'
#' Dispatch is type-driven: a `dock_layouts` is used as-is, a `dock_layout`
#' is wrapped via `as_dock_layouts()`, and a plain list (raw grid spec) is
#' resolved via `create_dock_layout()` and then wrapped.
#'
#' @inheritParams blockr.core::new_board
#' @param extensions Dock extensions
#' @param layout A `dock_layouts()` object, a `dock_layout`, or a raw
#'   grid specification (list). All forms are normalised to `dock_layouts`.
#'
#' @examples
#' brd <- new_dock_board(c(a = blockr.core::new_dataset_block()))
#' str(dock_layout(brd), max.level = 2)
#'
#' @return The constructor `new_dock_board()` returns a `board` object, as does
#' the coercion function `as_dock_board()`. Inheritance can be checked using
#' `is_dock_board()`, which returns a boolean. `board_views()` returns the
#' board's `dock_layouts`; `dock_layout()` returns the active view's resolved
#' `dock_layout` and `dock_layout<-()` writes into the active view. The
#' `dock_extensions()` and `dock_extensions<-()` accessors return / set the
#' board's `dock_extension` objects. A character vector of IDs is returned by
#' `dock_ext_ids()` and `dock_board_options()` returns a `board_options`
#' object.
#'
#' @rdname dock
#' @export
new_dock_board <- function(blocks = list(), links = list(), stacks = list(),
                           ..., extensions = new_dock_extensions(),
                           layout = dock_layouts(
                             Page = default_layout(blocks, extensions)
                           ),
                           options = dock_board_options(),
                           ctor = NULL, pkg = NULL, class = character()) {

  layout <- initialise_layout(layout, blocks, extensions)

  new_board(
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
}

# Normalise the user-provided `layout` into a `dock_layouts` object whose
# views are all resolved `dock_layout` objects. Dispatch is type-driven:
#   - dock_layouts: resolve any raw view specs into dock_layout objects
#   - dock_layout : wrap into a single-view dock_layouts (active = TRUE)
#   - raw list / grid spec: build a dock_layout via create_dock_layout(),
#                           then wrap as above
initialise_layout <- function(layout, blocks, extensions) {

  if (!is_dock_layouts(layout) && !is_dock_layout(layout)) {

    # Either a pre-resolved layout payload (with grid+panels keys) from
    # deserialization, or a raw grid spec.
    if (is.list(layout) && all(c("grid", "panels") %in% names(layout))) {
      layout <- as_dock_layout(layout)
    } else {
      layout <- create_dock_layout(blocks, extensions, layout)
    }
  }

  if (is_dock_layout(layout)) {
    layout <- as_dock_layouts(layout)
  }

  # layout is now dock_layouts; resolve each view's raw spec into a
  # dock_layout, preserving the active attribute across coercion.
  c_exts <- as_dock_extensions(extensions)
  c_blks <- as_blocks(blocks)

  for (view_name in names(layout)) {
    ly <- layout[[view_name]]
    was_active <- is_active_view(ly)

    if (!is_dock_layout(ly)) {
      if (is.list(ly) && all(c("grid", "panels") %in% names(ly))) {
        # Already a resolved layout (e.g. deserialized from JSON) --
        # just restore the S3 class
        layout[[view_name]] <- as_dock_layout(ly)
      } else {
        v_ids <- view_ids(ly)
        v_blks <- c_blks[intersect(v_ids, names(c_blks))]
        ext_list <- as.list(c_exts)
        v_exts <- as_dock_extensions(
          ext_list[intersect(v_ids, names(ext_list))]
        )
        layout[[view_name]] <- create_dock_layout(v_blks, v_exts, ly)
      }
    }

    if (was_active) {
      layout[[view_name]] <- mark_active(layout[[view_name]])
    }
  }

  layout
}

#' @export
validate_board.dock_board <- function(x) {

  x <- NextMethod()

  validate_dock_layouts(x[["layout"]])
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
  validate_dock_layout(ly[[active_view(ly)]], board_block_ids(x))
}

#' @param value Replacement value
#' @rdname dock
#' @export
`dock_layout<-` <- function(x, value) {
  stopifnot(is_dock_board(x))

  ly <- x[["layout"]]
  view_name <- active_view(ly)
  ly[[view_name]] <- mark_active(
    validate_dock_layout(value, board_block_ids(x))
  )
  x[["layout"]] <- ly

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

#' @export
rm_blocks.dock_board <- function(x, rm, ...) {

  # for now, b/c dock$layout(), passed in ... is not updated in time, we can
  # only clear the layout to not run into validation issues
  dock_layout(x) <- create_dock_layout(extensions = dock_extensions(x))

  NextMethod(object = x)
}
