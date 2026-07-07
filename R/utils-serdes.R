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

  # Geometry is client-owned until #294's mirror, so read the live layout for
  # grid freshness and split it into the board's two slots. When the client
  # has not reported yet, fall back to the stored slots (NULL data).
  live <- view_data()

  layout_parts <- if (is.null(live)) {
    list(views = NULL, grids = NULL)
  } else {
    decompose_layouts(live)
  }

  do.call(
    blockr_ser,
    c(
      list(
        x,
        board_id = id,
        blocks = Map(c, state, visible = lapply(visibility, list)),
        options = opts,
        views = layout_parts[["views"]],
        grids = layout_parts[["grids"]],
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
    payload = layout_to_spec(payload),
    name = view_name(payload)
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
    payload = lapply(Filter(Negate(is.null), as.list(arr)), layout_to_spec)
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

  # The split structure / grid slots recombine into the constructor's
  # `layouts` argument, which re-decomposes them. Pre-split payloads carry a
  # fused `layouts` field instead.
  layouts <- if (not_null(des[["views"]])) {
    compose_layouts(des[["views"]], des[["grids"]])
  } else {
    des[["layouts"]]
  }

  args <- c(
    des[setdiff(names(des), c("views", "grids", "layouts"))],
    list(
      layouts = layouts,
      ctor = coal(ctor_name(ctor), ctor_fun(ctor)),
      pkg = ctor_pkg(ctor)
    )
  )

  res <- do.call(ctor_fun(ctor), args)

  attr(res, "id") <- data[["id"]]

  res
}

# Pick by highest applicable `since`, not list position, so the registry
# needn't be kept in order.
layout_reader_for <- function(version, formats) {

  since <- numeric_version(chr_xtr(formats, "since"))
  applicable <- which(since <= version)

  if (!length(applicable)) {
    return(NULL)
  }

  newest <- applicable[order(since[applicable], decreasing = TRUE)[1L]]

  formats[[newest]][["read"]]
}

# Route by producer version, falling back to shape discrimination when it
# is missing or predates the registry.
read_layout_payload <- function(payload, producer_version = NULL) {

  formats <- list(
    list(since = "0.1.2", read = spec_to_layout)
  )

  version <- if (is_string(producer_version)) {
    numeric_version(producer_version, strict = FALSE)
  }

  if (length(version) && !is.na(version)) {
    reader <- layout_reader_for(version, formats)
    if (!is.null(reader)) {
      return(reader(payload))
    }
  }

  if ("grid" %in% names(payload)) {
    dockview_to_layout(payload)
  } else {
    spec_to_layout(payload)
  }
}

#' @export
blockr_deser.dock_layout <- function(x, data, ..., producer_version = NULL) {

  layout <- read_layout_payload(data[["payload"]], producer_version)

  # Which view is active is restored at the container level from
  # `dock_layouts`' `active` field; a per-view `active` boolean in older
  # payloads is ignored here.
  if (is_string(data[["name"]])) {
    view_name(layout) <- data[["name"]]
  }

  layout
}

#' @export
blockr_deser.dock_layouts <- function(x, data, ...) {
  payload <- data[["payload"]]

  # `views` is keyed by stable id; each carries its display name (when
  # set). The active view is recorded once, at the container level, and
  # restored by id below.
  res <- reconstruct_dock_layouts(
    lapply(payload[["views"]], blockr_deser, ...)
  )

  if (is_string(payload[["active"]]) && payload[["active"]] %in% names(res)) {
    active_view(res) <- payload[["active"]]
  }

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
blockr_deser.dock_grids <- function(x, data, ..., producer_version = NULL) {
  new_dock_grids(
    lapply(data[["payload"]], read_layout_payload,
           producer_version = producer_version)
  )
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
# Spec converters (our serialized format <-> dockview's internal grid)
#
# The persisted layout shape (the "spec") is a recursive tree,
# deliberately distinct from dockview's internal grid format. The
# top-level object is itself a
# branch — it carries `children`, an optional `sizes`, and an
# `orientation`. A node within is either:
#
#   - a bare string: a single-panel leaf (the common case);
#   - an object with `panels` (and optional `active`): a tabbed leaf;
#   - an object with `children` (and optional `sizes`): a nested branch.
#
# Bare arrays never appear as node values, so the three forms are
# syntactically distinct. Sizes are ratios summing to 1, omitted when
# the split is even. The active tab is omitted when the open tab is the
# first panel (the implicit default).
#
# A top-level `focus` names the panel with current focus (the open tab
# of the focused group). It's omitted when focus is on the first leaf
# (the default on load). It's distinct from a leaf's `active`: `active`
# is leaf-local (which tab is open here), `focus` is layout-global
# (which panel is focused overall). grid_to_spec()/spec_to_grid() handle
# the pure structure; layout_to_spec()/spec_to_layout() add the focus.
#
# Dockview's internal fields (type tags, per-branch size, leaf id) are
# not stored — they're regenerated on the way back in. Dockview emits
# absolute pixel sizes from live state after a user resize; grid_to_spec()
# normalises them to ratios on save. spec_to_grid() is robust to the
# atomic-vector coercion `jsonlite::fromJSON(simplifyVector = TRUE)`
# applies to all-scalar arrays.

# A whole-tree fold rather than a leaf map: branches become wire
# `children`/`sizes` and leaves collapse to strings or `panels`, so the
# leaf-only grid_map_leaves() can't express it — it keeps its own walk.
grid_to_spec <- function(grid) {

  walk <- function(node) {

    if (identical(node[["type"]], "leaf")) {
      views <- unlist(node[["data"]][["views"]])

      if (length(views) == 1L) {
        return(views[[1L]])
      }

      out <- list(panels = as.list(views))

      active <- node[["data"]][["activeView"]]
      if (!identical(active, views[[1L]])) {
        out[["active"]] <- active
      }

      return(out)
    }

    if (identical(node[["type"]], "branch")) {
      children <- lapply(node[["data"]], walk)

      raw_sizes <- dbl_xtr(node[["data"]], "size")
      norm_sizes <- normalise_sizes(raw_sizes)

      out <- list(children = children)

      if (length(norm_sizes) && !sizes_are_even(norm_sizes)) {
        out[["sizes"]] <- norm_sizes
      }

      return(out)
    }

    blockr_abort(
      "Unknown grid node type: {node[['type']]}.",
      class = "dock_layout_wire_invalid"
    )
  }

  # The root is always a branch (the constructor wraps args in a group),
  # so its `children` / `sizes` hoist to the top alongside orientation.
  root_wire <- walk(grid[["root"]])

  c(
    list(orientation = tolower(coal(grid[["orientation"]], "horizontal"))),
    root_wire
  )
}

spec_to_grid <- function(wire) {

  group_id <- 0L

  next_id <- function() {
    group_id <<- group_id + 1L
    as.character(group_id)
  }

  leaf <- function(views, active, size) {
    list(
      type = "leaf",
      data = list(views = views, activeView = active, id = next_id()),
      size = size
    )
  }

  walk <- function(node, size = 1) {

    # Bare string (possibly a length-1 vector post-fromJSON): single leaf.
    if (is.character(node)) {
      view <- node[[1L]]
      return(leaf(list(view), view, size))
    }

    if (!is.null(node[["panels"]])) {
      views <- as.list(unlist(node[["panels"]]))
      active <- coal(node[["active"]], views[[1L]])
      return(leaf(views, active, size))
    }

    if (!is.null(node[["children"]])) {
      n <- length(node[["children"]])
      child_sizes <- coal(
        node[["sizes"]],
        if (n) rep(1 / n, n) else numeric()
      )
      children <- map(walk, node[["children"]], child_sizes)

      return(
        list(
          type = "branch",
          data = filter_empty(children),
          size = size
        )
      )
    }

    blockr_abort(
      "Wire node must be a string or an object with `panels` or `children`.",
      class = "dock_layout_wire_invalid"
    )
  }

  # The top object is the root branch (children + optional sizes) plus
  # orientation; reassemble it before walking.
  root_spec <- list(children = coal(wire[["children"]], list()))
  if (!is.null(wire[["sizes"]])) {
    root_spec[["sizes"]] <- wire[["sizes"]]
  }

  list(
    root = walk(root_spec),
    orientation = toupper(coal(wire[["orientation"]], "horizontal"))
  )
}

normalise_sizes <- function(sizes) {

  if (!length(sizes)) {
    return(numeric())
  }

  total <- sum(sizes)
  if (total > 0) sizes / total else sizes
}

# Layout-level conversion: the grid <-> wire structure plus the
# focused-panel pointer that lives on the `dock_layout`, not the grid.

layout_to_spec <- function(layout) {

  layout <- as_dock_layout(layout)
  wire <- grid_to_spec(layout[["grid"]])

  focus <- focus_panel(layout[["grid"]], layout[["activeGroup"]])
  if (!is.null(focus)) {
    wire[["focus"]] <- focus
  }

  wire
}

spec_to_layout <- function(wire) {

  grid <- spec_to_grid(wire)

  new_dock_layout(
    grid = grid,
    active_group = focus_group_id(grid, wire[["focus"]])
  )
}

# Wrap a dockview-shaped list (its own `get_dock()` output, or an
# unclassed `dock_layout`) back into a `dock_layout`. Internal only —
# dockview's grid shape is not a public input; `as_dock_layout()` speaks
# the spec, not this.
dockview_to_layout <- function(x) {
  new_dock_layout(
    grid = x[["grid"]],
    active_group = coal(
      x[["activeGroup"]], x[["active_group"]],
      fail_all = FALSE
    )
  )
}

grid_leaves <- function(grid) {

  leaves <- list()

  walk <- function(node) {
    if (is.null(node) || !length(node)) {
      return(invisible())
    }
    if (identical(node[["type"]], "leaf")) {
      leaves[[length(leaves) + 1L]] <<- node[["data"]]
    } else {
      lapply(node[["data"]], walk)
    }
    invisible()
  }

  walk(grid[["root"]])
  leaves
}

# Map counterpart to grid_leaves(): rebuild the grid, applying `fn` to
# each leaf node. `fn` returns a replacement leaf (to transform it) or
# NULL (to prune it); a branch whose children all prune away collapses to
# NULL too, so empty leaves and branches never survive. Returns the grid
# with its root rebuilt (a NULL root when everything prunes).
grid_map_leaves <- function(grid, fn) {

  walk <- function(node) {

    if (is.null(node) || !length(node)) {
      return(NULL)
    }

    if (identical(node[["type"]], "leaf")) {
      return(fn(node))
    }

    children <- Filter(Negate(is.null), lapply(node[["data"]], walk))

    if (!length(children)) {
      return(NULL)
    }

    node[["data"]] <- children
    node
  }

  grid[["root"]] <- walk(grid[["root"]])
  grid
}

# Serialize side: translate the focused group id to its open panel.
# Omitted when focus is on the first leaf ("1"), which is the load default.
focus_panel <- function(grid, active_group) {

  if (is.null(active_group) || identical(active_group, "1")) {
    return(NULL)
  }

  for (leaf in grid_leaves(grid)) {
    if (identical(leaf[["id"]], active_group)) {
      return(leaf[["activeView"]])
    }
  }

  NULL
}

# Deserialize side: find the group id whose panels include the focused
# panel, so it can be restored as the active group.
focus_group_id <- function(grid, focus) {

  if (is.null(focus)) {
    return(NULL)
  }

  for (leaf in grid_leaves(grid)) {
    if (focus %in% unlist(leaf[["views"]])) {
      return(leaf[["id"]])
    }
  }

  NULL
}

#' Layout serialization and inspection
#'
#' Read and write the JSON form of a [dock_layout][layout], and inspect
#' the panel IDs it references. These are the canonical accessors for the
#' serialized layout format — downstream tooling should call them rather
#' than re-implement the format.
#'
#' `layout_to_json()` renders a layout as a JSON string; `layout_from_json()`
#' is the inverse. The shape is a recursive tree: the top object carries
#' `orientation`, `children`, an optional `sizes`, and an optional `focus`
#' (the panel with current focus); a child is either a bare string
#' (single-panel leaf), an object with `panels` / optional `active`
#' (tabbed leaf), or an object with `children` / optional `sizes` (nested
#' branch). Sizes are ratios summing to 1, omitted when even.
#'
#' `layout_from_json()` accepts a JSON string or an already-parsed spec
#' list and delegates to [as_dock_layout()][layout]; when `blocks` /
#' `extensions` are supplied, bare IDs are resolved to canonical panel
#' IDs and the result is validated (an unknown panel or malformed
#' arrangement throws the usual classed error).
#'
#' `layout_panel_ids()` returns the canonical panel IDs
#' (`block_panel-…` / `ext_panel-…`) referenced by a layout;
#' `panel_obj_ids()` strips those prefixes back to bare block /
#' extension IDs.
#'
#' @param x A `dock_layout` (for `layout_to_json()`), or a JSON string /
#'   parsed spec list (for `layout_from_json()`).
#' @param layout A `dock_layout` object.
#' @param ids Character vector of panel IDs.
#' @param blocks,extensions Optional board components used to resolve and
#'   validate bare IDs in `layout_from_json()`.
#' @param ... Forwarded to [jsonlite::toJSON()].
#'
#' @return `layout_to_json()` returns a JSON string; `layout_from_json()`
#'   a `dock_layout`. `layout_panel_ids()` and `panel_obj_ids()` return
#'   character vectors.
#'
#' @examples
#' ly <- dock_layout("a", panels("b", "c", active = "c"), sizes = c(0.3, 0.7))
#'
#' json <- layout_to_json(ly)
#' cat(json)
#'
#' identical(layout_from_json(json), ly)
#'
#' @rdname layout-json
#' @export
layout_to_json <- function(x, ...) {
  jsonlite::toJSON(layout_to_spec(x), auto_unbox = TRUE, null = "null", ...)
}

#' @rdname layout-json
#' @export
layout_from_json <- function(x, blocks = NULL, extensions = NULL) {

  spec <- if (is.character(x)) {
    jsonlite::fromJSON(x, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
  } else {
    x
  }

  as_dock_layout(spec, blocks = blocks, extensions = extensions)
}

sizes_are_even <- function(sizes) {

  n <- length(sizes)
  if (n < 2L) {
    return(TRUE)
  }

  isTRUE(all.equal(sizes, rep(1 / n, n)))
}
