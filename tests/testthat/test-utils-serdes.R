test_that("ser/des utils", {

  board1 <- new_dock_board()

  expect_identical(
    board1,
    blockr_deser(blockr_ser(board1)),
    ignore_function_env = TRUE
  )

  board2 <- new_dock_board(extensions = new_edit_board_extension())

  expect_identical(
    board2,
    blockr_deser(blockr_ser(board2)),
    ignore_function_env = TRUE
  )
})

test_that("dock_layouts serialization round-trip", {
  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(
      Tab1 = list("a", "b"),
      Tab2 = dock_layout("a", active = TRUE)
    )
  )

  ser <- blockr_ser(brd)
  des <- blockr_deser(ser)

  ly <- des[["layouts"]]
  expect_s3_class(ly, "dock_layouts")
  expect_named(ly, c("Tab1", "Tab2"))
  expect_identical(active_view(ly), "Tab2")
})

test_that("serialized dock_layout uses the flattened wire spec", {
  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(Page = list("a", "b"))
  )

  ser <- blockr_ser(brd)
  payload <- ser[["payload"]][["layouts"]][["payload"]][["views"]][[1L]][[
    "payload"
  ]]

  # Flattened: no `root` wrapper, branch fields hoisted to the top.
  expect_true("children" %in% names(payload))
  expect_false(any(c("grid", "panels", "activeGroup", "root") %in%
                     names(payload)))

  # Single-panel leaves serialize as bare strings.
  expect_identical(payload[["children"]][[1L]], "block_panel-a")
  expect_identical(payload[["children"]][[2L]], "block_panel-b")
})

test_that("grid_to_spec normalises pixel sizes to ratios", {

  # dockview returns absolute sizes from live state. Build a tree that
  # looks like what get_dock() would emit after a user resize and check
  # that the wire form carries ratios summing to 1.
  pixel_grid <- list(
    root = list(
      type = "branch",
      data = list(
        list(
          type = "leaf",
          data = list(views = list("a"), activeView = "a", id = "1"),
          size = 300
        ),
        list(
          type = "leaf",
          data = list(views = list("b"), activeView = "b", id = "2"),
          size = 700
        )
      ),
      size = 1000
    ),
    orientation = "HORIZONTAL"
  )

  wire <- grid_to_spec(pixel_grid)
  expect_equal(wire[["sizes"]], c(0.3, 0.7))
})

test_that("grid_to_spec omits even sizes and default active tabs", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(Page = list("a", c("b", "a")))
  )

  ser <- blockr_ser(brd)
  payload <- ser[["payload"]][["layouts"]][["payload"]][["views"]][[1L]][[
    "payload"
  ]]

  # Two equal-share children: sizes field omitted entirely.
  expect_false("sizes" %in% names(payload))

  # Second child is a tabbed leaf whose first panel is the default
  # active — `active` should be absent.
  tabbed <- payload[["children"]][[2L]]
  expect_setequal(unlist(tabbed[["panels"]]),
                  c("block_panel-b", "block_panel-a"))
  expect_false("active" %in% names(tabbed))
})

test_that("spec_to_grid restores group ids from the flattened spec", {

  wire <- list(
    orientation = "horizontal",
    children = list("block_panel-a", "block_panel-b")
  )

  grid <- spec_to_grid(wire)
  expect_identical(grid[["orientation"]], "HORIZONTAL")
  expect_identical(grid[["root"]][["data"]][[1L]][["data"]][["id"]], "1")
  expect_identical(grid[["root"]][["data"]][[2L]][["data"]][["id"]], "2")
})

test_that("spec_to_grid leaves branch children unnamed for char-vector input", {

  # jsonlite simplifies `["a","b"]` to a character vector; base `Map`'s
  # USE.NAMES would then name the branch children by value, diverging
  # from the constructor path that passes an (unnamed) list.
  from_vector <- spec_to_grid(
    list(orientation = "horizontal", children = c("a", "b"))
  )
  from_list <- spec_to_grid(
    list(orientation = "horizontal", children = list("a", "b"))
  )

  expect_null(names(from_vector[["root"]][["data"]]))
  expect_identical(from_vector, from_list)
})

test_that("grid_map_leaves transforms leaves and keeps the tree shape", {

  grid <- spec_to_grid(
    list(
      orientation = "horizontal",
      children = list("a", list(children = list("b", "c")))
    )
  )

  upcase_leaf <- function(leaf) {

    views <- toupper(unlist(leaf[["data"]][["views"]]))
    leaf[["data"]][["views"]] <- as.list(views)
    leaf[["data"]][["activeView"]] <- toupper(leaf[["data"]][["activeView"]])

    leaf
  }

  out <- grid_map_leaves(grid, upcase_leaf)

  expect_identical(grid_panel_ids(out), c("A", "B", "C"))
  expect_identical(out[["orientation"]], "HORIZONTAL")

  inner <- out[["root"]][["data"]][[2L]]
  expect_identical(inner[["type"]], "branch")
  expect_length(inner[["data"]], 2L)
})

test_that("grid_map_leaves prunes a leaf on NULL and keeps its siblings", {

  grid <- spec_to_grid(
    list(
      orientation = "horizontal",
      children = list("a", list(children = list("b", "c")))
    )
  )

  drop_b <- function(leaf) {
    if (identical(unlist(leaf[["data"]][["views"]]), "b")) NULL else leaf
  }

  out <- grid_map_leaves(grid, drop_b)

  expect_identical(grid_panel_ids(out), c("a", "c"))

  inner <- out[["root"]][["data"]][[2L]]
  expect_identical(inner[["type"]], "branch")
  expect_length(inner[["data"]], 1L)
})

test_that("grid_map_leaves collapses a branch whose leaves all prune", {

  grid <- spec_to_grid(
    list(
      orientation = "horizontal",
      children = list("a", list(children = list("b", "c")))
    )
  )

  drop_bc <- function(leaf) {
    keep <- !any(unlist(leaf[["data"]][["views"]]) %in% c("b", "c"))
    if (keep) leaf else NULL
  }

  out <- grid_map_leaves(grid, drop_bc)

  expect_identical(grid_panel_ids(out), "a")
  expect_length(out[["root"]][["data"]], 1L)
  expect_identical(
    out[["root"]][["data"]][[1L]][["data"]][["views"]][[1L]],
    "a"
  )
})

test_that("grid_map_leaves collapses to a NULL root when every leaf prunes", {

  grid <- spec_to_grid(
    list(
      orientation = "horizontal",
      children = list("a", list(children = list("b", "c")))
    )
  )

  prune_all <- function(leaf) NULL

  out <- grid_map_leaves(grid, prune_all)

  expect_null(out[["root"]])
  expect_identical(grid_panel_ids(out), character())
})

test_that("grid_map_leaves is a no-op on an already-empty grid", {

  grid <- list(root = NULL, orientation = "HORIZONTAL")
  out <- grid_map_leaves(grid, identity)

  expect_null(out[["root"]])
  expect_identical(out[["orientation"]], "HORIZONTAL")
})

test_that("constructor and JSON paths yield identical specs", {

  ctor <- dock_layout("a", "b")
  json <- layout_from_json(
    '{"orientation":"horizontal","children":["a","b"]}'
  )

  expect_identical(layout_panel_ids(ctor), layout_panel_ids(json))
  expect_identical(layout_to_spec(ctor), layout_to_spec(json))
  expect_null(names(layout_to_spec(json)[["children"]]))

  # Nested: a tabbed leaf alongside a single-panel leaf.
  ctor_nested <- dock_layout(c("a", "b"), "c")
  json_nested <- layout_from_json(
    '{"orientation":"horizontal","children":[{"panels":["a","b"]},"c"]}'
  )

  expect_identical(
    layout_to_spec(ctor_nested),
    layout_to_spec(json_nested)
  )
})

test_that("focus round-trips and is omitted for the default group", {

  # Two leaves; focus the second (a tabbed group). Build via spec_to_grid
  # so we can assign a non-default activeGroup the way live dockview state
  # would.
  grid <- spec_to_grid(
    list(
      orientation = "horizontal",
      children = list(
        "block_panel-a",
        list(panels = list("block_panel-b", "block_panel-c"),
             active = "block_panel-c")
      )
    )
  )

  focused <- new_dock_layout(grid = grid, active_group = "2")
  wire <- layout_to_spec(focused)
  expect_identical(wire[["focus"]], "block_panel-c")
  expect_identical(spec_to_layout(wire)[["activeGroup"]], "2")

  # Focus on the first leaf is the load default — omitted.
  default <- new_dock_layout(grid = grid, active_group = "1")
  wire_default <- layout_to_spec(default)
  expect_false("focus" %in% names(wire_default))
  expect_identical(spec_to_layout(wire_default)[["activeGroup"]], "1")
})

test_that("focus survives a real JSON round-trip", {

  grid <- spec_to_grid(
    list(
      orientation = "horizontal",
      children = list("block_panel-a", "block_panel-b")
    )
  )
  focused <- new_dock_layout(grid = grid, active_group = "2")

  json <- layout_to_json(focused)
  expect_match(json, "\"focus\":\"block_panel-b\"", fixed = TRUE)
  expect_identical(layout_from_json(json)[["activeGroup"]], "2")
})

test_that("layout survives a real JSON encode/decode round-trip", {

  # The internal ser/des tests round-trip R lists only. blockr.core
  # writes to disk via toJSON(null = "null") / fromJSON(simplifyVector =
  # TRUE), which collapses all-scalar arrays to atomic vectors. Exercise
  # that path so a boxing/simplify regression can't ship silently.
  brd <- new_dock_board(
    blocks = c(
      a = new_dataset_block(),
      b = new_head_block(),
      c = new_head_block()
    ),
    layouts = list(
      Page = dock_layout(
        "a",
        group("b", "c", sizes = c(0.4, 0.6)),
        sizes = c(0.3, 0.7)
      )
    )
  )

  ser <- blockr_ser(brd)
  json <- jsonlite::toJSON(ser, null = "null")
  back <- jsonlite::fromJSON(json, simplifyDataFrame = FALSE,
                             simplifyMatrix = FALSE)
  ly <- blockr_deser(back)[["layouts"]][["Page"]]

  expect_equal(ly$grid$root$data[[1L]]$size, 0.3)
  expect_equal(ly$grid$root$data[[2L]]$size, 0.7)
  expect_identical(ly$grid$root$data[[2L]]$type, "branch")
  expect_equal(ly$grid$root$data[[2L]]$data[[1L]]$size, 0.4)
  expect_equal(ly$grid$root$data[[2L]]$data[[2L]]$size, 0.6)
  expect_setequal(
    layout_panel_ids(ly),
    c("block_panel-a", "block_panel-b", "block_panel-c")
  )
})

test_that("JSON crosses via layout_to_json / layout_from_json", {

  ly <- dock_layout(
    "a",
    panels("b", "c", active = "c"),
    sizes = c(0.3, 0.7)
  )

  json <- layout_to_json(ly)
  expect_type(json, "character")
  expect_identical(layout_from_json(json), ly)

  # layout_from_json also tolerates an already-parsed spec list.
  parsed <- jsonlite::fromJSON(json, simplifyDataFrame = FALSE,
                               simplifyMatrix = FALSE)
  expect_identical(layout_from_json(parsed), ly)
})

test_that("spec list crosses via as.list / as_dock_layout", {

  ly <- dock_layout(
    "a",
    panels("b", "c", active = "c"),
    sizes = c(0.3, 0.7)
  )

  # as.list() of a layout is the spec; as_dock_layout() inverts it.
  spec <- as.list(ly)
  expect_true(is.list(spec) && !is_dock_layout(spec))
  expect_identical(as_dock_layout(spec), ly)
})

test_that("as_dock_layout rejects dockview's internal grid shape", {

  ly <- resolve_dock_layout(
    blocks = c(a = new_dataset_block()),
    layout = dock_layout("a")
  )

  # An unclassed dock_layout / get_dock() output is grid-shaped — not a
  # public input.
  expect_error(
    as_dock_layout(unclass(ly)),
    class = "dock_layout_dockview_input"
  )
})

test_that("layout_from_json resolves bare IDs against blocks/extensions", {

  blks <- c(a = new_dataset_block(), b = new_head_block())
  json <- '{"orientation":"horizontal","children":["a","b"]}'

  ly <- layout_from_json(json, blocks = blks)
  expect_setequal(
    layout_panel_ids(ly),
    c("block_panel-a", "block_panel-b")
  )

  # An unknown panel is rejected by the folded validation.
  expect_error(
    layout_from_json(
      '{"orientation":"horizontal","children":["a","nope"]}',
      blocks = blks
    ),
    class = "dock_layout_invalid"
  )
})

test_that("layout_panel_ids and panel_obj_ids are inverse-ish", {

  ly <- resolve_dock_layout(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layout = dock_layout("a", "b")
  )

  pids <- layout_panel_ids(ly)
  expect_setequal(pids, c("block_panel-a", "block_panel-b"))
  expect_setequal(panel_obj_ids(pids), c("a", "b"))
})

# Layout features encoded in the grid tree (sizes, activeView,
# orientation, nested group sizes) must survive ser/des untouched —
# otherwise a saved custom arrangement would degrade to defaults on
# load.
test_that("dock_layout custom sizes round-trip through ser/des", {
  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(Page = dock_layout("a", "b", sizes = c(0.3, 0.7)))
  )
  ly <- blockr_deser(blockr_ser(brd))[["layouts"]][["Page"]]
  expect_equal(ly$grid$root$data[[1L]]$size, 0.3)
  expect_equal(ly$grid$root$data[[2L]]$size, 0.7)
})

test_that("panels(active = ...) round-trips through ser/des", {
  brd <- new_dock_board(
    blocks = c(
      a = new_dataset_block(),
      b = new_head_block(),
      c = new_head_block()
    ),
    layouts = list(
      Page = dock_layout(panels("a", "b", "c", active = "b"))
    )
  )
  ly <- blockr_deser(blockr_ser(brd))[["layouts"]][["Page"]]
  expect_identical(
    ly$grid$root$data[[1L]]$data$activeView,
    "block_panel-b"
  )
})

test_that("orientation round-trips through ser/des", {
  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(
      Page = dock_layout("a", "b", orientation = "vertical")
    )
  )
  ly <- blockr_deser(blockr_ser(brd))[["layouts"]][["Page"]]
  expect_identical(ly$grid$orientation, "VERTICAL")
})

test_that("nested group() sizes round-trip through ser/des", {
  brd <- new_dock_board(
    blocks = c(
      a = new_dataset_block(),
      b = new_head_block(),
      c = new_head_block()
    ),
    layouts = list(
      Page = dock_layout(
        "a",
        group("b", "c", sizes = c(0.4, 0.6)),
        sizes = c(0.3, 0.7)
      )
    )
  )
  ly <- blockr_deser(blockr_ser(brd))[["layouts"]][["Page"]]
  outer <- ly$grid$root$data
  expect_equal(outer[[1L]]$size, 0.3)
  expect_equal(outer[[2L]]$size, 0.7)
  expect_equal(outer[[2L]]$data[[1L]]$size, 0.4)
  expect_equal(outer[[2L]]$data[[2L]]$size, 0.6)
})

# Forward compat: a payload saved by a pre-Option B build still carries
# the old wire-shape `panels` map. The new deserializer must read the
# grid + activeGroup and silently drop the stale panels (which would be
# re-derived from blocks at restore time anyway).
test_that("legacy dock_layout payload with panels still loads", {

  legacy_payload <- list(
    object = "dock_layout",
    payload = list(
      grid = list(
        root = list(
          type = "branch",
          data = list(
            list(
              type = "leaf",
              data = list(
                views = list("block_panel-a"),
                activeView = "block_panel-a",
                id = "1"
              ),
              size = 0.5
            )
          ),
          size = 1
        ),
        orientation = "HORIZONTAL"
      ),
      panels = list(
        `block_panel-a` = list(
          id = "block_panel-a",
          title = "Stale title from save time"
        )
      ),
      activeGroup = "1"
    )
  )

  ly <- blockr_deser(legacy_payload)
  expect_s3_class(ly, "dock_layout")
  expect_false("panels" %in% names(ly))
  expect_identical(layout_panel_ids(ly), "block_panel-a")
  expect_identical(ly$grid$orientation, "HORIZONTAL")
  expect_identical(ly$activeGroup, "1")
})
