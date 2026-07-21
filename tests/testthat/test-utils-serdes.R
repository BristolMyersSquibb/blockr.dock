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

# A dock board's option values -- board-owned (board_name) and block-
# contributed (page_size, baked on by board_server's effective option set)
# alike -- must come from the saved board, never leak in from the pre-restore
# board. Restore deserializes the saved board and returns it as-is.
test_that("restoring a dock board keeps the saved board-option values", {

  saved <- new_dock_board(blocks = c(a = new_dataset_block()))
  board_options(saved) <- new_board_options(
    new_board_name_option(value = "Saved"),
    new_page_size_option(value = 25L)
  )

  current <- new_dock_board(blocks = c(a = new_dataset_block()))
  board_options(current) <- new_board_options(
    new_board_name_option(value = "Current"),
    new_page_size_option(value = 99L)
  )

  restored <- NULL
  restore_board(current, blockr_ser(saved), function(x) restored <<- x)

  opts <- board_options(restored)
  expect_identical(board_option_value(opts[["board_name"]]), "Saved")
  expect_identical(board_option_value(opts[["page_size"]]), 25L)
})

test_that("views round-trip through serialization", {
  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    views = list(Tab1 = c("a", "b"), Tab2 = "a"),
    active = "Tab2"
  )

  des <- blockr_deser(blockr_ser(brd))

  views <- board_views(des)
  expect_s3_class(views, "dock_views")
  expect_identical(unname(view_names(views)), c("Tab1", "Tab2"))
  expect_identical(active_name(views), "Tab2")
})

test_that("a multi-view board round-trips identically through ser/des", {

  brd <- new_dock_board(
    blocks = c(
      a = new_dataset_block(),
      b = new_head_block(),
      c = new_head_block()
    ),
    views = list(
      analysis = dock_view(c("a", "b", "c"), name = "Analysis"),
      overview = dock_view("a", name = "Overview")
    ),
    grids = list(
      analysis = dock_grid(
        "a",
        panels("b", "c", active = "c"),
        sizes = c(0.3, 0.7)
      )
    ),
    active = "overview"
  )

  des <- blockr_deser(blockr_ser(brd))

  # ids (keys), display names, the active marker and every view's membership
  # and geometry survive a full serialize / deserialize cycle.
  expect_identical(board_views(des), board_views(brd))
  expect_identical(board_grids(des), board_grids(brd))
})

test_that("dock_views preserve a non-alphabetical key order through ser/des", {

  # View order is implicit in the list's key sequence -- no separate slot -- so
  # it survives save / restore only by JSON key ordering. Pin that with ids in
  # reverse-alphabetical order (and a non-first active): a layer that sorted
  # keys would reorder these and fail.
  brd <- new_dock_board(
    blocks = c(
      a = new_dataset_block(),
      b = new_head_block(),
      c = new_head_block()
    ),
    views = list(zebra = "a", mango = "b", apple = "c"),
    active = "mango"
  )

  des <- blockr_deser(blockr_ser(brd))

  expect_identical(names(board_views(des)), c("zebra", "mango", "apple"))
  expect_identical(active_view(board_views(des)), "mango")
})

test_that("serialized dock_views records view id, name and active", {

  # Fixed ids (the list keys) keep the wire shape deterministic so the id
  # (object key) / name (field) / active split stays visible and
  # regression-guarded.
  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    views = list(
      view_one = dock_view("a", name = "Analysis"),
      view_two = dock_view("b", name = "Overview")
    ),
    active = "view_two"
  )

  views <- blockr_ser(brd)[["payload"]][["views"]][["payload"]]

  expect_identical(views[["active"]], "view_two")
  expect_identical(names(views[["views"]]), c("view_one", "view_two"))
  expect_identical(views[["views"]][["view_one"]][["name"]], "Analysis")
  expect_identical(views[["views"]][["view_two"]][["name"]], "Overview")
  expect_identical(
    unlist(views[["views"]][["view_one"]][["payload"]]),
    "block_panel-a"
  )
})

test_that("a grid serializes to our compact form", {
  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    views = list(Page = c("a", "b")),
    grids = list(Page = dock_grid("a", "b", sizes = c(0.3, 0.7)))
  )

  payload <- blockr_ser(brd)[["payload"]][["grids"]][["payload"]][[1L]]

  # Our compact form: orientation / children / sizes, never dockView's tree
  # (`grid` / `root`), resolved `panels`, or `activeGroup`.
  expect_true(all(c("orientation", "children", "sizes") %in% names(payload)))
  expect_false(
    any(c("grid", "root", "panels", "activeGroup") %in% names(payload))
  )
  expect_equal(payload[["sizes"]], c(0.3, 0.7))
  expect_identical(payload[["children"]][[1L]][["panels"]], "block_panel-a")
})

test_that("custom sizes round-trip through ser/des", {
  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    views = list(Page = c("a", "b")),
    grids = list(Page = dock_grid("a", "b", sizes = c(0.3, 0.7)))
  )
  grid <- board_grids(blockr_deser(blockr_ser(brd)))[[1L]]
  expect_equal(grid[["sizes"]], c(0.3, 0.7))
})

test_that("panels(active = ...) round-trips through ser/des", {
  brd <- new_dock_board(
    blocks = c(
      a = new_dataset_block(), b = new_head_block(), c = new_head_block()
    ),
    views = list(Page = c("a", "b", "c")),
    grids = list(Page = dock_grid(panels("a", "b", "c", active = "b")))
  )
  grid <- board_grids(blockr_deser(blockr_ser(brd)))[[1L]]
  expect_identical(grid[["children"]][[1L]][["active"]], "block_panel-b")
})

test_that("orientation round-trips through ser/des", {
  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    views = list(Page = c("a", "b")),
    grids = list(Page = dock_grid("a", "b", orientation = "vertical"))
  )
  grid <- board_grids(blockr_deser(blockr_ser(brd)))[[1L]]
  expect_identical(grid[["orientation"]], "vertical")
})

test_that("nested group() sizes round-trip through ser/des", {
  brd <- new_dock_board(
    blocks = c(
      a = new_dataset_block(), b = new_head_block(), c = new_head_block()
    ),
    views = list(Page = c("a", "b", "c")),
    grids = list(
      Page = dock_grid(
        "a", group("b", "c", sizes = c(0.4, 0.6)), sizes = c(0.3, 0.7)
      )
    )
  )
  grid <- board_grids(blockr_deser(blockr_ser(brd)))[[1L]]
  expect_equal(grid[["sizes"]], c(0.3, 0.7))
  expect_equal(grid[["children"]][[2L]][["sizes"]], c(0.4, 0.6))
})

test_that("a grid survives a real JSON encode/decode round-trip", {
  # blockr.core writes via toJSON(null = "null") / fromJSON(simplifyVector =
  # TRUE), which collapses all-scalar arrays to atomic vectors. Exercise that
  # path so a boxing / simplify regression can't ship silently.
  brd <- new_dock_board(
    blocks = c(
      a = new_dataset_block(), b = new_head_block(), c = new_head_block()
    ),
    views = list(Page = c("a", "b", "c")),
    grids = list(
      Page = dock_grid(
        "a", group("b", "c", sizes = c(0.4, 0.6)), sizes = c(0.3, 0.7)
      )
    )
  )
  ser <- blockr_ser(brd)
  json <- jsonlite::toJSON(ser, null = "null")
  back <- jsonlite::fromJSON(json, simplifyDataFrame = FALSE,
                             simplifyMatrix = FALSE)
  grid <- board_grids(blockr_deser(back))[[1L]]

  expect_equal(grid[["sizes"]], c(0.3, 0.7))
  expect_equal(grid[["children"]][[2L]][["sizes"]], c(0.4, 0.6))
  expect_setequal(
    layout_panel_ids(grid),
    c("block_panel-a", "block_panel-b", "block_panel-c")
  )
})

test_that("focus round-trips through the dockView seam", {
  blks <- as_blocks(
    list(a = new_dataset_block(), b = new_head_block(), c = new_head_block())
  )

  # dockView's focus is a group, so the focused panel is that group's open
  # tab -- `focus` and the leaf's `active` name the same panel.
  grid <- dock_grid(
    "block_panel-a",
    panels("block_panel-b", "block_panel-c", active = "block_panel-c")
  )
  grid[["focus"]] <- "block_panel-c"

  # Expanding to a dockView layout carries the focus as an activeGroup id;
  # collapsing back recovers the focused panel.
  lay <- as_dock_layout(grid, blocks = blks)
  expect_true(is.character(lay[["activeGroup"]]))
  expect_identical(as_dock_grid(lay)[["focus"]], "block_panel-c")
})

test_that("dockView pixel sizes normalise to ratios on collapse", {
  # A live dockView echo carries absolute pixel sizes; collapsing to our
  # canonical grid normalises them to 0-1 ratios.
  leaf <- function(id, size) {
    list(type = "leaf",
         data = list(views = list(id), activeView = id, id = "1"),
         size = size)
  }
  echo <- list(
    grid = list(
      root = list(
        type = "branch",
        data = list(leaf("block_panel-a", 300), leaf("block_panel-b", 700)),
        size = 1000
      ),
      orientation = "HORIZONTAL"
    ),
    activeGroup = "1"
  )
  grid <- as_dock_grid(as_dock_layout(echo))
  expect_equal(grid[["sizes"]], c(0.3, 0.7))
})

test_that("layout_panel_ids and panel_obj_ids are inverse-ish", {
  grid <- resolve_grid(
    dock_grid("a", "b"),
    panel_id_map(c(a = new_dataset_block(), b = new_head_block()), list())
  )
  pids <- layout_panel_ids(grid)
  expect_setequal(pids, c("block_panel-a", "block_panel-b"))
  expect_setequal(panel_obj_ids(pids), c("a", "b"))
})
