test_that("panel layout", {
  expect_snapshot(draw_panel_tree(NULL))
  expect_snapshot(draw_panel_tree(c("a", "b", "c")))
  expect_snapshot(draw_panel_tree(list("a", list("b", "c"))))
})

test_that("layout resolution accepts a bare dock_extension", {

  blks <- c(a = new_dataset_block(), b = new_head_block())
  ext <- new_edit_board_extension()

  brd <- new_dock_board(blocks = blks, extensions = ext)
  ly <- active_layout(brd)

  expect_true(is_dock_layout(ly))
  expect_setequal(
    layout_panel_ids(ly),
    c("block_panel-a", "block_panel-b", "ext_panel-edit_board_extension")
  )

  brd2 <- new_dock_board(
    blocks = blks,
    extensions = ext,
    layouts = list("edit_board_extension", c("a", "b"))
  )
  expect_setequal(
    layout_panel_ids(active_layout(brd2)),
    c("block_panel-a", "block_panel-b", "ext_panel-edit_board_extension")
  )
})

test_that("layout resolution accepts a named list of extensions", {

  blks <- c(a = new_dataset_block(), b = new_head_block())
  exts <- list(edit = new_edit_board_extension())

  brd <- new_dock_board(blocks = blks, extensions = exts)
  ly <- active_layout(brd)

  expect_true(is_dock_layout(ly))
  expect_setequal(
    layout_panel_ids(ly),
    c("block_panel-a", "block_panel-b", "ext_panel-edit_board_extension")
  )

  brd2 <- new_dock_board(
    blocks = blks,
    extensions = exts,
    layouts = list("edit_board_extension", c("a", "b"))
  )
  expect_setequal(
    layout_panel_ids(active_layout(brd2)),
    c("block_panel-a", "block_panel-b", "ext_panel-edit_board_extension")
  )
})

test_that("layout resolution accepts a dock_extensions collection", {

  blks <- c(a = new_dataset_block(), b = new_head_block())
  exts <- new_dock_extensions(list(new_edit_board_extension()))

  brd <- new_dock_board(blocks = blks, extensions = exts)
  ly <- active_layout(brd)

  expect_true(is_dock_layout(ly))
  expect_setequal(
    layout_panel_ids(ly),
    c("block_panel-a", "block_panel-b", "ext_panel-edit_board_extension")
  )

  brd2 <- new_dock_board(
    blocks = blks,
    extensions = exts,
    layouts = list("edit_board_extension", c("a", "b"))
  )
  expect_setequal(
    layout_panel_ids(active_layout(brd2)),
    c("block_panel-a", "block_panel-b", "ext_panel-edit_board_extension")
  )
})

test_that("default_layout uses class-name convention across input forms", {

  blks <- c(a = new_dataset_block())
  expected <- list("edit_board_extension", "a")

  expect_identical(
    default_layout(blks, new_edit_board_extension()),
    expected
  )

  expect_identical(
    default_layout(blks, list(edit = new_edit_board_extension())),
    expected
  )

  expect_identical(
    default_layout(blks, new_dock_extensions(list(new_edit_board_extension()))),
    expected
  )
})

test_that("dock_layout constructor with active flag", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = dock_layouts(
      First = list("a"),
      Second = dock_layout("a", "b", active = TRUE)
    )
  )

  expect_identical(active_view(board_layouts(brd)), "Second")
  expect_length(active_layout(brd)$panels, 2L)
})

test_that("new_dock_layout accepts active flag", {

  ly <- new_dock_layout(active = TRUE)
  expect_true(isTRUE(attr(ly, "active")))

  ly2 <- new_dock_layout()
  expect_null(attr(ly2, "active"))
})
