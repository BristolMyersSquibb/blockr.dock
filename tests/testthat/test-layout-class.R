test_that("panel layout", {
  expect_snapshot(draw_panel_tree(NULL))
  expect_snapshot(draw_panel_tree(c("a", "b", "c")))
  expect_snapshot(draw_panel_tree(list("a", list("b", "c"))))
})

test_that("create_dock_layout accepts a bare dock_extension", {

  blks <- c(a = new_dataset_block(), b = new_head_block())
  ext <- new_edit_board_extension()

  ly <- create_dock_layout(blks, ext)

  expect_true(is_dock_layout(ly))
  expect_setequal(
    layout_panel_ids(ly),
    c("block_panel-a", "block_panel-b", "ext_panel-edit_board_extension")
  )

  ly2 <- create_dock_layout(blks, ext, list("edit_board_extension", c("a", "b")))

  expect_setequal(
    layout_panel_ids(ly2),
    c("block_panel-a", "block_panel-b", "ext_panel-edit_board_extension")
  )
})

test_that("create_dock_layout accepts a named list of extensions", {

  blks <- c(a = new_dataset_block(), b = new_head_block())
  exts <- list(edit = new_edit_board_extension())

  ly <- create_dock_layout(blks, exts)

  expect_true(is_dock_layout(ly))
  expect_setequal(
    layout_panel_ids(ly),
    c("block_panel-a", "block_panel-b", "ext_panel-edit_board_extension")
  )

  ly2 <- create_dock_layout(blks, exts, list("edit", c("a", "b")))

  expect_setequal(
    layout_panel_ids(ly2),
    c("block_panel-a", "block_panel-b", "ext_panel-edit_board_extension")
  )
})

test_that("create_dock_layout accepts a dock_extensions collection", {

  blks <- c(a = new_dataset_block(), b = new_head_block())
  exts <- new_dock_extensions(list(new_edit_board_extension()))

  ly <- create_dock_layout(blks, exts)

  expect_true(is_dock_layout(ly))
  expect_setequal(
    layout_panel_ids(ly),
    c("block_panel-a", "block_panel-b", "ext_panel-edit_board_extension")
  )

  ly2 <- create_dock_layout(
    blks, exts, list("edit_board_extension", c("a", "b"))
  )

  expect_setequal(
    layout_panel_ids(ly2),
    c("block_panel-a", "block_panel-b", "ext_panel-edit_board_extension")
  )
})

test_that("default_layout matches create_dock_layout key shape per input form", {

  blks <- c(a = new_dataset_block())

  expect_identical(
    default_layout(blks, new_edit_board_extension()),
    list("edit_board_extension", "a")
  )

  expect_identical(
    default_layout(blks, list(edit = new_edit_board_extension())),
    list("edit", "a")
  )

  expect_identical(
    default_layout(blks, new_dock_extensions(list(new_edit_board_extension()))),
    list("edit_board_extension", "a")
  )
})
