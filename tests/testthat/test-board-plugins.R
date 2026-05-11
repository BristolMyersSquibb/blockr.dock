test_that("board plugins", {

  plugins <- board_plugins(new_dock_board())

  expect_true(is_plugins(plugins))

  expect_named(
    plugins,
    c("edit_block", "preserve_board", "generate_code"),
    ignore.order = TRUE
  )
})

test_that("preserve_board stays available in locked mode (#123)", {

  withr::local_options(blockr.dock_is_locked = TRUE)
  expect_true(is_dock_locked())

  plugins <- board_plugins(new_dock_board())

  expect_true("preserve_board" %in% names(plugins))
})
