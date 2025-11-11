test_that("board plugins", {
  plugins <- board_plugins(new_dock_board())

  expect_true(is_plugins(plugins))

  expect_named(
    plugins,
    c("edit_block", "preserve_board", "generate_code"),
    ignore.order = TRUE
  )
})
