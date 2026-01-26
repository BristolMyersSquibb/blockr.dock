test_that("board plugins", {

  plugins <- board_plugins(new_dock_board())

  expect_true(is_plugins(plugins))

  # Note: generate_code is handled directly via navbar/sidebar, not as a plugin
  expect_named(
    plugins,
    c("edit_block", "preserve_board"),
    ignore.order = TRUE
  )
})
