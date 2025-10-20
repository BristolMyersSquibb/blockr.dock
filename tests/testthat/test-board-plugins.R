test_that("board plugins", {

  plugins <- board_plugins(new_dock_board())

  expect_true(is_plugins(plugins))

  expect_named(
    plugins,
    c("preserve_board", "notify_user", "generate_code"),
    ignore.order = TRUE
  )
})
