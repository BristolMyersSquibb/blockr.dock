test_that("the compact switch is a dock board option, off by default", {

  opts <- dock_board_options()

  expect_true("compact" %in% names(opts))
  expect_false(board_option_default(opts[["compact"]]))

  expect_true(board_option_default(new_compact_option(TRUE)))

  expect_error(
    new_compact_option("yes"),
    class = "board_options_compact_invalid"
  )
})
