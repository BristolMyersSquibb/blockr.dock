test_that("view_tabs is a board option, off unless the blockr option says so", {

  expect_false(board_option_value(new_view_tabs_option()))
  expect_true(board_option_value(new_view_tabs_option(TRUE)))

  withr::local_options(blockr.view_tabs = TRUE)
  expect_true(board_option_value(new_view_tabs_option()))

  expect_true("view_tabs" %in% names(dock_board_options()))
})

test_that("the navbar is first drawn from the board's option", {

  on <- new_board_options(new_view_tabs_option(TRUE))
  off <- new_board_options(new_view_tabs_option(FALSE))

  expect_match(as.character(view_tabs_init(on)), "'blockr-view-tabs', true")
  expect_match(as.character(view_tabs_init(off)), "'blockr-view-tabs', false")

  # a board saved before the option existed takes the blockr option
  withr::local_options(blockr.view_tabs = TRUE)
  expect_match(
    as.character(view_tabs_init(new_board_options())),
    "'blockr-view-tabs', true"
  )
})
