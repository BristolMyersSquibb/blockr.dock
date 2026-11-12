test_that("each dock board option holds the value it is given, under its id", {

  opts <- list(
    compact = new_compact_option(TRUE),
    view_tabs = new_view_tabs_option(TRUE),
    sync_rails = new_sync_rails_option(FALSE)
  )

  for (id in names(opts)) {
    expect_true(is_board_option(opts[[id]]))
    expect_identical(board_option_id(opts[[id]]), id)
  }

  expect_true(board_option_value(opts[["compact"]]))
  expect_true(board_option_value(opts[["view_tabs"]]))
  expect_false(board_option_value(opts[["sync_rails"]]))
})

test_that("a dock board option made without a value holds none", {

  withr::local_options(
    blockr.compact = TRUE,
    blockr.view_tabs = TRUE,
    blockr.sync_rails = FALSE
  )

  opts <- list(
    new_compact_option(),
    new_view_tabs_option(),
    new_sync_rails_option()
  )

  for (opt in opts) {
    expect_length(opt, 0L)
    expect_null(board_option_value(opt))
  }
})
