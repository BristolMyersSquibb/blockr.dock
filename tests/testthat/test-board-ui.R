test_that("dummy board ui test", {

  ui <- board_ui(
    "test",
    new_dock_board(blocks = c(a = new_dataset_block()))
  )

  expect_s3_class(ui, "shiny.tag.list")
  expect_length(ui, 7L)
})

test_that("locked mode renders a navbar lock indicator", {

  brd <- new_dock_board(blocks = c(a = new_dataset_block()))

  unlocked_html <- withr::with_options(
    list(blockr.dock_is_locked = NULL),
    as.character(board_ui("test", brd))
  )
  expect_false(grepl("blockr-lock-indicator", unlocked_html, fixed = TRUE))

  locked_html <- withr::with_options(
    list(blockr.dock_is_locked = TRUE),
    as.character(board_ui("test", brd))
  )
  expect_match(locked_html, "blockr-lock-indicator", fixed = TRUE)
  expect_match(locked_html, 'role="status"', fixed = TRUE)
  expect_match(locked_html, "blockr-lock-indicator-label", fixed = TRUE)
  # Visible label, not just the aria-label / tooltip.
  expect_match(locked_html, ">Read-only<", fixed = TRUE)
})
