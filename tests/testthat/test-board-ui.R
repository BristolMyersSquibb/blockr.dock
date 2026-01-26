test_that("dummy board ui test", {

  ui <- board_ui(
    "test",
    new_dock_board(blocks = c(a = new_dataset_block()))
  )

  expect_s3_class(ui, "shiny.tag.list")
  # 6 original + 2 (shinyjs + sidebar panels) - settings now in sidebar
  expect_length(ui, 8L)
})
