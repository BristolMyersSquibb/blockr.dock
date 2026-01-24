test_that("dummy board ui test", {

  ui <- board_ui(
    "test",
    new_dock_board(blocks = c(a = new_dataset_block()))
  )

  expect_s3_class(ui, "shiny.tag.list")
  # 7 original + 2 (shinyjs + sidebar panels)
  expect_length(ui, 9L)
})
