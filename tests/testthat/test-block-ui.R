test_that("dummy block ui test", {

  ui <- block_ui(
    "test",
    new_dock_board(blocks = c(a = new_dataset_block())),
    edit_block_ui
  )

  expect_type(ui, "list")
  expect_length(ui, 1L)
  expect_s3_class(ui[[1L]], "shiny.tag")
})
