# Extracted from test-board-ui.R:13

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "blockr.dock", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
ui <- board_ui(
    "test",
    new_dock_board(blocks = c(a = new_dataset_block()))
  )
expect_s3_class(ui, "shiny.tag.list")
expect_length(ui, 18L)
