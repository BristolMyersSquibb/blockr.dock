test_that("dock board", {

  board <- new_dock_board()

  expect_s3_class(board, "dock_board")
  expect_true(is_dock_board(board))
})
