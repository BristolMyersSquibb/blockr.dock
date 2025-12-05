test_that("ser/des utils", {

  board <- new_dock_board()

  expect_identical(
    board,
    blockr_deser(blockr_ser(board)),
    ignore_function_env = TRUE
  )
})
