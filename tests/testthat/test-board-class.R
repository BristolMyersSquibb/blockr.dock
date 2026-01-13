test_that("dock board", {

  board0 <- new_dock_board()

  expect_s3_class(board0, "dock_board")
  expect_true(is_dock_board(board0))

  board1 <- new_dock_board(
    list(
      d = new_merge_block(),
      a = new_dataset_block(),
      c = new_subset_block(),
      e = new_subset_block(),
      b = new_dataset_block()
    ),
    data.frame(
      id = c("ad", "cd", "bc", "de"),
      from = c("a", "c", "b", "d"),
      to = c("d", "d", "c", "e"),
      input = c("x", "y", "", "")
    ),
    list(bc = c("b", "c")),
    extensions = new_edit_board_extension()
  )

  expect_s3_class(board1, "dock_board")
  expect_true(is_dock_board(board1))

  expect_setequal(board_block_ids(board1), letters[1:5])
  expect_setequal(board_link_ids(board1), c("ad", "cd", "bc", "de"))
  expect_setequal(board_stack_ids(board1), "bc")

  expect_length(dock_layout(board1)$panels, 6L)

  empty <- clear_board(board1)

  expect_length(board_blocks(empty), 0L)
  expect_length(board_links(empty), 0L)
  expect_length(board_stacks(empty), 0L)

  expect_length(dock_layout(empty)$panels, 1L)

  expect_identical(
    board_options(board1),
    board_options(empty)
  )
})
