test_that("ser/des utils", {

  board1 <- new_dock_board()

  expect_identical(
    board1,
    blockr_deser(blockr_ser(board1)),
    ignore_function_env = TRUE
  )

  board2 <- new_dock_board(extensions = new_edit_board_extension())

  expect_identical(
    board2,
    blockr_deser(blockr_ser(board2)),
    ignore_function_env = TRUE
  )
})

test_that("dock_layouts serialization round-trip", {
  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = dock_layouts(
      Tab1 = list("a", "b"),
      Tab2 = dock_grid("a", active = TRUE)
    )
  )

  ser <- blockr_ser(brd)
  des <- blockr_deser(ser)

  ly <- des[["layouts"]]
  expect_s3_class(ly, "dock_layouts")
  expect_named(ly, c("Tab1", "Tab2"))
  expect_identical(active_view(ly), "Tab2")
})
