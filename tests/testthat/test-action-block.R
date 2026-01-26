# Test that actions are created correctly and can be invoked
# Note: Full sidebar integration is tested in board-server tests

test_that("add_block_action creates valid action", {
  action <- add_block_action(
    trigger = reactive(TRUE),
    board = reactiveValues(board = new_board()),
    update = reactiveVal(list())
  )
  expect_s3_class(action, "action")
  expect_identical(attr(action, "id"), "add_block_action")
})

test_that("append_block_action creates valid action", {
  action <- append_block_action(
    trigger = reactive("a"),
    board = reactiveValues(
      board = new_board(blocks = c(a = new_dataset_block()))
    ),
    update = reactiveVal(list())
  )
  expect_s3_class(action, "action")
  expect_identical(attr(action, "id"), "append_block_action")
})

test_that("prepend_block_action creates valid action", {
  action <- prepend_block_action(
    trigger = reactive(list(target = "a", input = "data")),
    board = reactiveValues(
      board = new_board(blocks = c(a = new_head_block()))
    ),
    update = reactiveVal(list())
  )
  expect_s3_class(action, "action")
  expect_identical(attr(action, "id"), "prepend_block_action")
})

test_that("remove_block_action creates valid action", {
  action <- remove_block_action(
    trigger = reactive("a"),
    board = reactiveValues(
      board = new_board(blocks = c(a = new_dataset_block()))
    ),
    update = reactiveVal(list())
  )
  expect_s3_class(action, "action")
  expect_identical(attr(action, "id"), "remove_block_action")
})
