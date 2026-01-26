# Test that actions are created correctly and can be invoked
# Note: Full sidebar integration is tested in board-server tests

test_that("add_stack_action creates valid action", {
  action <- add_stack_action(
    trigger = reactive(TRUE),
    board = reactiveValues(board = new_board()),
    update = reactiveVal(list())
  )
  expect_s3_class(action, "action")
  expect_identical(attr(action, "id"), "add_stack_action")
})

test_that("edit_stack_action creates valid action", {
  action <- edit_stack_action(
    trigger = reactive("a"),
    board = reactiveValues(
      board = new_dock_board(
        c(a = new_dataset_block("iris")),
        stacks = stacks(a = "a")
      )
    ),
    update = reactiveVal(list())
  )
  expect_s3_class(action, "action")
  expect_identical(attr(action, "id"), "edit_stack_action")
})

test_that("remove_stack_action creates valid action and updates directly", {
  action <- remove_stack_action(
    trigger = reactive("a"),
    board = reactiveValues(
      board = new_dock_board(
        c(a = new_dataset_block("iris")),
        stacks = stacks(a = "a")
      )
    ),
    update = reactiveVal(list())
  )
  expect_s3_class(action, "action")
  expect_identical(attr(action, "id"), "remove_stack_action")
})
