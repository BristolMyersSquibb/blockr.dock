# Test that actions are created correctly and can be invoked
# Note: Full sidebar integration is tested in board-server tests

test_that("add_link_action creates valid action", {
  action <- add_link_action(
    trigger = reactive("a"),
    board = reactiveValues(
      board = new_board(
        c(
          a = new_dataset_block("iris"),
          b = new_head_block()
        )
      )
    ),
    update = reactiveVal(list())
  )
  expect_s3_class(action, "action")
  expect_identical(attr(action, "id"), "add_link_action")
})

test_that("remove_link_action creates valid action", {
  action <- remove_link_action(
    trigger = reactive("ab"),
    board = reactiveValues(
      board = new_board(
        c(
          a = new_dataset_block("iris"),
          b = new_head_block()
        ),
        links = links(id = "ab", from = "a", to = "b")
      )
    ),
    update = reactiveVal(list())
  )
  expect_s3_class(action, "action")
  expect_identical(attr(action, "id"), "remove_link_action")
})
