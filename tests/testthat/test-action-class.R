test_that("action ctor", {

  act <- new_action(
    function(input, output, session) log_info("hello", pkg = "blockr.test"),
    id = "test_action"
  )

  expect_s3_class(act, "action")
  expect_true(is_action(act))
  expect_identical(action_id(act), "test_action")

  gen <- function(trigger, board, update, ...) act

  expect_true(is_action_generator(gen))
  expect_identical(action_id(gen), "test_action")

  eb_act <- board_actions(new_edit_board_extension())

  expect_type(eb_act, "list")
  expect_length(eb_act, 0L)

  db_act <- board_actions(new_dock_board())

  expect_type(db_act, "list")
  expect_length(db_act, 8L)

  expect_null(
    register_actions(
      db_act,
      triggers = action_triggers(db_act),
      board = list(),
      update = list(),
      args = list(),
      session = MockShinySession$new()
    )
  )
})
