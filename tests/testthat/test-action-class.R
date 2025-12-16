test_that("action ctor", {

  act <- new_action(
    function(input, output, session) log_info("hello", pkg = "blockr.test"),
    id = "test_action"
  )

  expect_s3_class(act, "action")
  expect_true(is_action(act))
  expect_identical(action_id(act), "test_action")

  gen <- function(trigger, board, update) act

  expect_true(is_action_generator(gen))
  expect_identical(action_id(gen), "test_action")

  eb_trig <- board_action_triggers(new_edit_board_extension())

  expect_type(eb_trig, "list")
  expect_length(eb_trig, 0L)

  db_trig <- board_action_triggers(new_dock_board())

  expect_type(db_trig, "list")
  expect_length(db_trig, 8L)

  acts <- dock_actions()

  expect_type(acts, "list")
  expect_length(acts, 8L)

  expect_null(
    register_actions(
      dock_actions(),
      board_action_triggers(new_dock_board()),
      board = list(),
      update = list(),
      session = MockShinySession$new()
    )
  )
})
