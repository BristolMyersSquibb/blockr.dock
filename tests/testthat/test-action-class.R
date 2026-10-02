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
  expect_length(db_act, 12L)

  trig <- action_triggers(db_act)

  expect_length(trig, 12L)
  expect_named(trig, chr_ply(db_act, action_id))

  expect_null(
    register_actions(
      db_act,
      trig,
      board = list(),
      update = list(),
      extensions = list(),
      session = MockShinySession$new()
    )
  )
})

test_that("a trigger names where its gesture happened, for that firing", {

  trigger <- new_trigger()

  isolate({
    trigger("a", at = list(x = 10, y = 20))
    expect_identical(trigger(), "a")
    expect_identical(trigger_at(trigger), list(x = 10, y = 20))

    trigger("b")
    expect_identical(trigger(), "b")
    expect_null(trigger_at(trigger))
  })

  # A plain reactive used as a trigger names nowhere.
  expect_null(trigger_at(reactive("a")))
})
