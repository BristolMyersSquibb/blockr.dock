test_that("action ctor", {

  act <- new_action(function() log_info("hello", pkg = "blockr.test"))

  expect_s3_class(act, "action")
  expect_true(is_action(act))

  fun <- act("go", as_module = FALSE)

  expect_s3_class(fun, "action_function")
  expect_true(is_action_function(fun))

  with_mock_session(
    expect_null(fun())
  )

  mod <- act(function(input) TRUE)

  expect_s3_class(mod, "action_module")
  expect_true(is_action_module(mod))

  expect_null(
    testServer(
      function(id) moduleServer(id, mod()),
      NULL
    )
  )

  err <- act(TRUE)

  expect_error(
    testServer(
      function(id) moduleServer(id, err()),
      NULL
    ),
    class = "invalid_action_trigger"
  )
})
