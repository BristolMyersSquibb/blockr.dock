test_that("action utils", {

  act <- new_action(function() log_info("hello", pkg = "blockr.test"))

  expect_s3_class(act, "action")

  fun <- act("go", as_module = FALSE)

  expect_s3_class(fun, "action_function")

  with_mock_session(
    expect_null(fun())
  )

  mod <- act(function(input) TRUE)

  expect_s3_class(mod, "action_module")

  expect_null(
    testServer(
      function(id) moduleServer(id, mod()),
      NULL
    )
  )
})
