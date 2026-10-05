test_that("the rail sync switch is a dock board option, on by default", {

  opts <- dock_board_options()

  expect_true("sync_rails" %in% names(opts))
  expect_true(board_option_default(opts[["sync_rails"]]))

  expect_false(board_option_default(new_sync_rails_option(FALSE)))

  expect_error(
    new_sync_rails_option("yes"),
    class = "board_options_sync_rails_invalid"
  )
})

test_that("the rail sync option tells the client, which keeps rails in step", {

  ms <- new_mock_session()
  withr::defer(if (!ms$isClosed()) ms$close())

  sent <- list()

  value <- with_mock_context(ms, reactiveVal(TRUE))

  session <- list(
    ns = identity,
    userData = list2env(list(board_options = list(sync_rails = value))),
    onFlush = function(fun, once = TRUE) fun(),
    sendInputMessage = function(...) invisible(),
    sendCustomMessage = function(type, message) {
      if (identical(type, "blockr-rail-sync")) {
        sent[[length(sent) + 1L]] <<- message
      }
      invisible()
    }
  )

  with_mock_context(
    ms,
    board_option_server(new_sync_rails_option(), session = session)
  )
  ms$flushReact()

  expect_identical(sent, list(TRUE))
  expect_true(rails_synced(session))

  with_mock_context(ms, value(FALSE))
  ms$flushReact()

  expect_identical(sent, list(TRUE, FALSE))
  expect_false(rails_synced(session))

  # A board without the option, such as one saved before it existed, keeps
  # each view's rails as they were left.
  expect_false(rails_synced(list(userData = new.env())))
})
