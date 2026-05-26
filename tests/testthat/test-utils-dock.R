test_that("req_unlocked() is a no-op when the dock is unlocked", {
  withr::local_options(blockr.dock_is_locked = NULL)
  expect_false(is_dock_locked())
  # Returns invisibly without throwing
  expect_silent(res <- req_unlocked())
  expect_null(res)
})

test_that("req_unlocked() short-circuits via req(FALSE) when locked", {
  withr::local_options(blockr.dock_is_locked = TRUE)
  expect_true(is_dock_locked())

  # `req(FALSE)` throws a "shiny.silent.error" — observeEvent catches it
  # and treats the trigger as "no event"; outside of a reactive context
  # the error surfaces as a regular condition.
  expect_error(req_unlocked(), class = "shiny.silent.error")
})

test_that("req_unlocked() in an eventExpr block stops handler execution", {
  # Mirrors the call-site pattern used throughout: `observeEvent({ req_unlocked();
  # input$x }, handler)`. Verifies that setInputs of the gated input in locked
  # mode does *not* invoke the handler — the trigger never fires.

  handler_calls <- 0L

  testServer(
    function(id, ...) {
      moduleServer(id, function(input, output, session) {
        observeEvent(
          {
            req_unlocked()
            input$go
          },
          handler_calls <<- handler_calls + 1L
        )
      })
    },
    {
      withr::local_options(blockr.dock_is_locked = TRUE)

      session$setInputs(go = 1L)
      expect_identical(handler_calls, 0L)

      session$setInputs(go = 2L)
      expect_identical(handler_calls, 0L)
    }
  )

  # Same module, unlocked: handler does fire.
  handler_calls <- 0L

  testServer(
    function(id, ...) {
      moduleServer(id, function(input, output, session) {
        observeEvent(
          {
            req_unlocked()
            input$go
          },
          handler_calls <<- handler_calls + 1L
        )
      })
    },
    {
      withr::local_options(blockr.dock_is_locked = NULL)

      session$setInputs(go = 1L)
      expect_identical(handler_calls, 1L)
    }
  )
})
