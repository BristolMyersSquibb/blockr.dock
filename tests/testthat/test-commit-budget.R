test_that("attribute_commit names the writer behind each payload slot", {

  expect_identical(
    attribute_commit(list(views = list(grid = list(V = NULL)))),
    "grid mirror"
  )
  expect_identical(
    attribute_commit(list(views = list(mod = list(V = "block_panel-a")))),
    "membership lifecycle"
  )
  expect_identical(
    attribute_commit(list(views = list(active = "V"))),
    "view switch"
  )
  expect_identical(
    attribute_commit(list(blocks = list(rm = "a"))),
    "block lifecycle"
  )
  expect_identical(attribute_commit(list()), "unknown")
})

test_that("record_commit tallies commits and aborts past the budget", {

  ms <- new_mock_session()
  withr::defer(if (!ms$isClosed()) ms$close())

  with_mock_context(ms, {

    ledger <- new_commit_ledger()

    record_commit(ledger, "grid mirror", budget = 2)
    record_commit(ledger, "membership lifecycle", budget = 2)

    expect_identical(isolate(ledger$count()), 2L)

    # The third commit in the same flush exceeds a budget of two: the abort
    # carries the attributed writer chain so a loop names its writers.
    expect_error(
      record_commit(ledger, "grid mirror", budget = 2),
      class = "dock_commit_budget_exceeded"
    )

    err <- tryCatch(
      record_commit(ledger, "membership lifecycle", budget = 2),
      dock_commit_budget_exceeded = function(e) conditionMessage(e)
    )
    expect_match(err, "grid mirror -> membership lifecycle")
  })
})

test_that("reset_flush_ledger clears the flush chain but not the tally", {

  ms <- new_mock_session()
  withr::defer(if (!ms$isClosed()) ms$close())

  with_mock_context(ms, {

    ledger <- new_commit_ledger()
    record_commit(ledger, "grid mirror", budget = 5)
    record_commit(ledger, "grid mirror", budget = 5)

    reset_flush_ledger(ledger)

    expect_length(ledger$flush, 0L)
    expect_identical(isolate(ledger$count()), 2L)
  })
})

test_that("the budget bounds a spinning flush instead of hanging", {

  ms <- new_mock_session()
  withr::defer(if (!ms$isClosed()) ms$close())

  with_mock_context(ms, {

    withr::local_options(blockr.dock_commit_budget = 3)

    ledger <- new_commit_ledger()
    tick <- reactiveVal(0L)
    raw_update <- function(payload) isolate(tick(tick() + 1L))
    update <- instrument_commits(raw_update, ledger, ms)

    # A synchronous reactive loop: each run commits and re-invalidates, the
    # shape a runaway takes inside a single flush. Without the budget it would
    # spin to 500; the abort breaks it at the fourth commit (budget + 1). Shiny
    # logs the abort from the looping observer -- the "stack trace not a
    # spinning app" the budget promises -- so the effect to assert is the bound.
    observe({
      if (tick() < 500L) {
        update(list(views = list(grid = set_names(list(NULL), "V"))))
      }
    })

    suppressWarnings(ms$flushReact())

    expect_identical(isolate(ledger$count()), 4L)
  })
})

test_that("the flush chain resets between flushes, so gestures don't accrue", {

  ms <- new_mock_session()
  withr::defer(if (!ms$isClosed()) ms$close())

  with_mock_context(ms, {

    withr::local_options(blockr.dock_commit_budget = 1)

    ledger <- new_commit_ledger()
    committed <- 0L
    raw_update <- function(payload) committed <<- committed + 1L
    update <- instrument_commits(raw_update, ledger, ms)

    trigger <- reactiveVal(0L)

    observeEvent(trigger(), update(list(views = list(active = "V"))),
                 ignoreInit = TRUE)

    ms$flushReact()

    # Two separate client events, one commit each. A budget of one holds
    # because onFlushed resets the flush chain between them.
    trigger(1L)
    ms$flushReact()
    trigger(2L)
    ms$flushReact()

    expect_identical(committed, 2L)
    expect_identical(isolate(ledger$count()), 2L)
    expect_length(ledger$flush, 0L)
  })
})

test_that("a bare update() read forwards through the instrumented handle", {

  ms <- new_mock_session()
  withr::defer(if (!ms$isClosed()) ms$close())

  with_mock_context(ms, {

    ledger <- new_commit_ledger()
    payload <- reactiveVal(list(marker = TRUE))
    update <- instrument_commits(payload, ledger, ms)

    expect_identical(isolate(update()), list(marker = TRUE))
    expect_identical(isolate(ledger$count()), 0L)
  })
})

test_that("grids_stable holds when live is the stored fixed point", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_dataset_block()),
    layouts = list(V = dock_layout("a", "b", sizes = c(0.3, 0.7)))
  )

  stored <- board_grids(brd)

  # A non-default authored layout stores a non-NULL grid, so the comparison is
  # not vacuously true.
  expect_false(is.null(stored[["V"]]))

  # board_layouts models what dockview renders from the authored push;
  # projecting it must return the stored authored grid.
  expect_true(grids_stable(stored, board_layouts(brd)))

  # Sub-tolerance size jitter is still the fixed point -- grids_stable uses the
  # mirror's own all.equal(tolerance = grid_size_tol()), so it doesn't commit.
  jittered <- reconstruct_dock_layouts(
    list(
      V = dock_layout("block_panel-a", "block_panel-b", sizes = c(0.301, 0.699))
    )
  )
  expect_true(grids_stable(stored, jittered))

  # A live layout whose sizes drifted past the tolerance is not the fixed point.
  drifted <- reconstruct_dock_layouts(
    list(
      V = dock_layout("block_panel-a", "block_panel-b", sizes = c(0.85, 0.15))
    )
  )
  expect_false(grids_stable(stored, drifted))
})
