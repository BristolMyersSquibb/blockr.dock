# End-to-end sentinel for the loop-safety harness. Two test-only server exports
# -- via blockr.core's `blockr_test_exports` hook, gated by Shiny's test mode so
# a running app carries nothing -- let a browser test observe, through the full
# server stack, the contract no unit test sees: one settled gesture is a bounded
# number of board commits and quiescence adds none, and the authored layout
# round-trips through real dockView. A reconciliation loop either never settles
# (wait_for_idle times out) or keeps the commit count climbing.

test_that("a settled gesture commits a bounded amount, quiescence adds none", {

  skip_on_cran()

  app <- new_app_driver(
    system.file("examples", "empty", "app.R", package = "blockr.dock"),
    name = "commit-sentinel",
    seed = 42,
    load_timeout = 30 * 1000,
    timeout = 30 * 1000
  )
  withr::defer(app$stop())

  app$wait_for_idle()

  baseline <- app$get_value(export = "commit_count")
  expect_false(is.na(baseline))

  # One gesture: add a block through the edit-board extension (its panel is
  # active on load). The board update mounts the panel -- a stable
  # block_handle-<id> DOM id marks the commit landing.
  app$set_inputs(
    `my_board-ext_edit_board-registry_select` = "dataset_block",
    `my_board-ext_edit_board-block_id` = "a"
  )
  app$click("my_board-ext_edit_board-confirm_add")

  app$wait_for_js("document.getElementById('my_board-block_handle-a') !== null")
  app$wait_for_idle()

  after_gesture <- app$get_value(export = "commit_count")

  # The gesture committed, and by a bounded amount -- not a runaway. The exact
  # count varies with whether real dockview echoes the placement (CI) or not
  # (headless), so the assertion is the bound, not an exact value.
  expect_gt(after_gesture, baseline)
  expect_lte(after_gesture - baseline, 8L)

  # Quiescence: with no further input, the board settles and commits nothing
  # more. A layout loop would keep the count climbing here.
  app$wait_for_idle()
  at_rest <- app$get_value(export = "commit_count")

  expect_identical(at_rest, after_gesture)
})

test_that("the authored layout round-trips to its stored form", {

  skip_on_cran()

  app <- new_app_driver(
    system.file("examples", "sized-grid", "app.R", package = "blockr.dock"),
    name = "roundtrip-sentinel",
    seed = 42,
    load_timeout = 30 * 1000,
    timeout = 30 * 1000
  )
  withr::defer(app$stop())

  # `roundtrip_stable` is NA until every view has reported a live layout; real
  # dockView only runs against Chrome in CI, so this is where the property that
  # justifies the settled-echo restore -- the authored grid equals its echo's
  # projection -- is actually exercised, not just its abstract idempotence.
  app$wait_for_idle()

  expect_true(isTRUE(app$get_value(export = "roundtrip_stable")))
})
