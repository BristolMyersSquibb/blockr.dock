# End-to-end sentinel for the loop-safety budget. The commit probe -- a hidden,
# suspend-exempt readout of the monotonic board-commit count, live only when a
# budget is set -- lets a browser test observe the "one settled gesture, a
# bounded number of commits, and none after quiescence" contract through the
# full server stack, the seam no unit test sees. A reconciliation loop would
# either never settle (wait_for_idle times out) or keep incrementing the count
# after the gesture -- both fail here.

read_commit_probe <- function(app) {
  txt <- app$get_js(
    paste0(
      "(function(){var e=document.getElementById('my_board-commit_probe');",
      "return e ? e.textContent : null;})()"
    )
  )
  as.integer(txt)
}

test_that("a settled gesture commits a bounded amount, quiescence adds none", {

  skip_on_cran()

  app <- shinytest2::AppDriver$new(
    system.file("examples", "commit-probe", "app.R", package = "blockr.dock"),
    name = "commit-sentinel",
    seed = 42,
    load_timeout = 30 * 1000,
    timeout = 30 * 1000
  )
  withr::defer(app$stop())

  app$wait_for_js("document.getElementById('my_board-commit_probe') !== null")
  app$wait_for_idle()

  baseline <- read_commit_probe(app)
  expect_false(is.na(baseline))

  # One gesture: add a block through the edit-board extension (its panel is
  # active on load). The board update mounts the panel -- a stable
  # block_handle-<id> DOM id marks the commit landing.
  app$set_inputs(
    `my_board-edit_board_extension-registry_select` = "dataset_block",
    `my_board-edit_board_extension-block_id` = "a"
  )
  app$click("my_board-edit_board_extension-confirm_add")

  app$wait_for_js("document.getElementById('my_board-block_handle-a') !== null")
  app$wait_for_idle()

  after_gesture <- read_commit_probe(app)

  # The gesture committed, and by a bounded amount -- not a runaway. The exact
  # count varies with whether real dockview echoes the placement (CI) or not
  # (headless), so the assertion is the bound, not an exact value.
  expect_gt(after_gesture, baseline)
  expect_lte(after_gesture - baseline, 8L)

  # Quiescence: with no further input, the board settles and commits nothing
  # more. A layout loop would keep the count climbing here.
  app$wait_for_idle()
  at_rest <- read_commit_probe(app)

  expect_identical(at_rest, after_gesture)
})

test_that("the authored layout round-trips to its stored form", {

  skip_on_cran()

  app <- shinytest2::AppDriver$new(
    system.file(
      "examples", "commit-probe-seeded", "app.R", package = "blockr.dock"
    ),
    name = "roundtrip-sentinel",
    seed = 42,
    load_timeout = 30 * 1000,
    timeout = 30 * 1000
  )
  withr::defer(app$stop())

  el <- "document.getElementById('my_board-roundtrip_stable')"

  app$wait_for_js(paste0(el, " !== null"))

  # The flag is empty until every view has reported its live layout (view_data
  # is all-or-nothing); wait for it to resolve, then assert stability. Real
  # dockview only runs against Chrome in CI, so this is where the property that
  # justifies the server-echo skip -- authored form == projection of its echo --
  # is actually exercised, not just its abstract idempotence.
  app$wait_for_js(paste0(el, ".textContent.length > 0"))
  app$wait_for_idle()

  expect_identical(app$get_js(paste0(el, ".textContent")), "TRUE")
})
