test_that("serve utils", {

  board <- new_dock_board()

  expect_s3_class(
    blockr_app_options(board),
    "board_options"
  )

  expect_s3_class(
    blockr_app_ui(
      "test",
      board,
      blockr_app_plugins(board),
      blockr_app_options(board)
    ),
    "shiny.tag.list"
  )

  testServer(
    blockr_app_server,
    session$flushReact(),
    args = list(
      board,
      blockr_app_plugins(board),
      blockr_app_options(board)
    )
  )
})

test_that("dock app renders a block added via the extension (#191)", {

  skip_on_cran()

  app <- shinytest2::AppDriver$new(
    system.file("examples", "empty", "app.R", package = "blockr.dock"),
    name = "dock",
    seed = 42,
    load_timeout = 30 * 1000,
    timeout = 30 * 1000
  )
  withr::defer(app$stop())

  # The Edit board panel is active on load, so a plain click adds the block.
  app$set_inputs(
    `my_board-edit_board_extension-registry_select` = "dataset_block",
    `my_board-edit_board_extension-block_id` = "a"
  )
  app$click("my_board-edit_board_extension-confirm_add")

  # The block card carries a stable `block_handle-<id>` DOM id; its presence
  # is the add-block -> dock-render seam -- the board update mounting a panel.
  app$wait_for_js("document.getElementById('my_board-block_handle-a') !== null")

  expect_equal(
    app$get_js(
      "document.querySelectorAll('[id^=\"my_board-block_handle-\"]').length"
    ),
    1
  )
})

test_that("edit board extension links blocks (e2e)", {

  skip_on_cran()

  # A board pre-seeded with a source and a transform block so the test drives
  # only link operations -- adding blocks would deactivate the extension panel
  # and race shinytest2 (see apps/edit-link/app.R, mirroring edit-stacks).
  app <- shinytest2::AppDriver$new(
    test_path("apps", "edit-link"),
    name = "edit-link",
    seed = 42,
    load_timeout = 30 * 1000
  )

  set_in(app, "new_link_id", "ab")
  click(app, "add_link")
  app$wait_for_idle()
  wait_bound(app, "ab_from")

  set_in(app, "ab_from", "a")
  app$wait_for_idle()
  set_in(app, "ab_to", "b")
  app$wait_for_idle()
  set_in(app, "ab_input", "data")
  app$wait_for_idle()

  click(app, "apply_changes")
  app$wait_for_idle()

  expect_identical(field(app, "ab_from"), "a")
  expect_identical(field(app, "ab_to"), "b")
  expect_identical(field(app, "ab_input"), "data")

  app$stop()
})

test_that("edit board extension stacks (e2e)", {

  skip_on_cran()

  app <- shinytest2::AppDriver$new(
    test_path("apps", "edit-stacks"),
    name = "edit-stacks",
    seed = 42,
    load_timeout = 30 * 1000
  )

  set_in(app, "new_stack_id", "grp")
  click(app, "add_stack")
  app$wait_for_idle()
  wait_bound(app, "grp_name")

  set_in(app, "grp_name", "Group A")
  set_in(app, "grp_blocks", "data")
  set_color(app, "grp_color", "#aabbcc")
  app$wait_for_idle()

  click(app, "apply_changes")
  app$wait_for_idle()

  expect_identical(field(app, "grp_name"), "Group A")
  expect_identical(tolower(field(app, "grp_color")), "#aabbcc")

  set_color(app, "grp_color", "#112233")
  app$wait_for_idle()
  click(app, "apply_changes")
  app$wait_for_idle()

  expect_identical(tolower(field(app, "grp_color")), "#112233")

  app$run_js(
    paste0(
      "Shiny.setInputValue('", nsid("stacks_dt_rows_selected"),
      "', [1], {priority: 'event'});"
    )
  )
  app$wait_for_idle()
  click(app, "rm_stack")
  app$wait_for_idle()
  click(app, "apply_changes")
  app$wait_for_idle()

  expect_null(field(app, "grp_name"))

  app$stop()
})

test_that("multi-view nav renders one labelled entry per view (#189)", {

  skip_on_cran()

  app <- shinytest2::AppDriver$new(
    system.file("examples", "multi-view", "app.R", package = "blockr.dock"),
    name = "multi-view",
    seed = 42,
    load_timeout = 30 * 1000,
    timeout = 20 * 1000
  )
  withr::defer(app$stop())

  app$wait_for_idle()

  # The bug rendered 2N entries: board_ui drew N items statically and the
  # reconcile pass re-added each as a blank-labelled duplicate sharing the
  # same id. Asserting on the composed DOM is what a unit test can't reach.
  nav <- read_view_nav(app)

  expect_identical(nrow(nav), 2L)
  expect_false(anyDuplicated(nav$id) > 0L)
  expect_setequal(nav$label, c("First", "Second"))
  expect_identical(nav$label[nav$active], "First")

  # Drive a runtime add through the nav UI: the client `add` handler must
  # render the new view once, correctly labelled.
  app$run_js(
    "document.querySelector('#my_board-view_nav .blockr-view-add').click()"
  )
  app$wait_for_idle()

  app$set_inputs(`my_board-view_new_name` = "Third")
  app$click("my_board-confirm_view_add")
  app$wait_for_idle()

  nav <- read_view_nav(app)

  expect_identical(nrow(nav), 3L)
  expect_false(anyDuplicated(nav$id) > 0L)
  expect_true("Third" %in% nav$label)
  expect_false(any(nav$label == ""))
})
