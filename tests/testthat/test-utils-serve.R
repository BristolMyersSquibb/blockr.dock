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

test_that("dock app", {

  skip_on_cran()

  app <- shinytest2::AppDriver$new(
    system.file("examples", "empty", "app.R", package = "blockr.dock"),
    name = "dock",
    seed = 42,
    load_timeout = 30 * 1000
  )

  app$set_inputs(
    `my_board-edit_board_extension-registry_select` = "dataset_block",
    `my_board-edit_board_extension-block_id` = "a"
  )

  app$click("my_board-edit_board_extension-confirm_add")

  app$wait_for_idle()
  app$expect_values(export = TRUE, screenshot_args = FALSE)

  app$set_inputs(
    `my_board-edit_board_extension-registry_select` = "head_block",
    `my_board-edit_board_extension-block_id` = "b"
  )

  app$click("my_board-edit_board_extension-confirm_add")

  app$wait_for_idle()
  app$expect_values(export = TRUE, screenshot_args = FALSE)

  app$set_inputs(`my_board-edit_board_extension-new_link_id` = "ab")

  app$click("my_board-edit_board_extension-add_link")
  app$wait_for_idle()

  app$set_inputs(`my_board-edit_board_extension-ab_from` = "a")
  app$wait_for_idle()

  app$set_inputs(`my_board-edit_board_extension-ab_to` = "b")
  app$wait_for_idle()

  app$set_inputs(`my_board-edit_board_extension-ab_input` = "data")
  app$wait_for_idle()

  app$click("my_board-edit_board_extension-modify_links")

  app$wait_for_idle()
  app$expect_values(export = TRUE, screenshot_args = FALSE)

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
