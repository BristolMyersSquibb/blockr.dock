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

test_that("adding a second block keeps both block panels (#196)", {

  skip_on_cran()

  app <- shinytest2::AppDriver$new(
    system.file("examples", "empty", "app.R", package = "blockr.dock"),
    name = "panel-visibility",
    seed = 42,
    load_timeout = 30 * 1000,
    timeout = 20 * 1000
  )
  withr::defer(app$stop())

  app$wait_for_idle()

  add_block <- function(registry, id) {
    app$set_inputs(
      `my_board-edit_board_extension-registry_select` = registry,
      `my_board-edit_board_extension-block_id` = id
    )
    app$click("my_board-edit_board_extension-confirm_add")
    app$wait_for_idle()
  }

  add_block("dataset_block", "a")
  expect_identical(block_panel_tabs(app), "block_panel-a")

  # Pre-fix, the second add fired reconcile_views against a board_layouts that
  # lagged the live dock, restoring it and wiping both block panels -- leaving
  # only the extension (#196). Both block tabs must survive.
  add_block("head_block", "b")
  expect_identical(block_panel_tabs(app), c("block_panel-a", "block_panel-b"))

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

test_that("appending a block via its context menu adds a linked panel", {

  skip_on_cran()

  app <- shinytest2::AppDriver$new(
    test_path("apps", "append-block"),
    name = "append-block",
    seed = 42,
    load_timeout = 30 * 1000,
    timeout = 20 * 1000
  )
  withr::defer(app$stop())

  app$wait_for_idle()

  expect_identical(block_panel_tabs(app), "block_panel-a")

  # Open the source block's context menu and fire "Append block", which opens
  # the pre-rendered append browser in the right sidebar.
  app$run_js(
    paste0(
      "document.querySelector('#my_board-block_handle-a ",
      ".blockr-header-icon').click()"
    )
  )
  app$click("my_board-block_a-edit_block-append_block")
  app$wait_for_idle()

  expect_true(
    app$get_js(
      paste0(
        "document.getElementById('my_board-append_block_sidebar')",
        ".classList.contains('blockr-sidebar-open')"
      )
    )
  )

  # Clicking a catalogue card quick-adds that block with default settings,
  # firing the browser's client-side commit binding that the append action
  # observes.
  app$run_js(
    paste0(
      "document.querySelector('#my_board-append_block_action-browser-commit ",
      ".blockr-block-browser-card[data-block-type=\"head_block\"] ",
      ".blockr-block-browser-card-name').click()"
    )
  )
  app$wait_for_idle()

  tabs <- block_panel_tabs(app)

  expect_length(tabs, 2L)
  expect_true("block_panel-a" %in% tabs)

  new_id <- sub("^block_panel-", "", setdiff(tabs, "block_panel-a"))

  # Append, unlike a plain add, also wires a link from the source into the new
  # block. The link is proven through data flow: the appended head block
  # computes head(iris) -- six rows of the source's five columns -- which is
  # reachable only if it received `a`'s output.
  results <- app$get_value(export = "result")

  expect_setequal(names(results), c("a", new_id))
  expect_s3_class(results[[new_id]], "data.frame")
  expect_identical(dim(results[[new_id]]), c(6L, 5L))
})
