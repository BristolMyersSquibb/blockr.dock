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
  # and race shinytest2. The same bare fixture serves the stacks test below.
  app <- shinytest2::AppDriver$new(
    system.file("examples", "edit-add", "app.R", package = "blockr.dock"),
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
    system.file("examples", "edit-add", "app.R", package = "blockr.dock"),
    name = "edit-stacks",
    seed = 42,
    load_timeout = 30 * 1000
  )

  set_in(app, "new_stack_id", "grp")
  click(app, "add_stack")
  app$wait_for_idle()
  wait_bound(app, "grp_name")

  set_in(app, "grp_name", "Group A")
  set_in(app, "grp_blocks", "a")
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

test_that("a board survives the live Export/Import round-trip (#233)", {

  skip_on_cran()

  app <- shinytest2::AppDriver$new(
    system.file("examples", "serdes", "app.R", package = "blockr.dock"),
    name = "serdes",
    seed = 42,
    load_timeout = 40 * 1000,
    timeout = 30 * 1000
  )
  withr::defer(app$stop())

  wait_dock_loaded(app, n_blocks = 3)
  before <- read_dock_state(app)

  # The fixture seeds the dock-owned state the round-trip must preserve: two
  # named views with a non-default active view, plus three blocks.
  expect_setequal(before$nav$label, c("Overview", "Analysis"))
  expect_identical(before$nav$label[before$nav$active], "Analysis")
  expect_identical(before$active_view, "analysis")
  expect_identical(before$blocks, c("a", "b", "c"))

  # Export through the live download handler, then assert the server-produced
  # artifact carries the dock-owned state the DOM does not surface without the
  # dockview client -- the extension, the panel-level layout, the producer
  # version that routes deserialization -- alongside blocks, links and stacks.
  path <- app$get_download("my_board-preserve_board-serialize")
  expect_gt(file.size(path), 0)

  ser <- jsonlite::fromJSON(path, simplifyDataFrame = FALSE,
                            simplifyMatrix = FALSE)
  expect_identical(
    ser$constructor$version,
    as.character(utils::packageVersion("blockr.dock"))
  )

  restored <- blockr_deser(ser)
  expect_setequal(board_block_ids(restored), c("a", "b", "c"))
  expect_identical(board_link_ids(restored), "ab")
  expect_setequal(names(board_stacks(restored)), "grp")
  expect_length(dock_extensions(restored), 1L)

  ly <- board_layouts(restored)
  expect_identical(unname(view_names(ly)), c("Overview", "Analysis"))
  expect_identical(active_view(ly), "analysis")
  expect_setequal(
    layout_panel_ids(ly[["analysis"]]),
    c("ext_panel-edit_board_extension", "block_panel-a", "block_panel-b")
  )

  # Import the saved file. Restoring reloads the session: the probe, wiped by
  # the reload, both waits for and proves the reload fired.
  app$run_js("window.__serdes_probe = true;")
  app$upload_file(`my_board-preserve_board-restore` = path, wait_ = FALSE)
  app$wait_for_js("typeof window.__serdes_probe === 'undefined'",
                  timeout = 30 * 1000)

  wait_dock_loaded(app, n_blocks = 3)

  # The deserialize + reconcile + re-render rebuilds the dock-owned view
  # structure and the blocks identically.
  expect_identical(read_dock_state(app), before)
})

test_that("deleting a block via its card menu drops the panel and its link", {

  skip_on_cran()

  app <- shinytest2::AppDriver$new(
    system.file("examples", "edit-board", "app.R", package = "blockr.dock"),
    name = "remove-block",
    seed = 42,
    load_timeout = 30 * 1000,
    timeout = 20 * 1000
  )
  withr::defer(app$stop())

  app$wait_for_idle()

  expect_identical(block_panel_tabs(app), c("block_panel-a", "block_panel-b"))
  expect_identical(field(app, "ab_from"), "a")

  # The card's "Delete block" dropdown item sets this input (immediate, no
  # browser); fire it directly so the test does not depend on b's panel being
  # the front tab -- its card is detached from the DOM while inactive.
  app$run_js(
    paste0(
      "Shiny.setInputValue(",
      "'my_board-block_b-edit_block-delete_block', 1, {priority: 'event'});"
    )
  )
  app$wait_for_idle()

  # The board update removes b's dock panel and cascade-removes the dependent
  # link, whose row then leaves the extension's links table.
  expect_identical(block_panel_tabs(app), "block_panel-a")
  expect_null(field(app, "ab_from"))
})

test_that("removing a link via the edit extension updates the board", {

  skip_on_cran()

  app <- shinytest2::AppDriver$new(
    system.file("examples", "edit-board", "app.R", package = "blockr.dock"),
    name = "remove-link",
    seed = 42,
    load_timeout = 30 * 1000
  )
  withr::defer(app$stop())

  app$wait_for_idle()
  expect_identical(field(app, "ab_from"), "a")

  app$run_js(
    paste0(
      "Shiny.setInputValue('", nsid("links_dt_rows_selected"),
      "', [1], {priority: 'event'});"
    )
  )
  app$wait_for_idle()

  click(app, "rm_link")
  app$wait_for_idle()
  click(app, "apply_changes")
  app$wait_for_idle()

  expect_null(field(app, "ab_from"))
})

test_that("removing a stack via the edit extension updates the board", {

  skip_on_cran()

  app <- shinytest2::AppDriver$new(
    system.file("examples", "edit-board", "app.R", package = "blockr.dock"),
    name = "remove-stack",
    seed = 42,
    load_timeout = 30 * 1000
  )
  withr::defer(app$stop())

  app$wait_for_idle()
  expect_identical(field(app, "grp_name"), "Group A")

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
})
