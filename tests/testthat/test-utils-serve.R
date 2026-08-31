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

test_that("resolve_url_view matches the ?view= param to a view id", {

  views <- board_views(
    new_dock_board(
      blocks = c(a = new_dataset_block(), b = new_dataset_block()),
      views = list(First = blk("a"), Second = blk("b")),
      active = "First"
    )
  )

  expect_identical(resolve_url_view(views, list(view = "Second")), "Second")
  expect_identical(resolve_url_view(views, list(view = "First")), "First")

  # Absent, empty, or unknown all decline to select.
  expect_null(resolve_url_view(views, list(other = "1")))
  expect_null(resolve_url_view(views, list(view = "")))
  expect_null(resolve_url_view(views, list(view = "nope")))
  expect_null(resolve_url_view(views, list()))
})

test_that("apply_url_view opens the board on the ?view= view (#323)", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_dataset_block()),
    views = list(First = blk("a"), Second = blk("b")),
    active = "First"
  )

  # The default active view is "First", so flipping to "Second" is a real
  # (non-vacuous) change driven purely by the query param.
  expect_identical(active_view(brd), "First")
  expect_identical(
    active_view(apply_url_view(brd, list(view = "Second"))),
    "Second"
  )

  # An unknown id or an absent param leaves the default active.
  expect_identical(
    active_view(apply_url_view(brd, list(view = "nope"))),
    "First"
  )
  expect_identical(active_view(apply_url_view(brd, list())), "First")
})

test_that("blockr_app_ui threads the ?view= query into the GET nav (#357)", {

  skip_if_not_installed("xml2")

  board <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_dataset_block()),
    views = list(First = blk("a"), Second = blk("b")),
    active = "First"
  )

  item_xpath <- sprintf(
    "//*[contains(concat(' ', normalize-space(@class), ' '), ' %s ')]",
    "blockr-view-item"
  )

  active_nav_id <- function(query) {

    ui <- blockr_app_ui(
      "test", board, blockr_app_plugins(board), blockr_app_options(board),
      query = query
    )

    items <- xml2::xml_find_all(xml2::read_html(as.character(ui)), item_xpath)
    is_active <- grepl("(^| )active( |$)", xml2::xml_attr(items, "class"))

    xml2::xml_attr(items, "data-view-id")[is_active]
  }

  # Absent the param, the GET nav highlights the board's default active view.
  expect_identical(active_nav_id(list()), "First")

  # `?view=Second` pre-sets active_view before board_ui, so the GET nav
  # highlights Second -- the query is consumed by the method, not splatted into
  # page_fillable (without the `query` formal it would leak through `...`).
  expect_identical(active_nav_id(list(view = "Second")), "Second")
})

test_that("grids_stable holds when the live grid is the stored fixed point", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_dataset_block()),
    grids = list(V = dock_grid("a", "b", sizes = c(0.3, 0.7)))
  )

  stored <- board_grids(brd)

  # A non-default authored grid is stored non-NULL, so the comparison is not
  # vacuously true.
  expect_false(is.null(stored[["V"]]))

  # The live grid the client echoes, when it matches the stored grid, is stable.
  expect_true(grids_stable(stored, stored))

  # Sub-tolerance size jitter is still the fixed point -- grids_stable uses the
  # mirror's own all.equal(tolerance = grid_size_tol()), so it doesn't commit.
  jittered <- new_dock_grids(
    list(
      V = dock_grid("block_panel-a", "block_panel-b", sizes = c(0.301, 0.699))
    )
  )
  expect_true(grids_stable(stored, jittered))

  # A live grid whose sizes drifted past the tolerance is not the fixed point.
  drifted <- new_dock_grids(
    list(
      V = dock_grid("block_panel-a", "block_panel-b", sizes = c(0.85, 0.15))
    )
  )
  expect_false(grids_stable(stored, drifted))
})

test_that("grids_stable reads a squeezed rail as the fixed point (#457)", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_dataset_block()),
    views = list(V = c("a", "b")),
    grids = list(
      V = dock_grid("a", rail(blk("b"), position = "right", size = 420))
    )
  )

  stored <- board_grids(brd)

  # A viewport too narrow to render the rail reports a width the layout forced,
  # which the mirror declines to commit -- so the round trip is settled, and the
  # sentinel has to say so rather than reading the divergence as a pending
  # write. Anything else in the grid still moves it off the fixed point.
  squeezed <- stored[["V"]]
  squeezed[["rails"]][["right"]][["size"]] <- 120

  expect_true(grids_stable(stored, new_dock_grids(list(V = squeezed))))

  moved <- squeezed
  moved[["rails"]][["right"]][["collapsed"]] <- TRUE

  expect_false(grids_stable(stored, new_dock_grids(list(V = moved))))
})

test_that("dock app renders a block added via the extension (#191)", {

  skip_on_cran()

  app <- new_app_driver(
    system.file("examples", "empty", "app.R", package = "blockr.dock"),
    name = "dock",
    seed = 42,
    load_timeout = 30 * 1000,
    timeout = 30 * 1000
  )
  withr::defer(app$stop())

  # The Edit board panel is active on load, so a plain click adds the block.
  app$set_inputs(
    `my_board-ext_edit_board-registry_select` = "dataset_block",
    `my_board-ext_edit_board-block_id` = "a"
  )
  app$click("my_board-ext_edit_board-confirm_add")

  # The add-block -> dock-render seam is the panel mounting into the dock, with
  # its tab client-confirmed; the block card then lives inside that panel. The
  # `block_handle-<id>` card alone is inserted into the offcanvas pool ahead of
  # (and regardless of) the mount, so the tab is the signal that it landed.
  wait_block_panel_tabs(app, "block_panel-a")

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
  app <- new_app_driver(
    system.file("examples", "edit-add", "app.R", package = "blockr.dock"),
    name = "edit-link",
    seed = 42,
    load_timeout = 30 * 1000
  )
  withr::defer(app$stop())

  set_in(app, "new_link_id", "ab")
  click(app, "add_link")
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
})

test_that("adding a second block keeps both block panels (#196)", {

  skip_on_cran()

  app <- new_app_driver(
    system.file("examples", "empty", "app.R", package = "blockr.dock"),
    name = "panel-visibility",
    seed = 42,
    load_timeout = 30 * 1000,
    timeout = 30 * 1000
  )
  withr::defer(app$stop())

  wait_bound(app, "registry_select")

  # Each add leaves the extension's own re-seed of the id box in flight, which
  # a later one would otherwise stage its id against; take it here so the
  # helper hands back a field no pending message still writes to.
  add_block <- function(registry, id) {
    app$set_inputs(
      `my_board-ext_edit_board-registry_select` = registry,
      `my_board-ext_edit_board-block_id` = id
    )
    app$click("my_board-ext_edit_board-confirm_add")
    wait_reseeded(app, id)
  }

  add_block("dataset_block", "a")
  wait_block_panel_tabs(app, "block_panel-a")
  expect_identical(block_panel_tabs(app), "block_panel-a")

  # Pre-fix, the second add fired reconcile_views against a board that
  # lagged the live dock, restoring it and wiping both block panels -- leaving
  # only the extension (#196). Both block tabs must survive; gating on the
  # settled strip first makes the assertion deterministic (a permanent wipe
  # never reaches the target set, so the wait times out and still catches it).
  add_block("head_block", "b")
  wait_block_panel_tabs(app, c("block_panel-a", "block_panel-b"))
  expect_identical(block_panel_tabs(app), c("block_panel-a", "block_panel-b"))
})

test_that("edit board extension stacks (e2e)", {

  skip_on_cran()

  app <- new_app_driver(
    system.file("examples", "edit-add", "app.R", package = "blockr.dock"),
    name = "edit-stacks",
    seed = 42,
    load_timeout = 30 * 1000
  )
  withr::defer(app$stop())

  set_in(app, "new_stack_id", "grp")
  click(app, "add_stack")
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
})

test_that("multi-view nav renders one labelled entry per view (#189)", {

  skip_on_cran()

  app <- new_app_driver(
    system.file("examples", "multi-view", "app.R", package = "blockr.dock"),
    name = "multi-view",
    seed = 42,
    load_timeout = 30 * 1000,
    timeout = 20 * 1000
  )
  withr::defer(app$stop())

  wait_view_nav(app, 2)

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
  wait_view_nav(app, 3)

  nav <- read_view_nav(app)

  expect_identical(nrow(nav), 3L)
  expect_false(anyDuplicated(nav$id) > 0L)
  expect_true("Third" %in% nav$label)
  expect_false(any(nav$label == ""))
})

test_that("the view nav does not report the server's own push back (#424)", {

  skip_on_cran()

  app <- new_app_driver(
    system.file("examples", "multi-view", "app.R", package = "blockr.dock"),
    name = "view-nav-echo",
    seed = 42,
    load_timeout = 30 * 1000,
    timeout = 20 * 1000
  )
  withr::defer(app$stop())

  wait_view_nav(app, 2)

  nav <- read_view_nav(app)
  first <- nav$id[nav$label == "First"]
  second <- nav$id[nav$label == "Second"]

  watch_view_nav(app)

  # A click reports once, and only once. `switch_active_view()` answers it by
  # pushing the new active view back with `sendInputMessage("view_nav", ...)`,
  # and that push used to arrive as a second report -- the loop's fuel: with
  # two switches in flight the first push's echo lands after the second has
  # been applied, so it misses `switch_view_observer()`'s `client_active` guard
  # and is taken for a fresh switch, whose own push echoes in turn. Waiting on
  # the push means an echo of it would already be in the tally.
  click_view(app, second)
  wait_view_nav_push(app, second)

  expect_identical(view_nav_reports(app), second)

  # The push alone, delivered as the server sends it. It must move the nav and
  # report nothing at all.
  watch_view_nav(app)
  push_view_nav(app, sprintf('{"value": "%s"}', first))

  expect_identical(view_nav_reports(app), character())

  nav <- read_view_nav(app)
  expect_identical(nav$id[nav$active], first)

  # An `add` push must not move the active view either. The server owns that
  # and says so with a `value` message when an add means to navigate; a
  # client-side activation would report a view the board never switched to --
  # and with the echo gone, nothing would correct it.
  watch_view_nav(app)
  push_view_nav(app, '{"add": {"id": "ghost", "name": "Ghost"}}')

  expect_identical(view_nav_reports(app), character())

  nav <- read_view_nav(app)
  expect_identical(nrow(nav), 3L)
  expect_identical(nav$id[nav$active], first)
})

test_that("a click on the view the server left still switches (#424)", {

  skip_on_cran()

  app <- new_app_driver(
    system.file("examples", "multi-view", "app.R", package = "blockr.dock"),
    name = "view-nav-forget",
    seed = 42,
    load_timeout = 30 * 1000,
    timeout = 20 * 1000
  )
  withr::defer(app$stop())

  wait_view_nav(app, 2)

  nav <- read_view_nav(app)
  second <- nav$id[nav$label == "Second"]

  # Second becomes the last value the client sent, and so the one Shiny's
  # no-resend dedup holds for the nav.
  click_view(app, second)
  wait_view_handle(app, second)

  # Add a third view. The server activates it and pushes that back, so the
  # board leaves Second without the client having sent anything -- the dedup's
  # cached value is now stale. Dropping the echo is what makes it stale, so
  # `receiveMessage()` clears it with `forgetLastInputValue()`.
  app$run_js(
    "document.querySelector('#my_board-view_nav .blockr-view-add').click()"
  )
  wait_js(
    app,
    paste0(
      "(function(){var e=document.getElementById('my_board-view_new_name');",
      "return e !== null && e.classList.contains('shiny-bound-input');})()"
    ),
    function() dock_shell_diag(app, "my_board")
  )

  app$set_inputs(`my_board-view_new_name` = "Third")
  app$click("my_board-confirm_view_add")
  wait_view_nav(app, 3)

  nav <- read_view_nav(app)
  expect_identical(nav$label[nav$active], "Third")

  # Clicking Second again has to reach the server. Left cached, the dedup
  # would swallow the report as a repeat and the board would sit on Third.
  click_view(app, second)
  wait_view_dock_active(app, second)

  docks <- read_view_docks(app)
  expect_identical(docks$id[docks$active], second)
})

test_that("a board survives the live Export/Import round-trip (#233)", {

  skip_on_cran()

  app <- new_app_driver(
    system.file("examples", "serdes", "app.R", package = "blockr.dock"),
    name = "serdes",
    seed = 42,
    load_timeout = 40 * 1000,
    timeout = 30 * 1000
  )
  withr::defer(app$stop())

  # Only the active view's block cards are built at startup; off-screen views
  # defer to first visit. So the DOM surfaces the two cards of the active
  # Analysis view (a, b), not c -- which lives in the off-screen Overview view.
  # The full three-block board is asserted against the exported artifact below.
  wait_dock_loaded(app, n_blocks = 2)

  # wait_dock_loaded gates on the server-rendered cards; the dockview client
  # restores the a/b tab group asynchronously. Wait for it to settle (b fronted,
  # a a hidden back tab) before reading the exported grid, so the read sees the
  # restored layout the mirror commits rather than racing the async restore.
  wait_active_block_tabs(app, "analysis", "block_panel-b")

  before <- read_dock_state(app)

  # The fixture seeds the dock-owned state the round-trip must preserve: two
  # named views with a non-default active view, plus three blocks.
  expect_setequal(before$nav$label, c("Overview", "Analysis"))
  expect_identical(before$nav$label[before$nav$active], "Analysis")
  expect_identical(before$active_view, "analysis")
  expect_identical(before$blocks, c("a", "b"))

  # Export through the live download handler, then assert the server-produced
  # artifact carries the dock-owned state the DOM does not surface without the
  # dockview client -- the extension, the panel-level layout, the producer
  # version that routes deserialization -- alongside blocks, links and stacks.
  path <- retry_download(app, "my_board-preserve_board-serialize")
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

  views <- board_views(restored)
  expect_identical(unname(view_names(views)), c("Overview", "Analysis"))
  expect_identical(active_view(views), "analysis")
  expect_setequal(
    view_members(views[["analysis"]]),
    c("ext_panel-edit_board", "block_panel-a", "block_panel-b")
  )

  # Grid geometry survives deserialization, not just membership: the analysis
  # view restores its authored a/b tab group with b fronted (a is the hidden
  # back tab), so only b reads as visible.
  expect_setequal(visible_block_ids(active_view_grid(restored)), "b")

  # Import the saved file. Restoring reloads the session: the probe, wiped by
  # the reload, both waits for and proves the reload fired.
  app$run_js("window.__serdes_probe = true;")
  app$upload_file(`my_board-preserve_board-restore` = path, wait_ = FALSE)
  app$wait_for_js("typeof window.__serdes_probe === 'undefined'",
                  timeout = 30 * 1000)

  # The reload restores Analysis as the active view, so again only its two
  # cards are built (c stays deferred with the off-screen Overview view).
  wait_dock_loaded(app, n_blocks = 2)
  wait_active_block_tabs(app, "analysis", "block_panel-b")

  # The deserialize + reconcile + re-render rebuilds the dock-owned view
  # structure and the blocks identically.
  expect_identical(read_dock_state(app), before)

  # The restored board carries the per-view grid forward, not just the nav and
  # blocks: re-exporting after the reload reproduces the stored geometry -- the
  # tab group and its active tab exactly, the split sizes within the sash
  # tolerance. This reads the committed board's slots -- proving the stage /
  # reload cycle preserved them and that the fixture re-importing its own
  # (colliding) view ids does not drop them. It cannot see the client render, so
  # that leg is asserted below.
  #
  # The grid compare is `all.equal(tolerance = grid_size_tol(), scale = 1)`:
  # structure and the active tab match exactly, the sizes within the sash
  # tolerance. dockView renders sash sizes with sub-pixel run-to-run jitter, so
  # the first-load export and the post-import re-export land the ratios a
  # fraction apart -- below what the mirror commits as a change, not a layout
  # difference. The `views` slot carries no geometry, so it round-trips
  # byte-for-byte.
  #
  # get_download can transiently fail after the reload -- the link's href is
  # filled only once outputs bind, and the download endpoint may briefly not
  # answer ("Unable request data from server"); retry_download absorbs both.
  path2 <- retry_download(app, "my_board-preserve_board-serialize")
  ser2 <- jsonlite::fromJSON(path2, simplifyDataFrame = FALSE,
                             simplifyMatrix = FALSE)

  expect_true(
    isTRUE(
      all.equal(
        drop_focus(ser2[["payload"]][["grids"]]),
        drop_focus(ser[["payload"]][["grids"]]),
        tolerance = grid_size_tol(),
        scale = 1
      )
    )
  )
  expect_identical(ser2[["payload"]][["views"]], ser[["payload"]][["views"]])

  # Client leg: the byte checks read the stored slots, so they cannot observe
  # whether the dockview client actually applied the grid. Assert one rendered
  # DOM fact -- the analysis view's a/b tab group restores with `b` as the front
  # tab (its authored active tab, not `a` the first). The post-import settle
  # wait above already held for exactly this state, so a client-side
  # restore_layout failure or a collapse to separate leaves surfaces there.
  expect_identical(active_block_panel_tabs(app, "analysis"), "block_panel-b")
})

test_that("deleting a block via its card menu drops the panel and its link", {

  skip_on_cran()

  app <- new_app_driver(
    system.file("examples", "edit-board", "app.R", package = "blockr.dock"),
    name = "remove-block",
    seed = 42,
    load_timeout = 30 * 1000,
    timeout = 30 * 1000
  )
  withr::defer(app$stop())

  # Two independent client settlements: dockview mounts the panel tabs, and the
  # extension's links table renders and binds its cell inputs. Neither implies
  # the other, so gate on both before reading them.
  wait_block_panel_tabs(app, c("block_panel-a", "block_panel-b"))
  wait_bound(app, "ab_from")
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

  # The board update removes b's dock panel and cascade-removes the dependent
  # link, whose row then leaves the extension's links table. One update, but
  # again two settlements landing on their own schedules -- the panel drop is
  # client-confirmed by the tab going, the row removal by the cell input
  # leaving the DOM.
  wait_block_panel_tabs(app, "block_panel-a")
  wait_gone(app, "ab_from")
  expect_identical(block_panel_tabs(app), "block_panel-a")
  expect_null(field(app, "ab_from"))
})

test_that("removing a link via the edit extension updates the board", {

  skip_on_cran()

  app <- new_app_driver(
    system.file("examples", "edit-board", "app.R", package = "blockr.dock"),
    name = "remove-link",
    seed = 42,
    load_timeout = 30 * 1000
  )
  withr::defer(app$stop())

  wait_bound(app, "ab_from")
  expect_identical(field(app, "ab_from"), "a")

  app$run_js(
    paste0(
      "Shiny.setInputValue('", nsid("links_dt_rows_selected"),
      "', [1], {priority: 'event'});"
    )
  )
  wait_enabled(app, "rm_link")

  click(app, "rm_link", wait = FALSE)
  wait_enabled(app, "apply_changes")
  click(app, "apply_changes")
  wait_gone(app, "ab_from")

  expect_null(field(app, "ab_from"))
})

test_that("removing a stack via the edit extension updates the board", {

  skip_on_cran()

  app <- new_app_driver(
    system.file("examples", "edit-board", "app.R", package = "blockr.dock"),
    name = "remove-stack",
    seed = 42,
    load_timeout = 30 * 1000
  )
  withr::defer(app$stop())

  wait_bound(app, "grp_name")
  expect_identical(field(app, "grp_name"), "Group A")

  app$run_js(
    paste0(
      "Shiny.setInputValue('", nsid("stacks_dt_rows_selected"),
      "', [1], {priority: 'event'});"
    )
  )
  wait_enabled(app, "rm_stack")

  click(app, "rm_stack", wait = FALSE)
  wait_enabled(app, "apply_changes")
  click(app, "apply_changes")
  wait_gone(app, "grp_name")

  expect_null(field(app, "grp_name"))
})

test_that("view lifecycle: switch, rename, remove a view (#232)", {

  skip_on_cran()

  app <- new_app_driver(
    system.file("examples", "multi-view", "app.R", package = "blockr.dock"),
    name = "view-lifecycle",
    seed = 42,
    load_timeout = 30 * 1000,
    timeout = 20 * 1000
  )
  withr::defer(app$stop())

  app$wait_for_idle()

  # Resolve the two seeded views' stable ids from their labels once. Every
  # operation travels by id (`data-view-id`); a rename moves a label but never
  # the id, so these stay valid for the whole lifecycle.
  nav <- read_view_nav(app)
  expect_identical(nrow(nav), 2L)

  first <- nav$id[nav$label == "First"]
  second <- nav$id[nav$label == "Second"]

  item_sel <- function(id) {
    sprintf("#my_board-view_nav .blockr-view-item[data-view-id=\"%s\"]", id)
  }

  # First is active on load, and off-screen views' docks are deferred (#304),
  # so only its dock is built -- carrying blockr-view-dock-active. Second's dock
  # materialises on first visit (below).
  docks <- read_view_docks(app)
  expect_identical(docks$id, first)
  expect_true(docks$active)

  # Switch active view: clicking the Second nav item reports its id to
  # `view_nav`; the reconcile builds Second's deferred dock on this first visit
  # and swaps which dock is active.
  click_view(app, second)
  app$wait_for_idle()

  nav <- read_view_nav(app)
  expect_identical(nav$label[nav$active], "Second")

  # Second's dock is now built and active; First's stays around, inactive.
  docks <- read_view_docks(app)
  expect_setequal(docks$id, c(first, second))
  expect_identical(docks$id[docks$active], second)

  # Rename the active view through the pencil: it swaps the label span for an
  # inline input that commits on Enter, sending `view_nav_rename`. The id is
  # stable, so the label moves but the dock container (keyed by id) does not.
  app$run_js(
    paste0(
      "var it = document.querySelector('", item_sel(second), "');",
      "it.querySelector('.blockr-view-edit').click();",
      "var inp = it.querySelector('.blockr-view-rename-input');",
      "inp.value = 'Renamed';",
      "$(inp).trigger($.Event('keydown', {key: 'Enter'}));"
    )
  )
  app$wait_for_idle()

  nav <- read_view_nav(app)
  expect_identical(nrow(nav), 2L)
  expect_false(anyDuplicated(nav$id) > 0L)
  expect_setequal(nav$label, c("First", "Renamed"))
  expect_identical(nav$id[nav$label == "Renamed"], second)
  expect_identical(nav$label[nav$active], "Renamed")

  docks <- read_view_docks(app)
  expect_setequal(docks$id, c(first, second))
  expect_identical(docks$id[docks$active], second)

  # Remove the (non-active) First view: the x button sends `view_nav_remove`
  # and the confirmation modal's button drives the delete. The reconcile drops
  # both its nav entry and its dock container, leaving the renamed survivor.
  app$run_js(
    paste0(
      "document.querySelector('", item_sel(first),
      " .blockr-view-remove').click()"
    )
  )
  app$wait_for_idle()

  app$click("my_board-confirm_view_remove")
  app$wait_for_idle()

  nav <- read_view_nav(app)
  expect_identical(nrow(nav), 1L)
  expect_identical(nav$label, "Renamed")
  expect_true(nav$active)

  docks <- read_view_docks(app)
  expect_identical(docks$id, second)
  expect_true(docks$active)
})

test_that("a view moves down via the nav reorder control (#351)", {

  skip_on_cran()

  app <- new_app_driver(
    system.file("examples", "multi-view", "app.R", package = "blockr.dock"),
    name = "view-reorder",
    seed = 42,
    load_timeout = 30 * 1000,
    timeout = 20 * 1000
  )
  withr::defer(app$stop())

  wait_view_nav(app, 2)

  # Two seeded views, First active and on top.
  nav <- read_view_nav(app)
  expect_identical(nav$label, c("First", "Second"))
  expect_identical(nav$label[nav$active], "First")

  first <- nav$id[nav$label == "First"]

  # Nudge First down via its chevron: the gesture sends a relative
  # `view_nav_reorder`, the server applies the order and pushes it back, and the
  # binding re-sequences the nav. Order is board content, so the active view
  # rides along rather than snapping to the new first entry.
  app$run_js(
    paste0(
      "document.querySelector('",
      sprintf(
        "#my_board-view_nav .blockr-view-item[data-view-id=\"%s\"]", first
      ),
      " .blockr-view-down').click()"
    )
  )
  app$wait_for_idle()

  nav <- read_view_nav(app)
  expect_identical(nav$label, c("Second", "First"))
  expect_false(anyDuplicated(nav$id) > 0L)
  expect_identical(nav$label[nav$active], "First")
})

test_that("dock panel move updates layout state and serialization (#234)", {

  skip_on_cran()

  app <- new_app_driver(
    system.file("examples", "edit-board", "app.R", package = "blockr.dock"),
    name = "layout-edit",
    seed = 42,
    load_timeout = 30 * 1000,
    timeout = 20 * 1000
  )
  withr::defer(app$stop())

  wait_dock_loaded(app, n_blocks = 2)
  dock <- paste0("my_board-", read_dock_state(app)$active_view, "-dock")

  # The dockview client reports its live arrangement through the `_state`
  # input, wrapped at the seam as a `dock_layout` (its verbatim grid tree).
  # `group_of` resolves which dock group holds a panel.
  read_layout <- function() {
    new_dock_layout(app$get_value(input = paste0(dock, "_state")))
  }

  group_of <- function(layout, panel) {
    leaves <- grid_leaves(layout[["grid"]])
    Position(function(leaf) panel %in% unlist(leaf[["views"]]), leaves)
  }

  # Settle on a target group count: `wait_for_js` returns as soon as the
  # client reports it, then `wait_for_idle` lets the `_state` echo reach the
  # server before it is read. Optional-chain through the widget: early in
  # startup `HTMLWidgets.find()` / `getWidget()` can still be null, and the
  # poll must wait that window out (yield `false`) rather than dereference
  # null and abort with "Cannot read properties of null (reading 'getWidget')".
  await_groups <- function(n) {
    app$wait_for_js(
      paste0(
        "HTMLWidgets.find('#", dock, "')",
        "?.getWidget()?.groups.length === ", n
      ),
      timeout = 15 * 1000
    )
    app$wait_for_idle()
  }

  # The fixture seeds blocks a and b tabbed together in a single dock group.
  # Every board offers both edges, so `api.groups` carries the two alongside it
  # whether or not they hold anything -- three in total, and four once the
  # split below makes a second grid group.
  await_groups(3L)
  before <- read_layout()
  expect_identical(
    group_of(before, "block_panel-a"),
    group_of(before, "block_panel-b")
  )

  # A real HTML5 drag is impractical to drive through chromote, so reach the
  # live dockview API and split panel b out into its own group. The rearrange
  # lives entirely in the dockview client, so no `testServer` test observes
  # it -- the seam the issue calls out.
  app$run_js(
    paste0(
      "var api = HTMLWidgets.find('#", dock, "').getWidget();",
      "var b = api.getPanel('block_panel-b');",
      "b.api.moveTo({group: b.api.group, position: 'right'});"
    )
  )
  await_groups(4L)

  layout <- read_layout()

  # Layout state: a and b, tabbed together before, now sit in separate groups.
  expect_false(
    identical(
      group_of(layout, "block_panel-a"),
      group_of(layout, "block_panel-b")
    )
  )

  # Serialization: our grid format round-trips through JSON and keeps every
  # panel while still separating a and b. The extension is not among them --
  # it sits in the rail, which the grid tree does not carry.
  grid <- as_dock_grid(layout)
  restored <- as_dock_grid(
    jsonlite::fromJSON(
      jsonlite::toJSON(as.list(grid), null = "null"),
      simplifyDataFrame = FALSE, simplifyMatrix = FALSE
    )
  )
  reparsed <- new_dock_layout(list(grid = grid_to_tree(restored)))
  expect_setequal(panel_obj_ids(layout_panel_ids(reparsed)), c("a", "b"))
  expect_identical(
    rail_panel_ids(as_dock_rails(layout)), "ext_panel-edit_board"
  )
  expect_false(
    identical(
      group_of(reparsed, "block_panel-a"),
      group_of(reparsed, "block_panel-b")
    )
  )
})

test_that("locked board hides block actions, shows lock indicator (#236)", {

  skip_on_cran()

  app <- new_app_driver(
    system.file("examples", "locked-dock", "app.R", package = "blockr.dock"),
    name = "locked-board",
    seed = 42,
    load_timeout = 30 * 1000,
    timeout = 20 * 1000
  )
  withr::defer(app$stop())

  wait_dock_loaded(app, 4)

  count <- function(selector) {
    app$get_js(sprintf("document.querySelectorAll('%s').length", selector))
  }

  # Read-only indicator rendered in the navbar (driven by is_dock_locked()).
  expect_equal(count(".blockr-lock-indicator"), 1)
  expect_match(
    app$get_js("document.querySelector('.blockr-lock-indicator').innerText"),
    "Read-only"
  )

  # Block cards and their dropdown menus render, but the locked dock suppresses
  # the mutating actions: no append / delete buttons anywhere.
  expect_gte(count(".blockr-block-dropdown"), 1)
  expect_equal(count("[id$=\"-append_block\"]"), 0)
  expect_equal(count("[id$=\"-delete_block\"]"), 0)

  # The dropdown's "Block Actions" section is gone; "Block Details" remains.
  headers <- app$get_js(
    paste0(
      "Array.from(document.querySelectorAll(",
      "'.blockr-block-dropdown .dropdown-header'",
      ")).map(function(e) { return e.innerText; }).join('|')"
    )
  )
  expect_match(headers, "Block Details")
  expect_false(grepl("Block Actions", headers))

  # View CRUD is locked too: no "New page" add control.
  expect_equal(count(".blockr-view-add"), 0)
})

test_that("single-page board renders one auto-named view (#236)", {

  skip_on_cran()

  app <- new_app_driver(
    system.file("examples", "single-page", "app.R", package = "blockr.dock"),
    name = "single-page",
    seed = 42,
    load_timeout = 30 * 1000,
    timeout = 20 * 1000
  )
  withr::defer(app$stop())

  wait_dock_loaded(app, 2)

  nav <- read_view_nav(app)

  # No named views were declared, so the board resolves to exactly one view,
  # auto-named (non-blank label) and active.
  expect_identical(nrow(nav), 1L)
  expect_true(nav$active)
  expect_true(nzchar(nav$label))

  # Exactly one view dock is rendered and active, and it is this view's.
  docks <- read_view_docks(app)
  expect_identical(nrow(docks), 1L)
  expect_true(docks$active)
  expect_identical(docks$id, nav$id)
})

test_that("navbar spinner: real work vs bookkeeping (#285, #345, #355, #360)", {

  skip_on_cran()

  app <- new_app_driver(
    system.file("examples", "multi-view", "app.R", package = "blockr.dock"),
    name = "navbar-spinner",
    seed = 42,
    load_timeout = 30 * 1000,
    timeout = 20 * 1000
  )
  withr::defer(app$stop())

  app$wait_for_idle()

  # The navbar spinner replaces shiny's page pulse: it turns while the session
  # does real block evaluation. A panel switch marks the session busy (the
  # visibility report and layout fold round-trips) without recomputing a visible
  # output, so gating on `.shiny-busy` alone would spin for what is only layout
  # bookkeeping; block evaluation marks its output `.recalculating` inside the
  # view container. The ring is always painted: the busy scope adds a darker arc
  # (a `border-top-color` distinct from the faint track the other sides carry)
  # rather than toggling its presence, and the arc is held by a
  # `transition-delay`, so drive the two busy states directly and read whether
  # the top border differs from a side border (arc present when computing,
  # absent when only bookkeeping) with transitions disabled -- otherwise the
  # delayed arc would not have landed by the time the synchronous probe reads
  # it. The recalculating element is placed outside the view container (a hidden
  # block still pending evaluation in the offcanvas pool) versus inside it (real
  # visible work) to pin the scope.
  probe <- jsonlite::fromJSON(
    app$get_js(
      r"(JSON.stringify((function () {
        var html = document.documentElement;
        var spinner = document.querySelector('.blockr-navbar-spinner');
        if (spinner) spinner.style.transition = 'none';
        var arc = function () {
          if (!spinner) return null;
          var cs = getComputedStyle(spinner);
          return { top: cs.borderTopColor, side: cs.borderRightColor };
        };
        var mark = function (parent) {
          var el = document.createElement('div');
          el.className = 'recalculating';
          parent.appendChild(el);
          return el;
        };

        var pulseOff = html.dataset.shinyBusyPulse !== 'true';
        var container = document.querySelector('.blockr-view-container');
        html.classList.add('shiny-busy');

        // The app's own outputs may still be settling -- a block card inside a
        // view container can hold a lingering `.recalculating` well past
        // wait_for_idle(). Neutralise every real in-container marker (the exact
        // spinner-CSS scope) so only the synthetic markers below drive the
        // reading, then restore them.
        var real = Array.from(
          document.querySelectorAll('.blockr-view-container .recalculating')
        );
        real.forEach(function (el) { el.classList.remove('recalculating'); });

        var hidden = mark(document.body);
        var bookkeeping = arc();
        hidden.remove();

        var visible = mark(container);
        var computing = arc();
        visible.remove();

        real.forEach(function (el) { el.classList.add('recalculating'); });
        html.classList.remove('shiny-busy');
        if (spinner) spinner.style.transition = '';

        var painted = bookkeeping && bookkeeping.side !== 'rgba(0, 0, 0, 0)';
        var bkArc = bookkeeping && bookkeeping.top !== bookkeeping.side;
        var coArc = computing && computing.top !== computing.side;

        return {
          pulseOff: pulseOff, hasSpinner: spinner !== null,
          hasContainer: container !== null,
          trackPainted: painted, bookkeepingArc: bkArc, computingArc: coArc
        };
      })()))"
    )
  )

  # The page pulse is off, and the navbar spinner and the view container the
  # scope keys on are both present.
  expect_true(probe$pulseOff)
  expect_true(probe$hasSpinner)
  expect_true(probe$hasContainer)

  # Busy with a recalculating output only outside the view container (a bare
  # panel switch, or a hidden block still pending in the offcanvas) leaves the
  # ring as a bare track -- no arc; busy with a recalculating output inside it
  # (block evaluation) paints the darker arc on. The track itself stays painted
  # rather than toggling off -- the always-on behaviour.
  expect_true(probe$trackPainted)
  expect_false(probe$bookkeepingArc)
  expect_true(probe$computingArc)
})

test_that("a narrow viewport stacks a view into a scrolling column (#413)", {

  skip_on_cran()

  # An authored two-group board, one group tabbed: side by side when wide, and
  # the tabbed group means the stack carries a parked background-tab overlay.
  app <- new_app_driver(
    system.file("examples", "narrow-stack", "app.R", package = "blockr.dock"),
    name = "narrow-viewport",
    seed = 42,
    width = 500,
    height = 900,
    load_timeout = 30 * 1000,
    timeout = 30 * 1000
  )
  withr::defer(app$stop())

  wait_block_panel_tabs(
    app, c("block_panel-a", "block_panel-b", "block_panel-c")
  )
  wait_dock_groups(app, 2L)

  rects <- dock_group_rects(app)

  # Stacked, not side by side: the rows share a left edge and a width, and the
  # second starts below the first. Authored wide, these differ in `left`.
  expect_identical(nrow(rects), 2L)
  expect_identical(length(unique(rects$left)), 1L)
  expect_identical(length(unique(rects$width)), 1L)
  expect_gt(rects$top[[2L]], rects$top[[1L]])

  # The page carries the stack rather than dockView squeezing it into the
  # viewport, which is what `narrow_group_fraction()` buys.
  scroll <- page_scroll_extent(app)
  expect_gt(scroll$docH, scroll$viewH)

  # And it ends with the stack. Every background tab's render overlay is
  # parked below the grid by dockView, each a full container tall, so a
  # container that does not clip trails a blank screenful past the last panel.
  expect_lt(scroll$docH - max(rects$top + rects$height), 60)

  app$wait_for_idle()

  # The stack is a render, not a commit. The mirror is left unwired, so the
  # echo writes nothing back, and `view_data` still reports the authored 40/60
  # grid -- what a save persists and a wide viewport restores to.
  expect_equal(app$get_value(export = "commit_count"), 0)
  expect_true(isTRUE(app$get_value(export = "roundtrip_stable")))
})

test_that("the extension rides a left rail, shown only while it holds it", {

  skip_on_cran()

  app <- new_app_driver(
    system.file("examples", "edit-board", "app.R", package = "blockr.dock"),
    name = "rail-derived",
    seed = 42,
    load_timeout = 30 * 1000,
    timeout = 20 * 1000
  )
  withr::defer(app$stop())

  wait_dock_loaded(app, n_blocks = 2)
  dock <- paste0("my_board-", read_dock_state(app)$active_view, "-dock")
  api <- paste0("HTMLWidgets.find('#", dock, "').getWidget()")

  # Optional-chain in the polls: the widget can still be null early in startup,
  # and the condition must yield `false` rather than abort on a null deref.
  rail_visible <- function(state) {
    paste0(
      "HTMLWidgets.find('#", dock, "')?.getWidget()",
      "?.isEdgeGroupVisible('left') === ", state
    )
  }

  app$wait_for_js(rail_visible("true"), timeout = 15 * 1000)
  app$wait_for_idle()

  # A rail is a real dockview edge group carrying its tabs vertically, keyed by
  # the group id the payload names it with.
  # Both declared rails are in the DOM; the empty right one renders at zero
  # width. Address the left one by the group id its payload names it with.
  rail_html <- xml2::read_html(
    app$get_html('[data-testid="dv-edge-group-rail-left"]')
  )

  has_class <- function(token) {
    paste0(
      "//*[contains(concat(' ', normalize-space(@class), ' '), ' ",
      token, " ')]"
    )
  }

  expect_length(
    xml2::xml_find_all(rail_html, has_class("dv-groupview-edge")), 1L
  )
  expect_length(
    xml2::xml_find_all(rail_html, has_class("dv-tabs-container-vertical")), 1L
  )
  expect_identical(
    xml2::xml_attr(
      xml2::xml_find_all(rail_html, has_class("dv-tab")), "data-tab-panel-id"
    ),
    "ext_panel-edit_board"
  )

  read_state <- function() {
    new_dock_layout(app$get_value(input = paste0(dock, "_state")))
  }

  read_rails <- function() as_dock_rails(read_state())

  expect_identical(rail_panel_ids(read_rails()), "ext_panel-edit_board")

  # The right rail is declared but empty, so it is hidden -- which is what
  # gives a drag toward that edge something to reveal.
  expect_false(
    isTRUE(app$get_js(paste0(api, ".isEdgeGroupVisible('right')")))
  )
  expect_true(
    isTRUE(app$get_js(paste0("!!", api, ".getEdgeGroup('right')")))
  )

  # Emptying the rail hides it: visibility is derived from what it holds, not
  # stored, so nothing has to remember to turn it off. This is the client half
  # of the rule; the server half (the same rule in the restore payload) is
  # covered in test-rail-class.R. The two assertions above and below pin both
  # ends: born hidden when empty, and hidden again once emptied.
  app$run_js(
    paste0(
      "var api = ", api, ";",
      "var grid = api.groups.filter(",
      "function (g) { return g.api.location.type === 'grid' })[0];",
      "api.getPanel('ext_panel-edit_board').api.moveTo(",
      "{group: grid, position: 'within'});"
    )
  )

  app$wait_for_js(rail_visible("false"), timeout = 15 * 1000)
  app$wait_for_idle()

  expect_identical(rail_panel_ids(read_rails()), character())
  expect_true(
    "ext_panel-edit_board" %in% layout_panel_ids(as_dock_grid(read_state()))
  )
})

test_that("a viewport too narrow for a rail keeps its stored width (#457)", {

  skip_on_cran()

  # Narrow enough that the rail cannot have the 260px the fixture stores for it.
  # A rail is the low-priority view of dockView's shell splitview, so the layout
  # squeezes it rather than the centre, and hands the space back to the centre
  # rather than the rail once there is room again -- the width it reports from
  # here on is the viewport's, and committing it would leave the board holding
  # the narrowest one it was ever opened at.
  app <- new_app_driver(
    system.file("examples", "edit-board", "app.R", package = "blockr.dock"),
    name = "rail-narrow-viewport",
    seed = 42,
    load_timeout = 30 * 1000,
    timeout = 20 * 1000,
    width = 520,
    height = 900
  )
  withr::defer(app$stop())

  wait_dock_loaded(app, n_blocks = 2)

  rail_width <- function() {
    app$get_js(
      paste0(
        "document.querySelector(",
        "'[data-testid=\"dv-edge-group-rail-left\"]')",
        "?.getBoundingClientRect().width"
      )
    )
  }

  stored_rail_width <- function() {
    ser <- jsonlite::fromJSON(
      retry_download(app, "my_board-preserve_board-serialize"),
      simplifyDataFrame = FALSE, simplifyMatrix = FALSE
    )
    grids <- ser[["payload"]][["grids"]][["payload"]]
    grids[[1L]][["rails"]][["left"]][["size"]]
  }

  app$wait_for_js(
    paste0(
      "document.querySelector(",
      "'[data-testid=\"dv-edge-group-rail-left\"]')",
      "?.getBoundingClientRect().width > 0"
    ),
    timeout = 15 * 1000
  )
  app$wait_for_idle()

  # The DOM read is what keeps this from passing vacuously: it has to observe
  # the squeeze for the stored width to be asserting anything.
  expect_lt(rail_width(), 200)
  expect_equal(stored_rail_width(), 260)

  # And a viewport with room again leaves the stored width where it was, so a
  # later restore has the authored geometry to come back to.
  app$set_window_size(width = 1600, height = 900)
  app$wait_for_idle()

  expect_equal(stored_rail_width(), 260)
})

test_that("a drag toward the edge reveals a hidden rail, collapsed", {

  skip_on_cran()

  app <- new_app_driver(
    system.file("examples", "edit-board", "app.R", package = "blockr.dock"),
    name = "rail-reveal",
    seed = 42,
    load_timeout = 30 * 1000,
    timeout = 20 * 1000
  )
  withr::defer(app$stop())

  wait_dock_loaded(app, n_blocks = 2)
  dock <- paste0("my_board-", read_dock_state(app)$active_view, "-dock")
  api <- paste0("HTMLWidgets.find('#", dock, "').getWidget()")

  rail_visible <- function(state) {
    paste0(
      "HTMLWidgets.find('#", dock, "')?.getWidget()",
      "?.isEdgeGroupVisible('left') === ", state
    )
  }

  app$wait_for_js(rail_visible("true"), timeout = 15 * 1000)

  # Empty the rail so it is hidden, which is the state a reveal has to rescue:
  # with no hit area, a drag toward that edge would otherwise have nothing to
  # aim at.
  app$run_js(
    paste0(
      "var api = ", api, ";",
      "var grid = api.groups.filter(",
      "function (g) { return g.api.location.type === 'grid' })[0];",
      "api.getPanel('ext_panel-edit_board').api.moveTo(",
      "{group: grid, position: 'within'});"
    )
  )

  app$wait_for_js(rail_visible("false"), timeout = 15 * 1000)

  # Start a real dockview drag and hold the pointer in the band the collapsed
  # strip would occupy. dockview's own `dragstart` handler arms the drag, so
  # this exercises the same path a user's gesture takes.
  app$run_js(
    paste0(
      "var el = document.querySelector('#", dock, "');",
      "var tab = document.querySelector(",
      "'[data-tab-panel-id=\"block_panel-b\"]');",
      "var dt = new DataTransfer();",
      "tab.dispatchEvent(new DragEvent('dragstart',",
      "{bubbles: true, cancelable: true, dataTransfer: dt}));",
      "var r = el.getBoundingClientRect();",
      "el.dispatchEvent(new DragEvent('dragover',",
      "{bubbles: true, cancelable: true, dataTransfer: dt,",
      " clientX: r.left + 5, clientY: r.top + r.height / 2}));"
    )
  )

  app$wait_for_js(rail_visible("true"), timeout = 15 * 1000)

  # Revealed collapsed: an empty rail shows its strip, not a full-width pane.
  expect_true(
    isTRUE(app$get_js(paste0(api, ".getEdgeGroup('left').isCollapsed()")))
  )

  # A drag that ends without dropping into the rail leaves the derived rule to
  # hide it again.
  app$run_js(
    paste0(
      "document.querySelector('[data-tab-panel-id=\"block_panel-b\"]')",
      ".dispatchEvent(new DragEvent('dragend', {bubbles: true}));"
    )
  )

  app$wait_for_js(rail_visible("false"), timeout = 15 * 1000)

  expect_false(isTRUE(app$get_js(paste0(api, ".isEdgeGroupVisible('left')"))))
})

test_that("a rail's seam squares against its content on every edge", {

  skip_on_cran()

  app <- new_app_driver(
    system.file("examples", "rails", "app.R", package = "blockr.dock"),
    name = "rail-seams",
    seed = 42,
    load_timeout = 30 * 1000,
    timeout = 20 * 1000
  )
  withr::defer(app$stop())

  wait_dock_loaded(app, n_blocks = 3)
  app$wait_for_js(
    'document.querySelectorAll(".dv-groupview-edge").length === 2',
    timeout = 15 * 1000
  )

  # Dockview's spaced themes round a strip's *top* corners and its content's
  # *bottom* corners whichever edge the header is on, which notches the seam
  # once it moves. Rotating them is CSS, so the assertion is the computed
  # radius: square where strip meets content, rounded on the outer edge.
  radii <- function(position, part) {
    unlist(
      app$get_js(
        paste0(
          "(function(){var t=document.querySelector('",
          "[data-testid=\"dv-edge-group-rail-", position, "\"] ", part,
          "');if(!t)return 'none';var c=getComputedStyle(t);return [",
          "c.borderTopLeftRadius,c.borderTopRightRadius,",
          "c.borderBottomRightRadius,c.borderBottomLeftRadius].join('/')})()"
        )
      )
    )
  }

  strip <- ".dv-tabs-and-actions-container"
  content <- ".dv-content-container"

  # A left strip meets its content on its right, a right strip on its left.
  expect_identical(radii("left", strip), "12px/0px/0px/12px")
  expect_identical(radii("left", content), "0px/12px/12px/0px")
  expect_identical(radii("right", strip), "0px/12px/12px/0px")
  expect_identical(radii("right", content), "12px/0px/0px/12px")
})

test_that("a drop elsewhere hides the rail it emptied (#431)", {

  skip_on_cran()

  app <- new_app_driver(
    system.file("examples", "rails", "app.R", package = "blockr.dock"),
    name = "rail-empty-on-drop",
    seed = 42,
    load_timeout = 30 * 1000,
    timeout = 20 * 1000
  )
  withr::defer(app$stop())

  wait_dock_loaded(app, n_blocks = 3)
  dock <- paste0("my_board-", read_dock_state(app)$active_view, "-dock")
  api <- paste0("HTMLWidgets.find('#", dock, "').getWidget()")

  visible <- function(position, state) {
    paste0(
      "HTMLWidgets.find('#", dock, "')?.getWidget()",
      "?.isEdgeGroupVisible('", position, "') === ", state
    )
  }

  app$wait_for_js(visible("right", "true"), timeout = 15 * 1000)

  # Drag the right rail's only panel out, sweeping its reveal band on the way,
  # and drop it in the grid.
  app$run_js(
    paste0(
      "var el = document.querySelector('#", dock, "');",
      "var tab = document.querySelector(",
      "'[data-tab-panel-id=\"block_panel-b\"]');",
      "var dt = new DataTransfer();",
      "tab.dispatchEvent(new DragEvent('dragstart',",
      "{bubbles: true, cancelable: true, dataTransfer: dt}));",
      "var r = el.getBoundingClientRect();",
      "el.dispatchEvent(new DragEvent('dragover',",
      "{bubbles: true, cancelable: true, dataTransfer: dt,",
      " clientX: r.right - 5, clientY: r.top + r.height / 2}));"
    )
  )

  # Dockview detaches the dragged tab as the panel lands, and a detached node's
  # `dragend` propagates nowhere -- so the panel event, not the DOM event, is
  # what has to resolve the drag. Reproduce that exactly: detach, then move.
  app$run_js(
    paste0(
      "var api = ", api, ";",
      "var grid = api.groups.filter(",
      "function (g) { return g.api.location.type === 'grid' })[0];",
      "document.querySelector('[data-tab-panel-id=\"block_panel-b\"]')",
      ".remove();",
      "api.getPanel('block_panel-b').api.moveTo(",
      "{group: grid, position: 'within'});"
    )
  )

  app$wait_for_js(visible("right", "false"), timeout = 15 * 1000)

  expect_false(isTRUE(app$get_js(paste0(api, ".isEdgeGroupVisible('right')"))))
  expect_true(
    "block_panel-b" %in% grid_tree_ids(
      as_dock_grid(
        new_dock_layout(app$get_value(input = paste0(dock, "_state")))
      )
    )
  )
})

test_that("a panel landing in a revealed rail expands it (#431)", {

  skip_on_cran()

  app <- new_app_driver(
    system.file("examples", "edit-board", "app.R", package = "blockr.dock"),
    name = "rail-expand-on-drop",
    seed = 42,
    load_timeout = 30 * 1000,
    timeout = 20 * 1000
  )
  withr::defer(app$stop())

  wait_dock_loaded(app, n_blocks = 2)
  dock <- paste0("my_board-", read_dock_state(app)$active_view, "-dock")
  api <- paste0("HTMLWidgets.find('#", dock, "').getWidget()")

  state <- function(prop, want) {
    paste0(
      "HTMLWidgets.find('#", dock, "')?.getWidget()", prop, " === ", want
    )
  }

  app$wait_for_js(state("?.isEdgeGroupVisible('left')", "true"),
                  timeout = 15 * 1000)

  # Empty the rail so it hides, which is the state a reveal has to rescue.
  app$run_js(
    paste0(
      "var api = ", api, ";",
      "var grid = api.groups.filter(",
      "function (g) { return g.api.location.type === 'grid' })[0];",
      "api.getPanel('ext_panel-edit_board').api.moveTo(",
      "{group: grid, position: 'within'});"
    )
  )
  app$wait_for_js(state("?.isEdgeGroupVisible('left')", "false"),
                  timeout = 15 * 1000)

  # Reveal it by dwelling in the band the collapsed strip would occupy. It
  # comes up collapsed, so an empty rail shows its strip rather than a
  # full-width empty pane.
  app$run_js(
    paste0(
      "var el = document.querySelector('#", dock, "');",
      "var tab = document.querySelector(",
      "'[data-tab-panel-id=\"block_panel-a\"]');",
      "var dt = new DataTransfer();",
      "tab.dispatchEvent(new DragEvent('dragstart',",
      "{bubbles: true, cancelable: true, dataTransfer: dt}));",
      "var r = el.getBoundingClientRect();",
      "el.dispatchEvent(new DragEvent('dragover',",
      "{bubbles: true, cancelable: true, dataTransfer: dt,",
      " clientX: r.left + 5, clientY: r.top + r.height / 2}));"
    )
  )
  # Gate on visibility, not on `isCollapsed()`: a hidden empty rail already
  # reports itself collapsed, so waiting on that would pass before the reveal
  # had happened at all.
  app$wait_for_js(state("?.isEdgeGroupVisible('left')", "true"),
                  timeout = 15 * 1000)
  expect_true(
    isTRUE(app$get_js(paste0(api, ".getEdgeGroup('left').isCollapsed()")))
  )

  # Land a panel in it. `addPanel()` stands in for the drop because dockview's
  # `moveTo()` refuses a collapsed edge group outright ("Invalid grid element")
  # -- but what reaches our code is the same, a panel arriving in a rail that a
  # drag revealed, and the same `onDidAddPanel` that a real drop fires.
  app$run_js(
    paste0(
      api, ".addPanel({id: 'rail-drop-probe', component: 'default',",
      " title: 'Probe', params: {content: {html: 'probe'}},",
      " position: {referenceGroup: 'rail-left'}});"
    )
  )

  # The rail expands: content has arrived, so the bare strip is no longer what
  # the user wants to see.
  app$wait_for_js(state("?.getEdgeGroup('left')?.isCollapsed()", "false"),
                  timeout = 15 * 1000)

  expect_true(isTRUE(app$get_js(paste0(api, ".isEdgeGroupVisible('left')"))))
  expect_equal(
    app$get_js(
      paste0(
        "(", api, ".groups.filter(function (g) {",
        "var l = g.api.location;",
        "return l.type === 'edge' && l.position === 'left' })[0]",
        " || {panels: []}).panels.length"
      )
    ),
    1
  )
})

test_that("a rail collapse round-trips with no following gesture (#436)", {

  skip_on_cran()

  app <- new_app_driver(
    system.file("examples", "rails", "app.R", package = "blockr.dock"),
    name = "rail-collapse",
    seed = 42,
    load_timeout = 30 * 1000,
    timeout = 20 * 1000
  )
  withr::defer(app$stop())

  wait_dock_loaded(app, n_blocks = 3)
  view <- read_dock_state(app)$active_view
  dock <- paste0("my_board-", view, "-dock")
  api <- paste0("HTMLWidgets.find('#", dock, "').getWidget()")

  # Optional-chain the polls from `window` down: early in startup neither the
  # widget nor htmlwidgets itself is on the page yet, and the poll has to wait
  # that window out (yield `false`) rather than abort on a null dereference or
  # an undefined global.
  rail_is <- function(collapsed) {
    paste0(
      "window.HTMLWidgets?.find('#", dock, "')?.getWidget()",
      "?.getEdgeGroup('left')?.isCollapsed() === ", collapsed
    )
  }

  # A click on a rail's active tab toggles it collapsed and changes nothing
  # else -- no move, no add, no activation change. Every other rail gesture
  # rides an event the dockView client already reports, which is what hid this
  # one (cynkra/dockViewR#109): the rail rendered collapsed while the board
  # went on recording the state it had before, until an unrelated click
  # happened to flush. So the assertions below deliberately follow the toggle
  # with nothing at all.
  toggle_rail <- function(collapsed) {
    app$run_js(
      paste0(
        api, ".groups.find(function (g) {",
        "var l = g.api.location;",
        "return l.type === 'edge' && l.position === 'left' })",
        ".element.querySelector('.dv-tab').click();"
      )
    )
    app$wait_for_js(rail_is(collapsed), timeout = 15 * 1000)
    app$wait_for_idle()
  }

  # The board as a save finds it: exported through the live plugin, so the read
  # goes through the mirror that commits the client echo rather than through
  # the echo itself. A stale value is harmless in a live session -- the next
  # click corrects it -- and wrong exactly here, where the toggle is the last
  # thing the user did before the board was written out.
  stored_collapsed <- function() {
    board <- blockr_deser(
      jsonlite::fromJSON(
        retry_download(app, "my_board-preserve_board-serialize"),
        simplifyDataFrame = FALSE, simplifyMatrix = FALSE
      )
    )
    isTRUE(board_grids(board)[[view]][["rails"]][["left"]][["collapsed"]])
  }

  app$wait_for_js(rail_is("false"), timeout = 15 * 1000)
  expect_false(stored_collapsed())

  toggle_rail("true")
  expect_true(stored_collapsed())

  toggle_rail("false")
  expect_false(stored_collapsed())
})
