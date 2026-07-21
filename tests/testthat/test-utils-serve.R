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

  expect_identical(resolve_url_view(views, "?view=Second"), "Second")
  expect_identical(resolve_url_view(views, "?view=First"), "First")

  # Absent, empty, unknown, or no query string all decline to select.
  expect_null(resolve_url_view(views, "?other=1"))
  expect_null(resolve_url_view(views, "?view="))
  expect_null(resolve_url_view(views, "?view=nope"))
  expect_null(resolve_url_view(views, ""))
  expect_null(resolve_url_view(views, NULL))
})

test_that("select_url_view opens the board on the ?view= view (#323)", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_dataset_block()),
    views = list(First = blk("a"), Second = blk("b")),
    active = "First"
  )

  fake_session <- function(search) {
    list(clientData = list(url_search = search))
  }

  # The default active view is "First", so flipping to "Second" is a real
  # (non-vacuous) change driven purely by the query param.
  expect_identical(active_view(brd), "First")
  expect_identical(
    active_view(select_url_view(brd, fake_session("?view=Second"))),
    "Second"
  )

  # An unknown id, an absent param, or no session leaves the default active.
  expect_identical(
    active_view(select_url_view(brd, fake_session("?view=nope"))),
    "First"
  )
  expect_identical(
    active_view(select_url_view(brd, fake_session(""))),
    "First"
  )
  expect_identical(active_view(select_url_view(brd, NULL)), "First")
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
    timeout = 20 * 1000
  )
  withr::defer(app$stop())

  app$wait_for_idle()

  add_block <- function(registry, id) {
    app$set_inputs(
      `my_board-ext_edit_board-registry_select` = registry,
      `my_board-ext_edit_board-block_id` = id
    )
    app$click("my_board-ext_edit_board-confirm_add")
    app$wait_for_idle()
  }

  add_block("dataset_block", "a")
  expect_identical(block_panel_tabs(app), "block_panel-a")

  # Pre-fix, the second add fired reconcile_views against a board that
  # lagged the live dock, restoring it and wiping both block panels -- leaving
  # only the extension (#196). Both block tabs must survive.
  add_block("head_block", "b")
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
  # a a hidden back tab) before reading the exported grid, or the export can
  # capture a transient separate-leaves state -- the grid mirror commits
  # whatever the client last reported.
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
  # blocks: re-exporting after the reload reproduces the stored geometry (the
  # tab group, its active tab, the custom sizes) byte-for-byte. This reads the
  # committed board's slots -- proving the stage / reload cycle preserved them
  # and that the fixture re-importing its own (colliding) view ids does not drop
  # them. It cannot see the client render, so that leg is asserted below.
  #
  # Byte-exact holds because the mirror skips the restore's "restore"-tagged
  # replay, so the stored grid is never overwritten by the sizes dockview
  # renders (which jitter sub-tolerance run to run); a single post-restore focus
  # settles deterministically rather than churning them.
  #
  # get_download can transiently fail after the reload -- the link's href is
  # filled only once outputs bind, and the download endpoint may briefly not
  # answer ("Unable request data from server"); retry_download absorbs both.
  path2 <- retry_download(app, "my_board-preserve_board-serialize")
  ser2 <- jsonlite::fromJSON(path2, simplifyDataFrame = FALSE,
                             simplifyMatrix = FALSE)

  expect_identical(
    drop_focus(ser2[["payload"]][["grids"]]),
    drop_focus(ser[["payload"]][["grids"]])
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

  app <- new_app_driver(
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

  app <- new_app_driver(
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
  app$run_js(
    paste0("document.querySelector('", item_sel(second), "').click()")
  )
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

  # The fixture seeds blocks a and b tabbed together in a single dock group
  # (the extension panel sits in its own group).
  await_groups(2L)
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
  await_groups(3L)

  layout <- read_layout()

  # Layout state: a and b, tabbed together before, now sit in separate groups.
  expect_false(
    identical(
      group_of(layout, "block_panel-a"),
      group_of(layout, "block_panel-b")
    )
  )

  # Serialization: our grid format round-trips through JSON and keeps every
  # panel while still separating a and b.
  grid <- as_dock_grid(layout)
  restored <- as_dock_grid(
    jsonlite::fromJSON(
      jsonlite::toJSON(as.list(grid), null = "null"),
      simplifyDataFrame = FALSE, simplifyMatrix = FALSE
    )
  )
  reparsed <- new_dock_layout(list(grid = grid_to_tree(restored)))
  expect_setequal(
    panel_obj_ids(layout_panel_ids(reparsed)),
    c("a", "b", "edit_board")
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

test_that("navbar spinner tracks real work, not bookkeeping (#285, #345)", {

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

  # The navbar spinner (a ring next to the gear) replaces shiny's page pulse: it
  # turns while the session does real block evaluation. A panel switch marks the
  # session busy (the visibility report and layout fold round-trips) without
  # recomputing a visible output, so gating on `.shiny-busy` alone would spin
  # for what is only layout bookkeeping; block evaluation marks its output
  # `.recalculating` inside the view container. The live transition is a
  # sub-second transient, racy to sample, so drive the two busy states directly
  # and read the spinner's computed display (`none` when idle-scoped; a shown
  # value otherwise -- `block`, since the flex navbar blockifies the shown flex
  # item). The recalculating element is placed outside the view container (a
  # hidden block still pending evaluation in the offcanvas pool) versus inside
  # it (real visible work) to pin the scope.
  probe <- jsonlite::fromJSON(
    app$get_js(
      r"(JSON.stringify((function () {
        var html = document.documentElement;
        var spinner = document.querySelector('.blockr-navbar-spinner');
        var display = function () {
          return spinner ? getComputedStyle(spinner).display : null;
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
        var bookkeeping = display();
        hidden.remove();

        var visible = mark(container);
        var computing = display();
        visible.remove();

        real.forEach(function (el) { el.classList.add('recalculating'); });
        html.classList.remove('shiny-busy');

        return {
          pulseOff: pulseOff, hasSpinner: spinner !== null,
          hasContainer: container !== null,
          bookkeeping: bookkeeping, computing: computing
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
  # panel switch, or a hidden block still pending in the offcanvas) keeps the
  # spinner hidden; busy with a recalculating output inside it (block
  # evaluation) shows it.
  expect_identical(probe$bookkeeping, "none")
  expect_identical(probe$computing, "block")
})
