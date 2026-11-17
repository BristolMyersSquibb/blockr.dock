# End-to-end tests of the example apps, through shinytest2.
# Core's client and the dock's run on the page in Chrome, and the app runs in
# another R process.
# Each test acts on the page as a user does, and reads what the page gives:
# the board, with the dock's state, each block's status, and the block that
# has focus.
# They check what the dock's tests on core's CoreApi for tests cannot check,
# since happy-dom lays nothing out and runs no R:
# dockview's gestures and what the dock writes back from them, the narrow
# layout, the `?view=` link, the focus that a click gives a block, which blocks
# the dock demands, and a locked board.

test_that("serve() with the dock plugin serves a page that the dock draws", {

  # The example runs the dock plugin alone, as one row of plugins.
  # R accepts that row only where blockr.dock lists the plugin's class in its
  # `._BLOCKR_MODULES_`.
  app <- local_example_app("empty")

  # The dock draws only once its module has imported dockview from the vendor
  # file beside it, which R serves with the module.
  expect_no_error(wait_for_dock(app))

  # The dock's stylesheet imports dockview's from a vendor file as well.
  # Only dockview's rules place its element `.dv-dockview` relative.
  expect_identical(
    app$get_js(
      "getComputedStyle(document.querySelector('.dv-dockview')).position"
    ),
    "relative"
  )
})

test_that("a sash drag writes the shares that dockview shows, once it ends", {

  app <- local_example_app("sized-grid")
  wait_for_dock(app)

  expect_equal(grid_sizes(app_grid(app, "Main")), c(0.3, 0.7))

  # The user drags the sash between the view's two tab groups to the middle
  # between the groups' outer edges.
  sash <- center_of(
    app,
    "document.querySelector('.blockr-dock .dv-sash.dv-enabled')"
  )
  left <- box_of(app, group_js(1L))
  right <- box_of(app, group_js(2L))
  middle <- c(
    x = (left[["x"]] + right[["x"]] + right[["width"]]) / 2,
    y = sash[["y"]]
  )

  # The dock writes what a gesture changed once the user lets go, so the
  # stored shares stay as they were while the button is down.
  drag_mouse(
    app,
    sash,
    middle,
    held = function() {
      Sys.sleep(0.5)
      expect_equal(grid_sizes(app_grid(app, "Main")), c(0.3, 0.7))
    }
  )

  wait_for_grid(
    app,
    "Main",
    function(grid) all(abs(grid_sizes(grid) - 0.5) < 0.02),
    "shares of 0.5 each"
  )
})

test_that("a tab dropped on the right of its tab group splits the group", {

  app <- local_example_app("serdes")
  wait_for_dock(app)

  # The view Analysis shows the panel of core's Edit board plugin in one tab
  # group, and the panels of blocks `a` and `b` in another.
  expect_identical(
    lapply(tab_groups(app_grid(app, "analysis")), group_panels),
    list("blockr.core-edit_board_plugin", c("a", "b"))
  )

  # The user drags the tab of block `a` onto the right of its own tab group,
  # 100 pixels in from the group's right edge.
  group <- box_of(app, group_js(2L))
  drag_tab(
    app,
    center_of(app, tab_js(2L, 1L)),
    c(
      x = group[["x"]] + group[["width"]] - 100,
      y = group[["y"]] + group[["height"]] / 2
    )
  )

  wait_for_grid(
    app,
    "analysis",
    function(grid) length(tab_groups(grid)) == 3L,
    "three tab groups"
  )

  expect_identical(
    lapply(tab_groups(app_grid(app, "analysis")), group_panels),
    list("blockr.core-edit_board_plugin", "b", "a")
  )
})

test_that("a click on a tab shows the panel, and its close button removes it", {

  app <- local_example_app("narrow-stack")
  wait_for_dock(app)

  # The second tab group shows block `b` in front of block `c`.
  # The dock demands only the blocks whose cards are on screen, so R
  # evaluates `a` and `b`, but not `c`.
  wait_for_evaluated(app, c("a", "b"))
  expect_identical(app_evaluated(app, "c"), c(c = FALSE))

  click_at(app, center_of(app, tab_js(2L, 2L)))

  wait_for_grid(
    app,
    "Main",
    function(grid) identical(group_active(tab_groups(grid)[[2L]]), "c"),
    "block c in front"
  )

  wait_for_evaluated(app, "c")

  # Closing the tab removes the panel from the view, but not the block from
  # the board.
  click_at(
    app,
    center_of(
      app,
      paste0(tab_js(2L, 2L), ".querySelector('.dv-default-tab-action')")
    )
  )

  wait_for_grid(
    app,
    "Main",
    function(grid) {
      identical(lapply(tab_groups(grid), group_panels), list("a", "b"))
    },
    "no panel of block c"
  )

  expect_true("c" %in% names(app_board(app)[["blocks"]]))
})

test_that("a click into a card or onto a tab gives its block focus", {

  app <- local_example_app("narrow-stack")
  wait_for_dock(app)

  # No block has focus as the board opens.
  expect_null(app_focus(app))

  # The user clicks the name of block `a` on its card, which only a double
  # click renames.
  click_at(
    app,
    center_of(
      app,
      paste0(
        "document.querySelector(",
        "'.blockr-dock-panel[data-block=\"a\"] .blockr-card-name'",
        ")"
      )
    )
  )

  wait_for_focus(app, "a")

  # The user clicks the tab of block `c`, which the second tab group shows
  # behind block `b`.
  click_at(app, center_of(app, tab_js(2L, 2L)))

  wait_for_focus(app, "c")
})

test_that("a click on a rail's tab collapses the rails of every view", {

  app <- local_example_app("rail-sync")
  wait_for_dock(app)

  collapsed <- function() {
    views <- app_dock(app)[["views"]]
    set_names(
      lgl_ply(
        views,
        function(view) view[["grid"]][["rails"]][["left"]][["collapsed"]]
      ),
      names(views)
    )
  }

  expect_identical(collapsed(), c(First = FALSE, Second = FALSE, Third = FALSE))

  # The board option `sync_rails` is on by default, so collapsing the left
  # rail of one view collapses the left rail of every view.
  click_at(app, center_of(app, rail_tab_js("left")))

  wait_until(
    function() all(collapsed()),
    function() "the left rail of every view to collapse"
  )

  expect_true(
    app$get_js(
      paste0(
        "document.querySelector('.blockr-dock .dv-groupview-edge",
        ".dv-groupview-header-left').classList.contains('dv-edge-collapsed')"
      )
    )
  )
})

test_that("a narrow window shows the view in one column, and keeps the grid", {

  app <- local_example_app("narrow-stack")
  wait_for_dock(app)

  stored <- app_grid(app, "Main")

  # The example sets the blockr option `narrow_breakpoint` to 900 pixels.
  # In a window narrower than that, each tab group is a row of one column,
  # below the row before it.
  app$set_window_size(width = 800, height = 900)
  app$wait_for_js(
    "document.querySelector('.blockr-dock[data-layout=\"narrow\"]') !== null"
  )

  rows <- lapply(1:2, function(i) box_of(app, group_js(i)))

  expect_equal(rows[[1L]][["x"]], rows[[2L]][["x"]])
  expect_equal(rows[[1L]][["width"]], rows[[2L]][["width"]])
  expect_gt(rows[[2L]][["y"]], rows[[1L]][["y"]])

  # Drawing the column writes nothing back, so a wider window shows the grid
  # with its shares of 0.4 and 0.6 again.
  expect_identical(app_grid(app, "Main"), stored)

  app$set_window_size(width = 1400, height = 900)
  app$wait_for_js(
    "document.querySelector('.blockr-dock[data-layout=\"narrow\"]') === null"
  )

  wait_until(
    function() {
      widths <- lapply(1:2, function(i) box_of(app, group_js(i))[["width"]])
      abs(widths[[1L]] / (widths[[1L]] + widths[[2L]]) - 0.4) < 0.02
    },
    function() "two tab groups side by side, at shares of 0.4 and 0.6"
  )

  expect_identical(app_grid(app, "Main"), stored)
})

test_that("a link with ?view= opens the board on the view that it names", {

  app <- local_example_app("view-tabs")
  wait_for_dock(app)

  expect_identical(app_dock(app)[["active"]], "First")

  open_query(app, "?view=Second")

  expect_identical(app_dock(app)[["active"]], "Second")

  # An id that names no view leaves the board on the view that it opens on.
  open_query(app, "?view=Fourth")

  expect_identical(app_dock(app)[["active"]], "First")
})

test_that("the dock demands the blocks of the view that it shows", {

  app <- local_example_app("view-tabs")
  wait_for_dock(app)

  # Each of the three views shows one block, and the board opens on the view
  # First, so R evaluates block `a` alone.
  wait_for_evaluated(app, "a")
  expect_identical(app_evaluated(app, c("b", "c")), c(b = FALSE, c = FALSE))

  # A click on the tab of the view Second, in the line of tabs under the bar,
  # shows that view, so R evaluates block `b` as well.
  click_at(
    app,
    center_of(
      app,
      paste0(
        "[...document.querySelectorAll('.blockr-dock-tabs .blockr-dock-tab')]",
        ".find((tab) => tab.textContent.trim() === 'Second')"
      )
    )
  )

  wait_for_evaluated(app, "b")
  expect_identical(app_evaluated(app, "c"), c(c = FALSE))
})

test_that("a locked board offers no tab to close or drag, nor a panel to add", {

  app <- local_example_app("locked-dock")
  wait_for_dock(app)

  # The example sets the blockr option `locked`, so the bar shows the
  # read-only pill.
  expect_true(
    app$get_js(
      "document.querySelector('[aria-label=\"Read-only mode\"]') !== null"
    )
  )

  # No tab has a close button, and no tab group a "+" for the panel picker.
  expect_equal(
    app$get_js(
      "document.querySelectorAll('.blockr-dock .dv-default-tab-action').length"
    ),
    0
  )
  expect_equal(
    app$get_js("document.querySelectorAll('.blockr-dock-add').length"),
    0
  )

  # The blockr option `lock_rearrange` is on by default under the board lock,
  # so dockview offers no tab to drag.
  expect_identical(
    unique(
      unlist(
        app$get_js(
          paste0(
            "[...document.querySelectorAll('.blockr-dock .dv-tab')]",
            ".map((tab) => tab.getAttribute('draggable'))"
          )
        )
      )
    ),
    "false"
  )

  # A click on a tab still shows the panel, which changes only what the user
  # looks at.
  view <- app_dock(app)[["active"]]

  click_at(app, center_of(app, tab_js(1L, 2L)))

  wait_for_grid(
    app,
    view,
    function(grid) identical(group_active(tab_groups(grid)[[1L]]), "b"),
    "block b in front"
  )
})
