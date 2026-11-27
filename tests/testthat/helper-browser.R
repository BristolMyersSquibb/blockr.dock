# What the end-to-end tests use.
# They drive Chrome through shinytest2, and act on the page as a user does,
# with the mouse input and the drags that they send through Chrome's DevTools
# protocol.
# They read what core gives end-to-end tests through `blockrTest` on the page:
# the board, each block's status, and the block that has focus.

# Starts an example app from `inst/examples`, by its name, with shinytest2, and
# stops it as the test ends.
# The window is 1400 pixels wide and 900 high, so that the dock lays out each
# example at the same size.
# The example's script attaches blockr.dock with `library()`.
# The tests may run from the package's source, as `devtools::test()` runs
# them.
# Then shinytest2 has that call load the source with `pkgload::load_all()`.
# So the app runs the dock under test, and not an older install.
# Under `R CMD check`, the app runs the package that the check installed.
local_example_app <- function(name, frame = parent.frame()) {

  testthat::skip_on_cran()
  testthat::skip_if_not_installed("shinytest2")

  chrome <- try(chromote::default_chromote_object(), silent = TRUE)
  testthat::skip_if(inherits(chrome, "try-error"), "Chrome does not start.")

  app <- shinytest2::AppDriver$new(
    system.file("examples", name, "app.R", package = "blockr.dock"),
    load_timeout = 30 * 1000,
    timeout = 30 * 1000,
    width = 1400,
    height = 900
  )

  withr::defer(app$stop(), envir = frame)

  app
}

# Waits until the page holds what the dock's frontend draws: a dockview, in
# the dock's element.
# After `timeout` seconds without it, it fails with the page's text.
# That text shows what the page holds in place of the dock, such as core's
# basic frontend or core's error.
wait_for_dock <- function(app, timeout = 30) {

  drawn <- try(
    app$wait_for_js(
      "document.querySelector('.blockr-dock .dv-shell') !== null",
      timeout = timeout * 1000
    ),
    silent = TRUE
  )

  if (inherits(drawn, "try-error")) {
    stop(
      "The dock drew no page within ", timeout, " seconds. The page shows:\n",
      app$get_js("document.body.innerText"),
      call. = FALSE
    )
  }

  invisible(app)
}

# Opens the app's page again, with the query given, as a user opens a link
# such as `?view=Second`.
# The page starts a new session, so it waits until the dock draws again.
open_query <- function(app, query) {

  page <- app$get_chromote_session()

  loaded <- page$Page$loadEventFired(wait_ = FALSE)
  page$Page$navigate(paste0(app$get_url(), query), wait_ = FALSE)
  page$wait_for(loaded)

  wait_for_dock(app)
}

# The board that the client holds, as the page gives it.
app_board <- function(app) {
  app$get_js("blockrTest.board()")
}

# The dock's state on the board that the client holds.
app_dock <- function(app) {
  app_board(app)[["plugins"]][["blockr.dock"]][["dock_plugin"]]
}

# The grid of the view given, from the dock's state.
app_grid <- function(app, view) {
  app_dock(app)[["views"]][[view]][["grid"]]
}

# The status of each of the board's blocks, by block id, as the page gives it.
app_statuses <- function(app) {
  app$get_js("blockrTest.statuses()")
}

# The id of the block that has focus, as the page gives it, or `NULL` where
# none has.
app_focus <- function(app) {
  app$get_js("blockrTest.focus()")
}

# Whether R has evaluated each block given: whether the page holds a reply of
# R's for the block, as a status holds the revision of its latest reply.
app_evaluated <- function(app, blocks) {
  statuses <- app_statuses(app)
  set_names(
    lgl_ply(blocks, function(id) not_null(statuses[[id]][["evaluated"]])),
    blocks
  )
}

# The shares of the top split of a grid, as numbers.
grid_sizes <- function(grid) {
  as.numeric(unlist(grid[["sizes"]]))
}

# The tab groups of a grid, depth first, in the order that dockview draws
# them.
tab_groups <- function(node) {

  if (not_null(node[["panels"]])) {
    return(list(node))
  }

  do.call(c, lapply(node[["children"]], tab_groups))
}

# What each panel of a tab group or rail shows: the id of its block, or its
# plugin's class as core's messages name an id, as
# `edit_board_plugin of blockr.core`.
group_panels <- function(group) {
  chr_ply(
    group[["panels"]],
    function(panel) coal(panel[["block"]], id_label(panel[["plugin"]]))
  )
}

# What the panel shows that a tab group or rail shows in front.
group_active <- function(group) {
  coal(group[["active"]][["block"]], group[["active"]][["plugin"]])
}

# Waits until `check()` gives `TRUE`, and fails with what `what()` says once
# `timeout` seconds have passed.
wait_until <- function(check, what, timeout = 30) {

  deadline <- Sys.time() + timeout

  repeat {

    if (isTRUE(check())) {
      return(invisible())
    }

    if (Sys.time() > deadline) {
      stop("Waited ", timeout, " seconds for ", what(), call. = FALSE)
    }

    Sys.sleep(0.1)
  }
}

# Waits until `check()`, a function of the grid of the view given, gives
# `TRUE`, and fails with the grid as the page holds it.
wait_for_grid <- function(app, view, check, what) {
  wait_until(
    function() check(app_grid(app, view)),
    function() {
      paste0(
        what, ", where the grid of the view ", view, " is ",
        app$get_js(
          paste0(
            "JSON.stringify(blockrTest.board().plugins",
            "['blockr.dock'].dock_plugin.views['", view, "'].grid)"
          )
        )
      )
    }
  )
}

# Waits until R has evaluated each block given.
wait_for_evaluated <- function(app, blocks) {
  wait_until(
    function() all(app_evaluated(app, blocks)),
    function() {
      paste0(
        "R to evaluate ", paste(blocks, collapse = ", "),
        ", where the page gives the statuses ",
        paste(
          names(app_statuses(app)),
          chr_xtr(app_statuses(app), "status"),
          sep = ": ",
          collapse = ", "
        )
      )
    }
  )
}

# Waits until the block given has focus.
wait_for_focus <- function(app, block) {
  wait_until(
    function() identical(app_focus(app), block),
    function() {
      paste0(
        "block ", block, " to take focus, where ",
        coal(app_focus(app), "no block"), " has it"
      )
    }
  )
}

# The JS that finds the tab groups of the view that the dock draws, without
# its rails, in the order of the grid.
groups_js <- paste0(
  "[...document.querySelectorAll(",
  "'.blockr-dock .dv-groupview:not(.dv-groupview-edge)'",
  ")]"
)

# The JS that finds a tab group, by its place in the order of the grid.
group_js <- function(group) {
  paste0(groups_js, "[", group - 1L, "]")
}

# The JS that finds a tab of a tab group, by the places of both.
tab_js <- function(group, tab) {
  paste0(group_js(group), ".querySelectorAll('.dv-tab')[", tab - 1L, "]")
}

# The JS that finds the tab of the panel that the rail on the edge given shows.
rail_tab_js <- function(edge) {
  paste0(
    "document.querySelector(",
    "'.blockr-dock .dv-groupview-edge.dv-groupview-header-", edge, " .dv-tab'",
    ")"
  )
}

# The box of the element that the JS given finds, as its left, top, width
# and height in CSS pixels.
box_of <- function(app, js) {

  box <- app$get_js(
    paste0(
      "(() => {\n",
      "  const el = ", js, ";\n",
      "  if (!el) return null;\n",
      "  const box = el.getBoundingClientRect();\n",
      "  return [box.x, box.y, box.width, box.height];\n",
      "})()"
    )
  )

  if (is.null(box)) {
    stop("The page holds no element for ", js, call. = FALSE)
  }

  set_names(as.numeric(unlist(box)), c("x", "y", "width", "height"))
}

# The point at the middle of the element that the JS given finds.
center_of <- function(app, js) {
  box <- box_of(app, js)
  c(
    x = box[["x"]] + box[["width"]] / 2,
    y = box[["y"]] + box[["height"]] / 2
  )
}

# Sends the mouse event of the type given at the point given, with the left
# button down where `buttons` is 1.
mouse <- function(app, type, at, buttons = 0L, ...) {
  invisible(
    app$get_chromote_session()$Input$dispatchMouseEvent(
      type = type,
      x = at[["x"]],
      y = at[["y"]],
      button = "left",
      buttons = buttons,
      ...
    )
  )
}

# Clicks at the point given, as a user does: the pointer moves there, and the
# left button goes down and up.
click_at <- function(app, at) {
  mouse(app, "mouseMoved", at)
  mouse(app, "mousePressed", at, 1L, clickCount = 1L)
  mouse(app, "mouseReleased", at, 0L, clickCount = 1L)
}

# Drags with the left button from one point to another, in steps, as a user
# drags a sash.
# The function `held()` runs once the pointer has reached `to`, before the
# button goes up.
drag_mouse <- function(app, from, to, held = function() NULL, steps = 10L) {

  mouse(app, "mouseMoved", from)
  mouse(app, "mousePressed", from, 1L, clickCount = 1L)

  for (i in seq_len(steps)) {
    mouse(app, "mouseMoved", from + (to - from) * i / steps, 1L)
  }

  held()

  mouse(app, "mouseReleased", to, 0L, clickCount = 1L)
}

# Drags a tab from one point and drops it at another, as a user does.
# Dockview moves a tab with HTML5 drag and drop.
# So Chrome intercepts the drag that the mouse starts on the tab, and the
# test drops what Chrome intercepted at `to`.
# A drop within 35 pixels of the window's edge lands on the rail there, which
# the dock reveals as a drag dwells by the edge.
drag_tab <- function(app, from, to) {

  page <- app$get_chromote_session()

  page$Input$setInterceptDrags(enabled = TRUE)
  withr::defer(page$Input$setInterceptDrags(enabled = FALSE))

  dragged <- new.env()
  stop_listening <- page$Input$dragIntercepted(
    callback_ = function(event) dragged$data <- event$data
  )
  withr::defer(stop_listening())

  mouse(app, "mouseMoved", from)
  mouse(app, "mousePressed", from, 1L, clickCount = 1L)

  for (i in seq_len(5L)) {
    mouse(app, "mouseMoved", from + 5 * i, 1L)
  }

  # Chrome calls back as it handles the page's next message.
  wait_until(
    function() {
      app$get_js("true")
      not_null(dragged$data)
    },
    function() "Chrome to start the drag of the tab"
  )

  for (type in c("dragEnter", rep("dragOver", 5L), "drop")) {
    page$Input$dispatchDragEvent(
      type = type,
      x = to[["x"]],
      y = to[["y"]],
      data = dragged$data
    )
  }

  mouse(app, "mouseReleased", to, 0L, clickCount = 1L)
}
