# A dockview `_state` echo as get_dock() surfaces it: grid + active group.
echo_state <- function(layout) {
  list(grid = layout[["grid"]], activeGroup = layout[["activeGroup"]])
}

# Settle the mirror's 250 ms debounce bridge: flush so its internal observer
# schedules the timer, then advance the mock clock past the window to fire it.
settle <- function(ms) {
  ms$flushReact()
  ms$elapse(300)
}

test_that("canonicalize_grid is idempotent and keeps sizes verbatim", {

  ly <- dock_layout("a", "b", sizes = c(0.301, 0.699))

  expect_identical(
    canonicalize_grid(ly),
    canonicalize_grid(canonicalize_grid(ly))
  )

  # Sizes are stored faithfully now -- the jitter is not rounded away here, it
  # is tolerated at the commit guard (see the `all.equal` test below).
  expect_equal(grid_to_spec(canonicalize_grid(ly)[["grid"]])[["sizes"]],
               c(0.301, 0.699))
})

test_that("all.equal.dock_layout absorbs sash jitter but not a real drag", {

  base   <- dock_layout("a", "b", "c", sizes = c(0.30, 0.40, 0.30))
  jitter <- dock_layout("a", "b", "c", sizes = c(0.303, 0.398, 0.299))
  drag   <- dock_layout("a", "b", "c", sizes = c(0.45, 0.30, 0.25))

  expect_true(isTRUE(all.equal(base, jitter, tolerance = grid_size_tol())))
  expect_false(isTRUE(all.equal(base, drag, tolerance = grid_size_tol())))

  # At R's default tolerance the comparison stays near-exact, so a plain
  # `all.equal()` / `expect_equal()` on a layout is not silently fuzzed.
  expect_false(isTRUE(all.equal(base, jitter)))
})

test_that("the size tolerance is a tunable blockr_option", {

  expect_equal(grid_size_tol(), 0.005)

  withr::with_options(
    list(blockr.dock_grid_size_tol = 0.02),
    expect_equal(grid_size_tol(), 0.02)
  )
})

test_that("project_grid canonicalises and elides a plain default", {

  # A plain default (even split of its own panels, in order) has no geometry.
  expect_null(
    project_grid(dock_layout("block_panel-a", "block_panel-b"))
  )

  # A non-default layout is kept verbatim in canonical form -- no membership
  # projection, so every panel survives (a ghost is dealt with at the boundary).
  kept <- project_grid(
    dock_layout(
      "block_panel-a", "block_panel-b", "block_panel-c",
      sizes = c(0.2, 0.3, 0.5)
    )
  )
  expect_setequal(
    layout_panel_ids(kept),
    c("block_panel-a", "block_panel-b", "block_panel-c")
  )
})

test_that("grid mirror commits one echo, guards re-echoes", {

  brd <- new_dock_board(
    blocks = c(
      a = new_dataset_block(), b = new_head_block(), d = new_head_block()
    ),
    layouts = list(V = dock_layout("a", "b", "d"))
  )

  ms <- new_mock_session()
  withr::defer(if (!ms$isClosed()) ms$close())

  committed <- list()

  with_mock_context(ms, {

    board <- reactiveValues(board = brd)
    layout_rv <- reactiveVal(NULL)

    observe_grid_echo(
      "V", list(layout = layout_rv), board,
      commit_grid = function(grid) {
        committed[[length(committed) + 1L]] <<- grid
        board$board <- apply_board_update(
          board$board,
          list(views = list(grid = setNames(list(grid), "V")))
        )
      }
    )
    ms$flushReact()

    # A settled sash drag: a non-default grid -> exactly one commit.
    dragged <- dock_layout(
      "block_panel-a", "block_panel-b", "block_panel-d",
      sizes = c(0.2, 0.3, 0.5)
    )
    layout_rv(echo_state(dragged))
    settle(ms)

    expect_length(committed, 1L)
    expect_false(is.null(board_grids(board$board)[["V"]]))

    # A re-echo differing only by sub-tolerance jitter is the same layout to the
    # all.equal guard, so it writes nothing.
    jittered <- dock_layout(
      "block_panel-a", "block_panel-b", "block_panel-d",
      sizes = c(0.201, 0.299, 0.5)
    )
    layout_rv(echo_state(jittered))
    settle(ms)

    expect_length(committed, 1L)
  })
})

test_that("the debounce bridge coalesces per-frame echoes into one commit", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(V = dock_layout("a", "b"))
  )

  ms <- new_mock_session()
  withr::defer(if (!ms$isClosed()) ms$close())

  committed <- list()

  with_mock_context(ms, {

    board <- reactiveValues(board = brd)
    layout_rv <- reactiveVal(NULL)

    observe_grid_echo(
      "V", list(layout = layout_rv), board,
      commit_grid = function(grid) {
        committed[[length(committed) + 1L]] <<- grid
      }
    )
    ms$flushReact()

    # A sash drag streams several per-frame states before settling. Each frame
    # arrives within the debounce window (no clock advance between them), so the
    # bridge commits only the settled frame -- one board write for the gesture,
    # not one per frame.
    for (w in c(0.25, 0.30, 0.35, 0.40)) {
      layout_rv(echo_state(dock_layout(
        "block_panel-a", "block_panel-b", sizes = c(w, 1 - w)
      )))
      ms$flushReact()
    }

    expect_length(committed, 0L)

    settle(ms)

    expect_length(committed, 1L)
    expect_setequal(
      layout_panel_ids(committed[[1L]]),
      c("block_panel-a", "block_panel-b")
    )
  })
})

test_that("the mirror stores an in-flight echo verbatim; compose prunes it", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(V = dock_layout("a", "b"))
  )

  ms <- new_mock_session()
  withr::defer(if (!ms$isClosed()) ms$close())

  committed <- list()

  with_mock_context(ms, {

    board <- reactiveValues(board = brd)
    layout_rv <- reactiveVal(NULL)

    observe_grid_echo(
      "V", list(layout = layout_rv), board,
      commit_grid = function(grid) {
        committed[[length(committed) + 1L]] <<- grid
        board$board <- apply_board_update(
          board$board,
          list(views = list(grid = setNames(list(grid), "V")))
        )
      }
    )
    ms$flushReact()

    # An echo that still carries a panel dropped from membership is stored
    # verbatim as an inert ghost (total semantics); the composed handle prunes
    # it, so the view shows the intersection.
    layout_rv(echo_state(dock_layout(
      "block_panel-a", "block_panel-b", "block_panel-gone",
      sizes = c(0.2, 0.3, 0.5)
    )))
    settle(ms)

    expect_true(
      "block_panel-gone" %in%
        layout_panel_ids(board_grids(board$board)[["V"]])
    )
    expect_false(
      "block_panel-gone" %in%
        layout_panel_ids(board_layouts(board$board)[["V"]])
    )
  })
})

test_that("validate_grid_value tolerates size jitter but not real drift", {

  # canonicalize_grid normalises sizes, so re-canonicalising an arbitrary ratio
  # can shift the last bit -- the values a real drag commits are not bit-exact
  # idempotent. The fixed-point check must use the mirror's size tolerance (not
  # identical()), or the mirror's own committed grids fail validation. Find such
  # a grid: one project_grid() output that is not its own identical() fixed
  # point.
  set.seed(1)
  drifter <- NULL

  for (k in seq_len(200L)) {
    g <- project_grid(
      dock_layout("block_panel-a", "block_panel-b", sizes = runif(2L, 0.1, 1))
    )
    if (!identical(g, canonicalize_grid(g))) {
      drifter <- g
      break
    }
  }

  expect_false(is.null(drifter))
  expect_no_error(validate_grid_value(drifter, "V"))

  # A grid whose sizes are not normalised is a real, past-tolerance departure
  # from its canonical form, so it is still rejected -- the tolerance absorbs
  # last-bit jitter, not a genuinely different layout.
  bad <- dock_layout("block_panel-a", "block_panel-b", sizes = c(1, 9))

  expect_error(
    validate_grid_value(bad, "V"),
    class = "dock_grid_not_canonical"
  )
})
