# A dockview `_state` echo as get_dock() surfaces it: the grid tree + active
# group, expanded from the layout's canonical `dock_grid`.
echo_state <- function(layout) {
  grid <- as_dock_grid(layout)
  tree <- grid_to_tree(grid)
  list(grid = tree, activeGroup = focus_group_id(tree, grid[["focus"]]))
}

test_that("as_dock_grid casts to a canonical dock_grid, sizes verbatim", {

  ly <- dock_grid("a", "b", sizes = c(0.301, 0.699))
  grid <- as_dock_grid(ly)

  expect_s3_class(grid, "dock_grid")
  expect_true(is_dock_grid(grid))

  # Canonical by construction, so the cast is idempotent -- the `.dock_grid`
  # method returns it unchanged.
  expect_identical(grid, as_dock_grid(grid))

  # The dockView `_state` shape, wrapped as a `dock_layout`, casts to the same
  # grid as the layout it echoes.
  expect_identical(grid, as_dock_grid(new_dock_layout(echo_state(ly))))

  # Sizes are stored faithfully -- the jitter is not rounded away here, it is
  # tolerated at the commit guard (see the `all.equal` test below).
  expect_equal(grid[["sizes"]], c(0.301, 0.699))
})

test_that("all.equal.dock_grid absorbs sash jitter but not a real drag", {

  base   <- dock_grid("a", "b", "c", sizes = c(0.30, 0.40, 0.30))
  jitter <- dock_grid("a", "b", "c", sizes = c(0.303, 0.398, 0.299))
  drag   <- dock_grid("a", "b", "c", sizes = c(0.45, 0.30, 0.25))

  expect_true(isTRUE(all.equal(base, jitter, tolerance = grid_size_tol())))
  expect_false(isTRUE(all.equal(base, drag, tolerance = grid_size_tol())))

  # At R's default tolerance the comparison stays near-exact, so a plain
  # `all.equal()` / `expect_equal()` on a grid is not silently fuzzed.
  expect_false(isTRUE(all.equal(base, jitter)))
})

test_that("all.equal.dock_grid ignores the transient focus marker", {

  # The client's echo carries a `focus` (the active group) the authored seed
  # grid never has; a focus-only difference is not a geometry change, so the
  # mirror's guard -- and the round-trip probe -- must read the two as equal
  # (else a restore echo commits and the round-trip destabilises).
  plain <- dock_grid("block_panel-a", "block_panel-b", sizes = c(0.3, 0.7))
  focused <- plain
  focused[["focus"]] <- "block_panel-b"

  expect_true(isTRUE(all.equal(plain, focused)))
  expect_true(isTRUE(all.equal(plain, focused, tolerance = grid_size_tol())))
})

test_that("the size tolerance is a tunable blockr_option", {

  expect_equal(grid_size_tol(), 0.005)

  withr::with_options(
    list(blockr.dock_grid_size_tol = 0.02),
    expect_equal(grid_size_tol(), 0.02)
  )
})

test_that("grid mirror commits a client echo, guards re-echoes", {

  brd <- new_dock_board(
    blocks = c(
      a = new_dataset_block(), b = new_head_block(), d = new_head_block()
    ),
    views = list(V = c("a", "b", "d")),
    grids = list(V = dock_grid("a", "b", "d"))
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
    dragged <- dock_grid(
      "block_panel-a", "block_panel-b", "block_panel-d",
      sizes = c(0.2, 0.3, 0.5)
    )
    layout_rv(echo_state(dragged))
    ms$flushReact()

    expect_length(committed, 1L)
    expect_false(is.null(board_grids(board$board)[["V"]]))

    # A re-echo differing only by sub-tolerance jitter is the same layout to the
    # all.equal guard, so it writes nothing.
    jittered <- dock_grid(
      "block_panel-a", "block_panel-b", "block_panel-d",
      sizes = c(0.201, 0.299, 0.5)
    )
    layout_rv(echo_state(jittered))
    ms$flushReact()

    expect_length(committed, 1L)

    # A later distinct settled echo -- a move / resize / add the client
    # realises -- commits like the first.
    layout_rv(echo_state(dock_grid(
      "block_panel-a", "block_panel-b", "block_panel-d",
      sizes = c(0.6, 0.2, 0.2)
    )))
    ms$flushReact()

    expect_length(committed, 2L)
  })
})

test_that("the mirror stores an in-flight echo verbatim; placement prunes it", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    views = list(V = c("a", "b")),
    grids = list(V = dock_grid("a", "b"))
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
    # verbatim as an inert ghost (total semantics); the placement grid prunes
    # it, so the view shows the intersection.
    layout_rv(echo_state(dock_grid(
      "block_panel-a", "block_panel-b", "block_panel-gone",
      sizes = c(0.2, 0.3, 0.5)
    )))
    ms$flushReact()

    expect_true(
      "block_panel-gone" %in%
        layout_panel_ids(board_grids(board$board)[["V"]])
    )

    placement <- view_grid(
      board_views(board$board)[["V"]], board_grids(board$board)[["V"]]
    )
    expect_false("block_panel-gone" %in% layout_panel_ids(placement))
  })
})

test_that("validate_dock_grid enforces the canonical invariant", {

  g <- as_dock_grid(dock_grid("a", "b", sizes = c(0.3, 0.7)))
  expect_identical(validate_dock_grid(g), g)

  # A plain list is not a dock_grid.
  expect_error(
    validate_dock_grid(list(grid = list())),
    class = "dock_grid_structure_invalid"
  )

  # A non-canonical grid -- branch sizes that no longer sum to 1 -- is rejected.
  fake <- g
  fake[["sizes"]] <- c(0.9, 0.7)
  expect_error(validate_dock_grid(fake), class = "dock_grid_not_canonical")
})
