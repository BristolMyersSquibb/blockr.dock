test_that("new_dock_board splits the layout into structure and grid", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(
      A = dock_layout("a", "b", name = "Analysis"),
      B = dock_layout("a", name = "Overview")
    ),
    active = "B"
  )

  views <- board_views(brd)
  expect_s3_class(views, "dock_views")
  expect_identical(names(views), c("A", "B"))
  expect_identical(active_view(views), "B")
  expect_identical(unname(view_names(views)), c("Analysis", "Overview"))

  expect_s3_class(views[["A"]], "dock_view")
  expect_identical(
    view_members(views[["A"]]),
    c("block_panel-a", "block_panel-b")
  )
  expect_identical(view_members(views[["B"]]), "block_panel-a")

  arr <- board_grids(brd)
  expect_s3_class(arr, "dock_grids")
  expect_identical(names(arr), c("A", "B"))
  expect_identical(
    layout_panel_ids(arr[["A"]]),
    c("block_panel-a", "block_panel-b")
  )
})

test_that("structure stays bit-stable through an grid round-trip", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(A = dock_layout("a", "b", name = "Analysis"))
  )

  before <- board_views(brd)

  # A read-modify-write of the composed handle round-trips through the split
  # without perturbing the structure slot -- the property the mirror (#294)
  # relies on for identical()-gated narrow subscriptions.
  ly <- board_layouts(brd)
  board_layouts(brd) <- ly

  expect_identical(board_views(brd), before)
})

test_that("board_layouts composes the two slots back into a dock_layouts", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(
      A = dock_layout("a", panels("b", "a", active = "a"), sizes = c(0.4, 0.6),
                      name = "Analysis")
    )
  )

  # decompose then compose is an identity on the fused handle: geometry
  # (sizes, active tab, nesting) survives the split.
  expect_identical(
    board_layouts(brd),
    compose_layouts(board_views(brd), board_grids(brd))
  )
})

test_that("grid must be a subset of membership", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(A = dock_layout("a", "b", name = "Analysis"))
  )

  bad <- new_dock_grids(
    list(A = dock_layout("a", "b", "block_panel-ghost"))
  )

  expect_error(
    validate_dock_grids(bad, board_views(brd)),
    class = "dock_grid_not_subset"
  )

  expect_error(
    board_grids(brd) <- bad,
    class = "dock_grid_not_subset"
  )
})

test_that("an grid keyed by an unknown view is rejected", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    layouts = list(A = dock_layout("a", name = "Analysis"))
  )

  expect_error(
    board_grids(brd) <- new_dock_grids(
      list(ghost = dock_layout("a"))
    ),
    class = "dock_grids_unknown_view"
  )
})

test_that("a board is valid with no grid at all", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(
      A = dock_layout("a", "b", name = "Analysis"),
      B = dock_layout("a", name = "Overview")
    )
  )

  board_grids(brd) <- NULL

  expect_null(board_grids(brd))
  expect_s3_class(validate_board(brd), "dock_board")

  # Structure alone still drives a valid composed handle: each view gets a
  # default grid over its members, membership and active preserved.
  composed <- board_layouts(brd)
  expect_s3_class(composed, "dock_layouts")
  expect_identical(
    layout_panel_ids(composed[["A"]]),
    c("block_panel-a", "block_panel-b")
  )
  expect_identical(active_view(composed), active_view(board_views(brd)))
})

test_that("a single view's grid may be NULL", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(
      A = dock_layout("a", name = "Analysis"),
      B = dock_layout("b", name = "Overview")
    )
  )

  arr <- board_grids(brd)
  arr[["A"]] <- NULL
  board_grids(brd) <- arr

  expect_null(board_grids(brd)[["A"]])
  expect_s3_class(validate_board(brd), "dock_board")

  # The NULL view falls back to a default grid over its membership.
  expect_identical(layout_panel_ids(board_layouts(brd)[["A"]]), "block_panel-a")
})

test_that("board_views<- rejects a non-dock_views value", {

  brd <- new_dock_board(c(a = new_dataset_block()))

  expect_error(
    board_views(brd) <- list(A = "nope"),
    class = "dock_views_structure_invalid"
  )
})

test_that("rm_blocks drops the block from structure and grid", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(A = dock_layout("a", "b", name = "Analysis"))
  )

  brd <- rm_blocks(brd, "a")

  expect_identical(view_members(board_views(brd)[["A"]]), "block_panel-b")
  expect_identical(
    layout_panel_ids(board_grids(brd)[["A"]]),
    "block_panel-b"
  )
})

test_that("the split board round-trips through serialization", {

  brd <- new_dock_board(
    blocks = c(
      a = new_dataset_block(),
      b = new_head_block(),
      c = new_head_block()
    ),
    layouts = list(
      A = dock_layout("a", panels("b", "c", active = "c"), sizes = c(0.3, 0.7),
                      name = "Analysis"),
      B = dock_layout("a", name = "Overview")
    ),
    active = "B"
  )

  des <- blockr_deser(blockr_ser(brd))

  expect_identical(board_views(des), board_views(brd))
  expect_identical(board_grids(des), board_grids(brd))
})
