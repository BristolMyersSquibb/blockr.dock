test_that("new_dock_board splits the layout into structure and arrangement", {

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

  # A plain default arrangement (even split over the members, in order) is
  # elided to NULL; the structure slot alone reconstructs it.
  arr <- board_grids(brd)
  expect_s3_class(arr, "dock_grids")
  expect_identical(names(arr), c("A", "B"))
  expect_null(arr[["A"]])
  expect_null(arr[["B"]])
})

test_that("structure stays bit-stable through an arrangement round-trip", {

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

test_that("a grid may hold a ghost panel outside membership", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(A = dock_layout("a", "b", name = "Analysis"))
  )

  # Total semantics: a grid entry with no membership is an inert ghost, legal on
  # a committed board and pruned only when the view is composed.
  ghost <- as_dock_grid(
    dock_layout("block_panel-a", "block_panel-b", "block_panel-ghost")
  )
  board_grids(brd) <- new_dock_grids(list(A = ghost))

  expect_s3_class(validate_board(brd), "dock_board")
  expect_false(
    "block_panel-ghost" %in% layout_panel_ids(board_layouts(brd)[["A"]])
  )
})

test_that("validate rejects a member with no backing block or extension", {

  # Unlike a ghost arrangement entry, a member must reference a block or
  # extension on the board (members subset-of blocks, referential integrity).
  bad <- reconstruct_dock_views(
    list(A = new_dock_view(c("block_panel-a", "block_panel-ghost")))
  )

  expect_error(
    validate_view_membership(bad, "block_panel-a"),
    class = "dock_view_membership_unknown"
  )
})

test_that("an arrangement keyed by an unknown view is rejected", {

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

test_that("a board is valid with no arrangement at all", {

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

test_that("a single view's arrangement may be NULL", {

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

test_that("rm_blocks drops the block from structure and arrangement", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(A = dock_layout("a", "b", name = "Analysis"))
  )

  brd <- rm_blocks(brd, "a")

  expect_identical(view_members(board_views(brd)[["A"]]), "block_panel-b")
  # One surviving panel has no geometry, so its arrangement elides to NULL.
  expect_null(board_grids(brd)[["A"]])
})

test_that("rm_blocks drops the member but leaves a legal arrangement ghost", {

  brd <- new_dock_board(
    blocks = c(
      a = new_dataset_block(), b = new_head_block(), c = new_head_block()
    ),
    layouts = list(
      A = dock_layout("a", "b", "c", sizes = c(0.2, 0.3, 0.5), name = "A")
    )
  )

  brd <- rm_blocks(brd, "b")

  # Membership drops the block (a set op); the stored arrangement keeps it as an
  # inert ghost -- legal under total semantics -- and the composed handle prunes
  # it, so the view shows the intersection.
  expect_setequal(
    view_members(board_views(brd)[["A"]]),
    c("block_panel-a", "block_panel-c")
  )
  expect_true(
    "block_panel-b" %in% layout_panel_ids(board_grids(brd)[["A"]])
  )
  expect_s3_class(validate_board(brd), "dock_board")
  expect_setequal(
    layout_panel_ids(board_layouts(brd)[["A"]]),
    c("block_panel-a", "block_panel-c")
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

test_that("as_dock_views / as_dock_grids invert compose_layouts", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(
      V1 = dock_layout("a", "b", sizes = c(0.3, 0.7), name = "Split"),
      V2 = dock_layout("a", "b", name = "Even")
    ),
    active = "V2"
  )

  fused <- board_layouts(brd)
  views <- as_dock_views(fused)
  grids <- as_dock_grids(fused)

  expect_s3_class(views, "dock_views")
  expect_s3_class(grids, "dock_grids")

  # Membership, name and the active marker land on the views.
  expect_identical(view_name(views[["V1"]]), "Split")
  expect_identical(active_view(views), "V2")

  # An expressed grid is a canonical dock_grid; a plain default elides to NULL.
  expect_true(is_dock_grid(grids[["V1"]]))
  expect_null(grids[["V2"]])

  # The split inverts compose_layouts().
  expect_identical(views, board_views(brd))
  expect_identical(grids, board_grids(brd))
})
