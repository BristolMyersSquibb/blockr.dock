test_that("new_dock_board stores structure and geometry as two slots", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    views = list(
      A = dock_view(c("a", "b"), name = "Analysis"),
      B = dock_view("a", name = "Overview")
    ),
    grids = list(
      A = dock_grid("a", "b"),
      B = dock_grid("a")
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

  # Geometry is stored verbatim -- each view keeps its explicit grid, dropped
  # to the intersection with membership only where placement is read.
  arr <- board_grids(brd)
  expect_s3_class(arr, "dock_grids")
  expect_identical(names(arr), c("A", "B"))
  expect_true(is_dock_grid(arr[["A"]]))
  expect_true(is_dock_grid(arr[["B"]]))
})

test_that("bare member vectors coerce into views without an explicit grid", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    views = list(A = c("a", "b"), B = "a")
  )

  views <- board_views(brd)
  expect_identical(
    view_members(views[["A"]]),
    c("block_panel-a", "block_panel-b")
  )
  expect_identical(view_members(views[["B"]]), "block_panel-a")

  # No grids supplied -> the slot is an empty dock_grids; placement falls back
  # to a default grid over each view's members.
  expect_length(board_grids(brd), 0L)
  expect_identical(
    layout_panel_ids(view_grid(views[["A"]], board_grids(brd)[["A"]])),
    c("block_panel-a", "block_panel-b")
  )
})

test_that("view_grid prunes a ghost panel outside membership", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    views = list(A = dock_view(c("a", "b"), name = "Analysis"))
  )

  # Total semantics: a grid entry with no membership is an inert ghost, legal on
  # a committed board and pruned only where placement is read.
  ghost <- as_dock_grid(
    dock_grid("block_panel-a", "block_panel-b", "block_panel-ghost")
  )
  board_grids(brd) <- new_dock_grids(list(A = ghost))

  expect_s3_class(validate_board(brd), "dock_board")

  placement <- view_grid(board_views(brd)[["A"]], board_grids(brd)[["A"]])
  expect_false("block_panel-ghost" %in% layout_panel_ids(placement))
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
    views = list(A = dock_view("a", name = "Analysis"))
  )

  expect_error(
    board_grids(brd) <- new_dock_grids(
      list(ghost = dock_grid("a"))
    ),
    class = "dock_grids_unknown_view"
  )
})

test_that("a board is valid with no arrangement at all", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    views = list(
      A = dock_view(c("a", "b"), name = "Analysis"),
      B = dock_view("a", name = "Overview")
    ),
    grids = list(A = dock_grid("a", "b"))
  )

  board_grids(brd) <- NULL

  expect_null(board_grids(brd))
  expect_s3_class(validate_board(brd), "dock_board")

  # Structure alone still drives a valid placement: each view gets a default
  # grid over its members.
  expect_identical(
    layout_panel_ids(view_grid(board_views(brd)[["A"]], NULL)),
    c("block_panel-a", "block_panel-b")
  )
})

test_that("a single view's arrangement may be NULL", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    views = list(
      A = dock_view("a", name = "Analysis"),
      B = dock_view("b", name = "Overview")
    ),
    grids = list(A = dock_grid("a"), B = dock_grid("b"))
  )

  arr <- board_grids(brd)
  arr[["A"]] <- NULL
  board_grids(brd) <- arr

  expect_null(board_grids(brd)[["A"]])
  expect_s3_class(validate_board(brd), "dock_board")

  # The NULL view falls back to a default grid over its membership.
  expect_identical(
    layout_panel_ids(
      view_grid(board_views(brd)[["A"]], board_grids(brd)[["A"]])
    ),
    "block_panel-a"
  )
})

test_that("board_views<- rejects a non-dock_views value", {

  brd <- new_dock_board(c(a = new_dataset_block()))

  expect_error(
    board_views(brd) <- list(A = "nope"),
    class = "dock_views_structure_invalid"
  )
})

test_that("rm_blocks drops the member; the grid keeps it as a ghost", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    views = list(A = dock_view(c("a", "b"), name = "Analysis")),
    grids = list(A = dock_grid("a", "b"))
  )

  brd <- rm_blocks(brd, "a")

  # Membership drops the block (a set op); geometry is mirror-owned, so the grid
  # keeps the panel as an inert ghost, pruned only where placement is read.
  expect_identical(view_members(board_views(brd)[["A"]]), "block_panel-b")
  expect_true(
    "block_panel-a" %in% layout_panel_ids(board_grids(brd)[["A"]])
  )
  expect_identical(
    layout_panel_ids(
      view_grid(board_views(brd)[["A"]], board_grids(brd)[["A"]])
    ),
    "block_panel-b"
  )
})

test_that("view_grid renders a member the grid omits, defaulting its spot", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    views = list(A = dock_view(c("a", "b"), name = "Analysis")),
    grids = list(A = dock_grid("a"))
  )

  # Membership is authoritative: b belongs to the view, so it is placed even
  # though the grid mentions only a -- a default spot is appended, not dropped.
  expect_setequal(
    layout_panel_ids(
      view_grid(board_views(brd)[["A"]], board_grids(brd)[["A"]])
    ),
    c("block_panel-a", "block_panel-b")
  )
})

test_that("construction drops members with no backing block or extension", {

  # The block / extension set is authoritative. A member referencing a block
  # that is not on the board (e.g. dropped since the board was saved) is pruned
  # at construction rather than rejected -- restore of a stale board self-heals.
  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    views = list(A = dock_view(c("block_panel-a", "block_panel-gone")))
  )

  expect_identical(view_members(board_views(brd)[["A"]]), "block_panel-a")
  expect_s3_class(validate_board(brd), "dock_board")
})

test_that("construction restricts a grid to its view's members", {

  # A grid entry for a panel the view does not hold (a ghost) is dropped at
  # construction, so stored geometry never outlives the membership.
  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    views = list(A = dock_view("a")),
    grids = list(A = dock_grid("a", "b"))
  )

  expect_identical(layout_panel_ids(board_grids(brd)[["A"]]), "block_panel-a")
})

test_that("the default grid groups extensions left, blocks right, tabbed", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    extensions = new_edit_board_extension()
  )

  grid <- active_view_grid(brd)

  expect_length(grid[["children"]], 2L)
  expect_setequal(
    grid[["children"]][[1L]][["panels"]],
    "ext_panel-edit_board"
  )
  expect_setequal(
    grid[["children"]][[2L]][["panels"]],
    c("block_panel-a", "block_panel-b")
  )
})

test_that("a grid-less view falls back to the same ext / blocks default", {

  # A view with no grid entry lays out identically to a fresh board.
  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    extensions = new_edit_board_extension(),
    views = list(A = c("a", "b", "edit_board"))
  )

  grid <- view_grid(board_views(brd)[["A"]], board_grids(brd)[["A"]])

  expect_length(grid[["children"]], 2L)
  expect_setequal(
    grid[["children"]][[1L]][["panels"]],
    "ext_panel-edit_board"
  )
  expect_setequal(
    grid[["children"]][[2L]][["panels"]],
    c("block_panel-a", "block_panel-b")
  )
})

test_that("rm_blocks drops the member but leaves a legal arrangement ghost", {

  brd <- new_dock_board(
    blocks = c(
      a = new_dataset_block(), b = new_head_block(), c = new_head_block()
    ),
    views = list(A = dock_view(c("a", "b", "c"), name = "A")),
    grids = list(A = dock_grid("a", "b", "c", sizes = c(0.2, 0.3, 0.5)))
  )

  brd <- rm_blocks(brd, "b")

  expect_setequal(
    view_members(board_views(brd)[["A"]]),
    c("block_panel-a", "block_panel-c")
  )
  expect_true(
    "block_panel-b" %in% layout_panel_ids(board_grids(brd)[["A"]])
  )
  expect_s3_class(validate_board(brd), "dock_board")
  expect_setequal(
    layout_panel_ids(
      view_grid(board_views(brd)[["A"]], board_grids(brd)[["A"]])
    ),
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
    views = list(
      A = dock_view(c("a", "b", "c"), name = "Analysis"),
      B = dock_view("a", name = "Overview")
    ),
    grids = list(
      A = dock_grid("a", panels("b", "c", active = "c"), sizes = c(0.3, 0.7))
    ),
    active = "B"
  )

  des <- blockr_deser(blockr_ser(brd))

  expect_identical(board_views(des), board_views(brd))
  expect_identical(board_grids(des), board_grids(brd))
})

test_that("dock_view has a constructor and a validator", {

  v <- dock_view(c("block_panel-a", "block_panel-b"), name = "X")

  expect_s3_class(v, "dock_view")
  expect_identical(validate_dock_view(v), v)
  expect_identical(view_members(v), c("block_panel-a", "block_panel-b"))
  expect_identical(view_name(v), "X")

  # Invariants: members a character vector, name a string or NULL.
  expect_error(
    validate_dock_view(structure(list(members = 1:2), class = "dock_view")),
    class = "dock_view_members_invalid"
  )
  bad_name <- dock_view("block_panel-a")
  attr(bad_name, "view_name") <- c("a", "b")
  expect_error(validate_dock_view(bad_name), class = "dock_view_name_invalid")
})
