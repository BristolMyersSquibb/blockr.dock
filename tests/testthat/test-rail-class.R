test_that("rail() builds a rail and gates its arguments", {

  rl <- rail(blk("a"), blk("b"), position = "right", active = blk("b"))

  expect_s3_class(rl, "dock_rail")
  expect_identical(rl[["position"]], "right")
  expect_identical(rl[["panels"]], c("block_panel-a", "block_panel-b"))
  expect_identical(rl[["active"]], "block_panel-b")

  # The first panel fronts by default, and an empty rail has no open tab.
  expect_identical(rail(blk("a"), blk("b"))[["active"]], "block_panel-a")
  expect_null(rail()[["active"]])

  expect_false(rail(blk("a"))[["collapsed"]])
  expect_true(rail(blk("a"), collapsed = TRUE)[["collapsed"]])

  expect_error(
    rail(blk("a"), active = blk("b")),
    class = "dock_rail_active_invalid"
  )
  expect_error(
    rail(blk("a"), collapsed = "yes"),
    class = "dock_rail_collapsed_invalid"
  )
  expect_error(rail(blk("a"), position = "diagonal"))
  expect_error(rail(blk("a"), position = "top"))
  expect_error(rail(blk("a"), size = -1), class = "dock_rail_size_invalid")
})

test_that("a view carries at most one rail per edge", {

  expect_named(
    view_rail_set(list(rail(blk("a")), rail(blk("b"), position = "right"))),
    c("left", "right")
  )

  expect_error(
    view_rail_set(list(rail(blk("a")), rail(blk("b")))),
    class = "dock_rails_position_clash"
  )
})

test_that("a rail and the grid partition a view's membership", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    extensions = new_edit_board_extension(),
    views = list(V = c("a", "b", "edit_board")),
    grids = list(
      V = dock_grid(
        "a", "b",
        rail(ext("edit_board"), position = "right", size = 300)
      )
    )
  )

  expect_setequal(
    view_members(board_views(brd)[["V"]]),
    c("block_panel-a", "block_panel-b", "ext_panel-edit_board")
  )

  # `grid_tree_ids()` is the splitview half; `layout_panel_ids()` is everything
  # the grid places, tree and rails alike.
  expect_identical(
    grid_tree_ids(board_grids(brd)[["V"]]),
    c("block_panel-a", "block_panel-b")
  )
  expect_setequal(
    layout_panel_ids(board_grids(brd)[["V"]]),
    c("block_panel-a", "block_panel-b", "ext_panel-edit_board")
  )

  expect_identical(
    rail_panel_ids(board_grids(brd)[["V"]][["rails"]]), "ext_panel-edit_board"
  )

  expect_identical(board_grids(brd)[["V"]][["rails"]][["right"]][["size"]], 300)
})

test_that("a rail claims a member the grid also names", {

  # Both slots describe placement, so the same panel in both is ambiguous. The
  # rail wins and the grid tree loses it, keeping the partition total.
  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    extensions = new_edit_board_extension(),
    views = list(V = c("a", "edit_board")),
    grids = list(V = dock_grid("a", "edit_board", rail(ext("edit_board"))))
  )

  expect_identical(grid_tree_ids(board_grids(brd)[["V"]]), "block_panel-a")
  expect_identical(
    rail_panel_ids(board_grids(brd)[["V"]][["rails"]]), "ext_panel-edit_board"
  )
})

test_that("an empty rail is implied, and a ghost drops on read", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    views = list(V = "a"),
    grids = list(V = dock_grid("a", rail(position = "right")))
  )

  # Storing it would be noise: every dock offers every edge, so an empty rail
  # at its defaults says nothing the read does not already supply.
  expect_null(board_grids(brd)[["V"]][["rails"]])
  expect_named(active_view_grid(brd)[["rails"]], c("left", "right"))
  expect_identical(
    rail_panel_ids(active_view_grid(brd)[["rails"]]), character()
  )

  # A rail naming a panel the view no longer carries reports it gone, exactly
  # as `view_grid()` prunes a grid ghost.
  ghosted <- dock_grid("a", rail(blk("gone"), position = "right"))

  expect_identical(
    rail_panel_ids(
      restrict_grid(ghosted, view_members(board_views(brd)[["V"]]))[["rails"]]
    ),
    character()
  )
})

test_that("a board given only rails infers the view and its members", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    grids = list(V = dock_grid(rail(blk("a"))))
  )

  expect_named(board_views(brd), "V")
  expect_identical(view_members(board_views(brd)[["V"]]), "block_panel-a")

  # The sole member is railed, so the splitview half is empty while the grid
  # still places it.
  expect_identical(grid_tree_ids(active_view_grid(brd)), character())
  expect_identical(layout_panel_ids(active_view_grid(brd)), "block_panel-a")
})

test_that("rails reach dockView as edgeGroups, visibility derived", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    extensions = new_edit_board_extension(),
    views = list(V = c("a", "edit_board")),
    grids = list(
      V = dock_grid(
        "a", rail(ext("edit_board"), size = 300, collapsed_size = 20)
      )
    )
  )

  lay <- as_dock_layout(
    active_view_grid(brd), board_blocks(brd), dock_extensions(brd)
  )

  edge <- lay[["edgeGroups"]][["left"]]

  expect_true(edge[["visible"]])
  expect_identical(edge[["size"]], 300)
  expect_identical(edge[["collapsedSize"]], 20)
  expect_identical(edge[["group"]][["id"]], "rail-left")
  expect_identical(edge[["group"]][["views"]], list("ext_panel-edit_board"))
  expect_identical(edge[["group"]][["activeView"]], "ext_panel-edit_board")

  # The railed panel is resolved into `panels` but is absent from the grid
  # tree, which is what puts it in the rail rather than in a grid cell.
  expect_true("ext_panel-edit_board" %in% names(lay[["panels"]]))
  expect_false(
    "ext_panel-edit_board" %in% grid_tree_ids(as_dock_grid(lay))
  )

  # An empty rail is declared all the same, and hidden. This is the server half
  # of the derived-visibility rule -- what the dock is born as. The client half
  # (`sync()` re-asserting it as panels move) is covered by the e2e in
  # test-utils-serve.R; the two must agree.
  # The payload declares every edge, because it is what creates them client
  # side -- a rail has to exist before a drag can reveal it. The read supplies
  # the ones the grid does not store.
  empty <- as_dock_layout(
    view_grid(dock_view("block_panel-a"), dock_grid("block_panel-a")),
    board_blocks(brd),
    dock_extensions(brd)
  )

  expect_named(empty[["edgeGroups"]], c("left", "right"))

  empty_edge <- empty[["edgeGroups"]][["left"]]

  expect_false(empty_edge[["visible"]])
  expect_identical(empty_edge[["group"]][["views"]], list())
  expect_null(empty_edge[["group"]][["activeView"]])
})

test_that("a collapsed rail round-trips, so a restore comes back as left", {

  # Collapsing is a user gesture (a click on the rail's open tab), and unlike
  # visibility it is not derivable from anything -- an expanded rail and a
  # collapsed one hold the same panels. So it is stored.
  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    views = list(V = c("a", "b")),
    grids = list(
      V = dock_grid("a", rail(blk("b"), position = "right", collapsed = TRUE))
    )
  )

  expect_true(board_grids(brd)[["V"]][["rails"]][["right"]][["collapsed"]])
  expect_identical(board_grids(blockr_deser(blockr_ser(brd))), board_grids(brd))

  # It reaches dockView, and comes back off the echo.
  lay <- as_dock_layout(
    active_view_grid(brd), board_blocks(brd), dock_extensions(brd)
  )

  expect_true(lay[["edgeGroups"]][["right"]][["collapsed"]])
  expect_true(
    as_dock_rails(
      new_dock_layout(
        list(grid = list(), edgeGroups = lay[["edgeGroups"]])
      )
    )[["right"]][["collapsed"]]
  )
})

test_that("a client echo casts back to rails and a union membership", {

  # The shape dockview echoes: `edgeGroups` keyed by edge, beside `grid`.
  echo <- new_dock_layout(
    list(
      grid = grid_to_tree(dock_grid("block_panel-a")),
      activeGroup = "1",
      edgeGroups = list(
        left = list(
          size = 260,
          visible = TRUE,
          collapsedSize = 35,
          group = list(
            views = list("ext_panel-edit_board"),
            activeView = "ext_panel-edit_board",
            id = "rail-left",
            headerPosition = "left"
          )
        )
      )
    )
  )

  rails <- as_dock_rails(echo)

  expect_named(rails, "left")
  expect_identical(rails[["left"]][["panels"]], "ext_panel-edit_board")
  expect_identical(rails[["left"]][["size"]], 260)
  expect_identical(rails[["left"]][["collapsed_size"]], 35)

  # Membership is the union: reading the grid alone would report the railed
  # panel as gone the moment a drag parked it on an edge.
  expect_setequal(
    view_members(as_dock_view(echo)),
    c("block_panel-a", "ext_panel-edit_board")
  )

  expect_identical(as_dock_rails(new_dock_layout(list(grid = list()))), list())
})

test_that("a rail move rides the grid through the views delta", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    views = list(V = c("a", "b"), W = "a"),
    grids = list(V = dock_grid("a", "b"))
  )

  # The settled echo of a panel dragged onto the right edge: one grid, carrying
  # both halves, in one commit. Before the fold this needed a second delta key.
  moved <- apply_board_update(
    brd,
    list(
      views = list(
        grid = list(
          V = dock_grid(
            "block_panel-a", rail(blk("b"), position = "right")
          )
        )
      )
    )
  )

  grid <- board_grids(moved)[["V"]]

  expect_identical(grid_tree_ids(grid), "block_panel-a")
  expect_identical(rail_panel_ids(grid[["rails"]]), "block_panel-b")
  expect_setequal(layout_panel_ids(grid), c("block_panel-a", "block_panel-b"))
  expect_setequal(
    view_members(board_views(moved)[["V"]]),
    c("block_panel-a", "block_panel-b")
  )

  # Removing the view takes its grid, and so its rails, with it.
  gone <- apply_board_update(moved, list(views = list(rm = "V")))

  expect_null(board_grids(gone)[["V"]])
})

test_that("an unknown views slice key is still rejected", {

  brd <- new_dock_board(c(a = new_dataset_block()))

  expect_error(
    validate_views_delta(list(railz = list()), brd, list()),
    class = "dock_views_delta_invalid"
  )
})

test_that("a second rail on the same edge is rejected everywhere", {

  # Dockview allows one group per edge, so every route in has to say so --
  # including validation, where keying by position makes a duplicate look
  # well-formed (names and positions agree).
  expect_error(
    dock_grid(rail(blk("a")), rail(blk("b"))),
    class = "dock_rails_position_clash"
  )
  expect_error(
    new_dock_grid(
      rails = list(
        new_dock_rail("left", "block_panel-a"),
        new_dock_rail("left", "block_panel-b")
      )
    ),
    class = "dock_rails_position_clash"
  )
  expect_error(
    validate_dock_grid(
      structure(
        list(
          orientation = "horizontal", children = list(), sizes = numeric(),
          rails = list(
            left = new_dock_rail("left", "block_panel-a"),
            left = new_dock_rail("left", "block_panel-b")
          )
        ),
        class = "dock_grid"
      )
    ),
    class = "dock_rails_position_clash"
  )

  # Distinct edges are fine.
  expect_named(
    dock_grid(rail(blk("a")), rail(blk("b"), position = "right"))[["rails"]],
    c("left", "right")
  )
})

test_that("a grid gates its rails", {

  expect_error(
    validate_dock_grid(
      structure(
        list(orientation = "horizontal", children = list(), sizes = numeric(),
             rails = list(left = "not a rail")),
        class = "dock_grid"
      )
    ),
    class = "dock_rail_structure_invalid"
  )

  # Keyed by the edge each pins to, so a hand-written grid cannot drift.
  expect_error(
    validate_dock_grid(
      structure(
        list(orientation = "horizontal", children = list(), sizes = numeric(),
             rails = list(right = new_dock_rail("left"))),
        class = "dock_grid"
      )
    ),
    class = "dock_rails_ids_missing"
  )
})

test_that("a railed panel counts as on screen while the rail is expanded", {

  # `visible_block_ids()` drives the visible axis and core's render gate. It
  # reads the echo's grid, so a block a user parks on an edge would read as off
  # screen -- and never paint -- unless the rails fold in here too.
  echo <- function(visible = TRUE, collapsed = FALSE) {
    list(
      grid = grid_to_tree(dock_grid("block_panel-a")),
      edgeGroups = list(
        left = list(
          visible = visible,
          collapsed = collapsed,
          group = list(
            views = list("block_panel-b"),
            activeView = "block_panel-b",
            id = "rail-left"
          )
        )
      )
    )
  }

  expect_setequal(visible_block_ids(echo()), c("a", "b"))

  # A collapsed rail is a bare strip with no content pane, and a hidden one
  # renders at zero -- neither shows its tab.
  expect_identical(visible_block_ids(echo(collapsed = TRUE)), "a")
  expect_identical(visible_block_ids(echo(visible = FALSE)), "a")

  # The client's live active panel still overrides a stale echoed front.
  fronts <- determine_active_views(echo(), active_panel = "block_panel-b")
  expect_identical(fronts[["rail-left"]], "block_panel-b")

  # A compact grid carries no rails at all.
  expect_identical(
    visible_block_ids(dock_grid("block_panel-a")), "a"
  )
})
