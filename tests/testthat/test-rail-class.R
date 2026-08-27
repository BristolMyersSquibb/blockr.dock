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
    grids = list(V = dock_grid("a", "b")),
    rails = list(V = rail(ext("edit_board"), position = "right", size = 300))
  )

  expect_setequal(
    view_members(board_views(brd)[["V"]]),
    c("block_panel-a", "block_panel-b", "ext_panel-edit_board")
  )

  expect_identical(
    layout_panel_ids(board_grids(brd)[["V"]]),
    c("block_panel-a", "block_panel-b")
  )

  expect_identical(
    rail_panel_ids(board_rails(brd)[["V"]]), "ext_panel-edit_board"
  )

  expect_identical(board_rails(brd)[["V"]][["right"]][["size"]], 300)
})

test_that("a rail claims a member the grid also names", {

  # Both slots describe placement, so the same panel in both is ambiguous. The
  # rail wins and the grid tree loses it, keeping the partition total.
  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    extensions = new_edit_board_extension(),
    views = list(V = c("a", "edit_board")),
    grids = list(V = dock_grid("a", "edit_board")),
    rails = list(V = rail(ext("edit_board")))
  )

  expect_identical(layout_panel_ids(board_grids(brd)[["V"]]), "block_panel-a")
  expect_identical(
    rail_panel_ids(board_rails(brd)[["V"]]), "ext_panel-edit_board"
  )
})

test_that("a rail declared empty survives, and drops a ghost on read", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    views = list(V = "a"),
    rails = list(V = rail(position = "bottom"))
  )

  expect_named(board_rails(brd)[["V"]], "bottom")
  expect_identical(rail_panel_ids(board_rails(brd)[["V"]]), character())

  # A rail naming a panel the view no longer carries reports it gone, exactly
  # as `view_grid()` prunes a grid ghost.
  ghosted <- list(bottom = new_dock_rail("bottom", "block_panel-gone"))

  expect_identical(
    rail_panel_ids(view_rails(board_views(brd)[["V"]], ghosted)),
    character()
  )
})

test_that("a board given only rails infers the view and its members", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    rails = list(V = rail(blk("a")))
  )

  expect_named(board_views(brd), "V")
  expect_identical(view_members(board_views(brd)[["V"]]), "block_panel-a")

  # The sole member is railed, so the placement grid it falls back to is empty.
  expect_identical(layout_panel_ids(active_view_grid(brd)), character())
})

test_that("rails reach dockView as edgeGroups, visibility derived", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    extensions = new_edit_board_extension(),
    views = list(V = c("a", "edit_board")),
    rails = list(V = rail(ext("edit_board"), size = 300, collapsed_size = 20))
  )

  lay <- as_dock_layout(
    active_view_grid(brd),
    board_blocks(brd),
    dock_extensions(brd),
    active_view_rails(brd)
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
    "ext_panel-edit_board" %in% layout_panel_ids(as_dock_grid(lay))
  )

  # An empty rail is declared all the same, and hidden.
  empty <- as_dock_layout(
    dock_grid("block_panel-a"),
    board_blocks(brd),
    dock_extensions(brd),
    list(left = new_dock_rail("left"))
  )

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
    grids = list(V = dock_grid("a")),
    rails = list(V = rail(blk("b"), position = "right", collapsed = TRUE))
  )

  expect_true(board_rails(brd)[["V"]][["right"]][["collapsed"]])
  expect_identical(board_rails(blockr_deser(blockr_ser(brd))), board_rails(brd))

  # It reaches dockView, and comes back off the echo.
  lay <- as_dock_layout(
    active_view_grid(brd), board_blocks(brd), dock_extensions(brd),
    active_view_rails(brd)
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

test_that("the rails slot rides the views delta", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    views = list(V = c("a", "b"), W = "a"),
    grids = list(V = dock_grid("a", "b"))
  )

  # The mirror's write, as the settled echo of a panel dragged onto the right
  # edge produces it: the grid loses the panel and the rail gains it, in one
  # commit, keyed by view id.
  moved <- apply_board_update(
    brd,
    list(
      views = list(
        grid = list(V = dock_grid("block_panel-a")),
        rails = list(V = list(right = new_dock_rail("right", "block_panel-b")))
      )
    )
  )

  expect_identical(layout_panel_ids(board_grids(moved)[["V"]]), "block_panel-a")
  expect_identical(rail_panel_ids(board_rails(moved)[["V"]]), "block_panel-b")
  expect_setequal(
    view_members(board_views(moved)[["V"]]),
    c("block_panel-a", "block_panel-b")
  )

  cleared <- apply_board_update(
    moved, list(views = list(rails = list(V = NULL)))
  )

  expect_null(board_rails(cleared)[["V"]])

  # Removing the view takes its rails with it.
  gone <- apply_board_update(moved, list(views = list(rm = "V")))

  expect_length(board_rails(gone), 0L)
})

test_that("an unknown views slice key is still rejected", {

  brd <- new_dock_board(c(a = new_dataset_block()))

  expect_error(
    validate_views_delta(list(railz = list()), brd, list()),
    class = "dock_views_delta_invalid"
  )
})

test_that("rails validate against the board's views", {

  brd <- new_dock_board(c(a = new_dataset_block()))

  expect_error(
    validate_dock_rails(new_dock_rails(list(nope = list())), board_views(brd)),
    class = "dock_rails_unknown_view"
  )

  expect_error(
    validate_dock_rails(new_dock_rails(list(list()))),
    class = "dock_rails_ids_missing"
  )

  expect_error(
    validate_dock_rails(list()),
    class = "dock_rails_structure_invalid"
  )

  expect_null(validate_dock_rails(NULL))
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
