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

test_that("a grid carries at most one rail per edge", {

  expect_named(
    view_rail_set(list(rail(blk("a")), rail(blk("b"), position = "right"))),
    c("left", "right")
  )

  expect_error(
    view_rail_set(list(rail(blk("a")), rail(blk("b")))),
    class = "dock_rails_position_clash"
  )
})

test_that("a rail claims a panel that the grid's tree also names", {

  # Both say where the panel goes, so the same panel in both is ambiguous. The
  # rail wins and the tree loses it, keeping the partition total.
  grid <- dock_grid("block_panel-a", "block_panel-b", rail(blk("b")))

  expect_identical(grid_tree_ids(grid), "block_panel-a")
  expect_identical(rail_panel_ids(grid[["rails"]]), "block_panel-b")
})

test_that("an empty rail at its defaults is not stored", {

  # Every dock offers every edge, so an empty rail at its defaults says
  # nothing.
  expect_null(dock_grid("a", rail(position = "right"))[["rails"]])

  # One that sets its geometry is kept, as is one holding panels.
  expect_named(
    dock_grid("a", rail(position = "right", size = 300))[["rails"]],
    "right"
  )
  expect_true(
    dock_grid("a", rail(blk("b"), collapsed = TRUE))[["rails"]][["left"]][[
      "collapsed"
    ]]
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
