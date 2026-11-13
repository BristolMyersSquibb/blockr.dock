test_that("rail() writes a rail and checks its arguments", {

  rl <- rail(blk("a"), plg("b"), position = "right", active = plg("b"))

  expect_s3_class(rl, "dock_rail")
  expect_identical(rl$position, "right")
  expect_identical(rl$panels, list(blk("a"), plg("b")))
  expect_identical(rl$active, plg("b"))

  # A rail shows its first panel unless it is given another, and an empty
  # rail shows none.
  expect_identical(rail("a", "b")$active, blk("a"))
  expect_null(rail()$active)

  expect_false(rail("a")$collapsed)
  expect_identical(rail("a")$size, 260)
  expect_identical(rail("a")$collapsed_size, 35)
  expect_identical(rail("a", size = 300L)$size, 300)

  expect_error(
    rail("a", active = "b"),
    class = "dock_panels_active_invalid"
  )
  expect_error(
    rail("a", collapsed = "yes"),
    class = "dock_rail_collapsed_invalid"
  )
  expect_error(rail("a", position = "diagonal"))
  expect_error(rail("a", position = "top"))
  expect_error(rail("a", size = -1), class = "dock_rail_size_invalid")
  expect_error(rail("a", size = NA_real_), class = "dock_rail_size_invalid")
  expect_error(
    rail("a", collapsed_size = c(1, 2)),
    class = "dock_rail_size_invalid"
  )
})

test_that("a grid holds its rails by edge, left before right", {

  grid <- dock_grid(
    "a",
    rail("c", position = "right"),
    rail("b", collapsed = TRUE)
  )

  expect_named(grid$rails, c("left", "right"))
  expect_named(
    grid$rails$left,
    c("panels", "active", "collapsed", "size", "collapsed_size")
  )
  expect_true(grid$rails$left$collapsed)

  # A rail takes no share of the top split.
  expect_identical(grid$sizes, 1)
})

test_that("a view has one rail at most on each edge", {

  expect_error(
    dock_grid(rail("a"), rail("b")),
    "two on the left edge",
    class = "dock_rails_position_clash"
  )

  expect_named(
    dock_grid(rail("a"), rail("b", position = "right"))$rails,
    c("left", "right")
  )
})

test_that("a grid drops a rail that holds no panel, whatever its widths", {

  expect_null(dock_grid("a", rail(position = "right", size = 300))$rails)
  expect_null(dock_grid("a", rail(collapsed = TRUE))$rails)
})

test_that("validate_dock_grid() checks the rails of a grid", {

  grid <- dock_grid("a", rail("b"))
  expect_identical(validate_dock_grid(grid), grid)

  fake <- grid
  names(fake$rails) <- "top"
  expect_error(validate_dock_grid(fake), class = "dock_rails_structure_invalid")

  fake <- grid
  fake$rails$left$collapsed <- "no"
  expect_error(validate_dock_grid(fake), class = "dock_rail_collapsed_invalid")

  fake <- grid
  fake$rails$left$size <- -1
  expect_error(validate_dock_grid(fake), class = "dock_rail_size_invalid")

  fake <- grid
  fake$rails$left$panels <- list()
  expect_error(validate_dock_grid(fake), class = "dock_grid_not_canonical")
})

test_that("a rail prints the ids of its panels", {

  expect_output(print(rail("a", plg("dag"))), "<dock_rail> left: a, dag")
  expect_identical(str_value(rail()), "<dock_rail> left: (empty)")
})
