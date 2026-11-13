test_that("an invalid grid node reports a legible error", {

  expect_error(
    dock_grid("a", active = TRUE),
    "not an object of class logical",
    class = "dock_layout_node_invalid"
  )

  # A rail sits on an edge of the view, so only the top split takes one.
  expect_error(dock_grid(group(rail("a"))), class = "dock_layout_node_invalid")
  expect_error(
    dock_grid(list("a", rail("b"))),
    class = "dock_layout_node_invalid"
  )
})

test_that("a grid scales the shares of each split to ratios that sum to 1", {

  grid <- dock_grid(
    "a",
    group("b", "c", sizes = c(1, 3)),
    sizes = c(0.3, 0.7)
  )

  expect_identical(grid$sizes, c(0.3, 0.7))
  expect_identical(grid$children[[2L]]$sizes, c(0.25, 0.75))

  expect_identical(dock_grid("a", "b")$sizes, c(0.5, 0.5))
  expect_identical(dock_grid(list("a", "b"))$children[[1L]]$sizes, c(0.5, 0.5))
  expect_identical(dock_grid()$sizes, numeric())
})

test_that("a grid drops what holds no panel, and its share", {

  grid <- dock_grid(
    panels(),
    "a",
    group(character(), list()),
    "b",
    sizes = c(1, 1, 1, 2)
  )

  expect_length(grid$children, 2L)
  expect_equal(grid$sizes, c(1 / 3, 2 / 3))

  expect_identical(dock_grid(panels(), group())$children, list())
})

test_that("a tab group shows its first panel unless it is given another", {

  grid <- dock_grid(panels("a", "b", "c", active = "b"), c("d", "e"))

  expect_identical(grid$children[[1L]]$active, new_panel_ref("b", "bare"))
  expect_identical(grid$children[[2L]]$active, new_panel_ref("d", "bare"))
  expect_length(grid$children[[2L]]$panels, 2L)

  expect_error(
    panels("a", "b", active = "c"),
    class = "dock_panels_active_invalid"
  )
})

test_that("orientation sets how the top split divides", {

  expect_identical(dock_grid("a", "b")$orientation, "horizontal")
  expect_identical(
    dock_grid("a", "b", orientation = "vertical")$orientation,
    "vertical"
  )
  expect_error(dock_grid("a", orientation = "diagonal"))
})

test_that("dock_grid() and group() refuse shares that are not positive", {

  expect_error(
    dock_grid("a", "b", sizes = 0.5),
    "2 children, while it gives 1",
    class = "dock_layout_sizes_invalid"
  )
  expect_error(
    dock_grid("a", "b", sizes = c(-0.5, 1.5)),
    "the share -0.5 is not",
    class = "dock_layout_sizes_invalid"
  )
  expect_error(
    group("a", "b", sizes = c(0, -1)),
    "the shares 0 and -1 are not",
    class = "dock_layout_sizes_invalid"
  )
  expect_error(
    dock_grid("a", sizes = NA_real_),
    class = "dock_layout_sizes_invalid"
  )
  expect_error(dock_grid("a", sizes = "1"), class = "dock_layout_sizes_invalid")
})

test_that("validate_dock_grid() refuses a grid that is not canonical", {

  grid <- dock_grid("a", "b", sizes = c(0.3, 0.7))
  expect_identical(validate_dock_grid(grid), grid)

  expect_error(
    validate_dock_grid(list()),
    class = "dock_grid_structure_invalid"
  )

  fake <- grid
  fake$sizes <- c(0.9, 0.7)
  expect_error(validate_dock_grid(fake), class = "dock_grid_not_canonical")

  fake <- grid
  fake$children[[1L]]$active <- blk("z")
  expect_error(validate_dock_grid(fake), class = "dock_grid_not_canonical")

  fake <- grid
  fake$children[[1L]] <- list(children = list(), sizes = numeric())
  expect_error(validate_dock_grid(fake), class = "dock_grid_not_canonical")

  fake <- grid
  fake$focus <- "a"
  expect_error(validate_dock_grid(fake), class = "dock_grid_structure_invalid")
})

test_that("dock_grid has a tree-style print method", {

  expect_snapshot(print(dock_grid("a", "b")))

  expect_snapshot(
    print(
      dock_grid(
        group("data", "filt", "head"), plg("assistant"),
        sizes = c(0.6, 0.4)
      )
    )
  )

  expect_snapshot(
    print(dock_grid("a", panels("b", "c", plg("edit"), active = "c")))
  )

  expect_snapshot(
    print(
      dock_grid(
        "x", "y", "z",
        orientation = "vertical", sizes = c(0.5, 0.3, 0.2)
      )
    )
  )

  expect_snapshot(print(dock_grid("top", group("a", group("b", "c")))))

  expect_snapshot(
    print(
      dock_grid(
        "a",
        rail(plg("dag"), "b", active = "b", collapsed = TRUE),
        rail("c", position = "right")
      )
    )
  )

  expect_snapshot(print(dock_grid()))
})

test_that("print.dock_grid returns its input invisibly", {

  grid <- dock_grid("a", "b")

  expect_output(
    expect_identical(expect_invisible(print(grid)), grid),
    "<dock_grid> horizontal",
    fixed = TRUE
  )
})

test_that("str_value.dock_grid lists the ids of the panels", {

  grid <- dock_grid("b", blk("a"), rail(plg("dag")))

  expect_identical(str_value(grid), "<dock_grid> b, a, dag")
  expect_output(str(grid), "<dock_grid> b, a, dag", fixed = TRUE)

  expect_identical(str_value(dock_grid()), "<dock_grid> (empty)")
})
