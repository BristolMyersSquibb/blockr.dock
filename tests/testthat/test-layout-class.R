test_that("an invalid grid node reports a legible error", {
  expect_error(
    dock_grid("a", active = TRUE),
    "Unknown layout node type",
    class = "dock_layout_node_invalid"
  )
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

test_that("a grid places each panel once", {

  # Twice in the tree. A grid says where each panel goes, so two spots for one
  # panel express nothing, and neither spot wins on principle -- hence a
  # rejection rather than a quiet first-wins dedup.
  expect_error(
    validate_dock_grid(dock_grid("block_panel-a", "block_panel-a")),
    class = "dock_grid_panel_duplicated"
  )

  # The rails place panels too, so the invariant spans them: twice inside one
  # rail, or once in each of two.
  expect_error(
    validate_dock_grid(dock_grid(rail(blk("a"), blk("a")))),
    class = "dock_grid_panel_duplicated"
  )
  expect_error(
    validate_dock_grid(
      dock_grid(rail(blk("a")), rail(blk("a"), position = "right"))
    ),
    class = "dock_grid_panel_duplicated"
  )

  # Every repeated panel is named, not just the first one found.
  expect_error(
    validate_dock_grid(
      dock_grid(
        "block_panel-a", "block_panel-a", "block_panel-b", "block_panel-b"
      )
    ),
    "block_panel-a and block_panel-b"
  )

  # The one overlap that does have a winner stays a prune, not a rejection: a
  # rail claims its panel out of the tree in `canonicalize_grid()`, so what
  # reaches the check already places it once.
  ov <- dock_grid("block_panel-a", "block_panel-b", rail(blk("a")))
  expect_identical(validate_dock_grid(ov), ov)
  expect_setequal(grid_panel_ids(ov), c("block_panel-a", "block_panel-b"))
})

test_that("a grid casts to a list and back", {

  grid <- dock_grid(
    panels("block_panel-a", "block_panel-b", active = "block_panel-b"),
    "block_panel-c",
    rail("block_panel-d", collapsed = TRUE),
    sizes = c(0.4, 0.6)
  )

  lst <- as.list(grid)

  expect_false(is_dock_grid(lst))
  expect_false(is_dock_rail(lst[["rails"]][["left"]]))
  expect_identical(as_dock_grid(lst), grid)
  expect_identical(as_dock_grid(grid), grid)
})

test_that("sizes propagate to grid children and nested branches", {

  grid <- dock_grid("a", "b", sizes = c(0.3, 0.7))
  expect_equal(grid$sizes, c(0.3, 0.7))

  grid2 <- dock_grid(
    "a",
    group("b", "c", sizes = c(0.4, 0.6)),
    sizes = c(0.3, 0.7)
  )
  expect_equal(grid2$sizes, c(0.3, 0.7))
  expect_equal(grid2$children[[2L]]$sizes, c(0.4, 0.6))
})

test_that("panels() sets the active tab in a tabbed leaf", {

  grid <- dock_grid(panels("a", "b", "c", active = "b"))
  leaf <- grid$children[[1L]]
  expect_identical(leaf$active, "b")
  expect_identical(leaf$panels, c("a", "b", "c"))
})

test_that("panels() with single id is allowed but redundant", {

  expect_silent(panels("a"))
  grid <- dock_grid(panels("a"))
  expect_identical(grid_panel_ids(grid), "a")
})

test_that("orientation flips the top-level grid", {

  grid <- dock_grid("a", "b", orientation = "vertical")
  expect_identical(grid$orientation, "vertical")
})

test_that("dock_grid rejects invalid sizes vector", {

  expect_error(
    dock_grid("a", "b", sizes = c(0.5)),
    class = "dock_layout_sizes_invalid"
  )
  expect_error(
    dock_grid("a", "b", sizes = c(-0.5, 1.5)),
    class = "dock_layout_sizes_invalid"
  )
})

test_that("panels() rejects active that isn't one of the IDs", {

  expect_error(
    panels("a", "b", active = "c"),
    class = "dock_panels_active_invalid"
  )
})

test_that("dock_grid has a tree-style print method", {

  expect_snapshot(print(dock_grid("a", "b")))

  expect_snapshot(
    print(
      dock_grid(
        group("data", "filt", "head"), "assistant_extension",
        sizes = c(0.6, 0.4)
      )
    )
  )

  expect_snapshot(
    print(dock_grid("a", panels("b", "c", "edit", active = "c")))
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

  expect_snapshot(print(dock_grid()))
})
test_that("dock_grid format strips panel-id prefixes unless bare = FALSE", {

  grid <- dock_grid(blk("a"), rail(blk("b")))

  bare <- format(grid)
  full <- format(grid, bare = FALSE)

  expect_false(any(grepl("block_panel-", bare)))
  expect_true(any(grepl("block_panel-a", full, fixed = TRUE)))
})

test_that("dock_grid format marks the focused panel", {

  grid <- dock_grid("a", panels("b", "c", active = "c"))
  grid[["focus"]] <- "c"

  out <- format(grid)

  expect_true(any(grepl("focus", out, fixed = TRUE)))
})

test_that("empty dock_grid formats as a single line", {

  out <- format(dock_grid())

  expect_length(out, 1L)
  expect_match(out, "empty", fixed = TRUE)
})

test_that("print.dock_grid returns its input invisibly", {

  grid <- dock_grid("a", "b")

  expect_identical(expect_invisible(print(grid)), grid)
})

test_that("str_value.dock_grid lists the panel object ids", {

  grid <- dock_grid("block_panel-b", "block_panel-a")

  expect_identical(str_value(grid), "<dock_grid> b, a")

  expect_output(
    str(grid),
    "<dock_grid> b, a",
    fixed = TRUE
  )
})

test_that("str_value.dock_grid marks an empty grid", {

  expect_identical(str_value(dock_grid()), "<dock_grid> (empty)")
})
