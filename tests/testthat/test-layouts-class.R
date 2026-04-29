test_that("dock_layouts constructor", {
  ly <- dock_layouts(
    Analysis = list("a", "b"),
    Overview = list("c")
  )

  expect_s3_class(ly, "dock_layouts")
  expect_true(is_dock_layouts(ly))
  expect_length(ly, 2L)
  expect_named(ly, c("Analysis", "Overview"))
  expect_identical(active_view(ly), "Analysis")
})

test_that("dock_grid marks an arrangement active via attribute", {
  v <- dock_grid("a", "b", active = TRUE)
  expect_true(is_dock_grid(v))
  expect_identical(unlist(v), c("a", "b"))
  expect_true(isTRUE(attr(v, "active")))

  expect_null(attr(dock_grid("a"), "active"))
  expect_null(attr(dock_grid("a", active = FALSE), "active"))
})

test_that("dock_layouts uses per-view active attribute", {
  ly <- dock_layouts(
    A = list("x"),
    B = dock_grid("y", active = TRUE)
  )

  expect_identical(active_view(ly), "B")
})

test_that("dock_layouts auto-defaults first view active", {
  ly <- dock_layouts(A = list("x"), B = list("y"))
  expect_identical(active_view(ly), "A")
})

test_that("validate_dock_layouts rejects multiple active views", {
  expect_error(
    dock_layouts(
      A = dock_grid("x", active = TRUE),
      B = dock_grid("y", active = TRUE)
    ),
    class = "dock_layouts_multiple_active"
  )
})

test_that("dock_layouts default to one empty page", {
  ly <- dock_layouts()
  expect_length(ly, 1L)
  expect_named(ly, "Page")
})

test_that("dock_layouts from single list argument", {
  ly <- dock_layouts(list(
    Tab1 = list("a"),
    Tab2 = list("b", "c")
  ))

  expect_s3_class(ly, "dock_layouts")
  expect_named(ly, c("Tab1", "Tab2"))
})

test_that("active_view get/set", {
  ly <- dock_layouts(A = list("x"), B = list("y"))

  expect_identical(active_view(ly), "A")

  active_view(ly) <- "B"
  expect_identical(active_view(ly), "B")

  expect_error(
    { active_view(ly) <- "Z" },
    class = "dock_view_not_found"
  )
})

test_that("active_view get/set dispatches on dock_board", {
  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = dock_layouts(
      First = list("a"),
      Second = list("b")
    )
  )

  expect_identical(active_view(brd), "First")

  active_view(brd) <- "Second"
  expect_identical(active_view(brd), "Second")
  expect_identical(active_view(board_layouts(brd)), "Second")

  expect_error(
    { active_view(brd) <- "Nope" },
    class = "dock_view_not_found"
  )
})

test_that("validate_dock_layouts rejects invalid input", {
  expect_error(
    validate_dock_layouts(list()),
    class = "dock_layouts_structure_invalid"
  )

  expect_error(
    dock_layouts(list("a", "b")),
    class = "dock_layouts_names_missing"
  )

  expect_error(
    dock_layouts(A = list("x"), A = list("y")),
    class = "dock_layouts_names_duplicated"
  )
})

test_that("view_ids extracts IDs from layout spec", {
  expect_identical(
    sort(view_ids(list("a", list("b", "c")))),
    c("a", "b", "c")
  )

  expect_null(view_ids(list()))
})

test_that("as_dock_layouts coerces named list", {
  raw <- list(A = list("x"), B = list("y"))
  ly <- as_dock_layouts(raw)

  expect_s3_class(ly, "dock_layouts")
  expect_named(ly, c("A", "B"))
})

test_that("as_dock_layouts identity on dock_layouts", {
  ly <- dock_layouts(A = list("x"))
  expect_identical(as_dock_layouts(ly), ly)
})

test_that("multi-view requires dock_layouts() (no auto-promotion)", {
  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = dock_layouts(
      Tab1 = list("a", "b"),
      Tab2 = list("a")
    )
  )

  views <- board_layouts(brd)
  expect_s3_class(views, "dock_layouts")
  expect_named(views, c("Tab1", "Tab2"))
  expect_identical(active_view(views), "Tab1")

  # Each view should be resolved to dock_layout
  expect_true(is_dock_layout(views[["Tab1"]]))
  expect_true(is_dock_layout(views[["Tab2"]]))
})

test_that("dock_layouts() accepted by new_dock_board", {
  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    layouts = dock_layouts(V1 = list("a"))
  )

  expect_s3_class(board_layouts(brd), "dock_layouts")
  expect_true(is_dock_layout(board_layouts(brd)[["V1"]]))
})

test_that("active attribute survives layout resolution", {
  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = dock_layouts(
      First = list("a"),
      Second = dock_grid("a", "b", active = TRUE)
    )
  )

  views <- board_layouts(brd)
  expect_identical(active_view(views), "Second")
  expect_true(is_dock_layout(views[["Second"]]))
})

test_that("default board has a single auto-named Page view", {
  brd <- new_dock_board(c(a = new_dataset_block()))
  views <- board_layouts(brd)
  expect_s3_class(views, "dock_layouts")
  expect_named(views, "Page")
  expect_identical(active_view(views), "Page")
  # Default Page is auto-populated from blocks + extensions
  expect_true(is_dock_layout(views[["Page"]]))
})

test_that("raw grid layout is wrapped in a single-page dock_layouts", {
  brd <- new_dock_board(
    c(a = new_dataset_block()),
    layouts = list("a")
  )
  views <- board_layouts(brd)
  expect_s3_class(views, "dock_layouts")
  expect_named(views, "Page")
  expect_identical(active_view(views), "Page")
})

test_that("dock_layout getter returns active view layout", {
  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = dock_layouts(
      First = list("a"),
      Second = list("a", "b")
    )
  )

  ly <- active_layout(brd)
  expect_true(is_dock_layout(ly))
  # Active view is "First" which has 1 panel (block a)
  expect_length(ly$panels, 1L)
})

test_that("as_dock_layouts.dock_layout wraps in single Page", {
  ly <- new_dock_layout()
  views <- as_dock_layouts(ly)
  expect_s3_class(views, "dock_layouts")
  expect_named(views, "Page")
  expect_identical(active_view(views), "Page")
  expect_true(is_dock_layout(views[["Page"]]))
})
