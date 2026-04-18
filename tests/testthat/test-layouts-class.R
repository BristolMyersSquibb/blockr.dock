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

test_that("dock_layouts .active parameter", {
  ly <- dock_layouts(
    A = list("x"),
    B = list("y"),
    .active = "B"
  )

  expect_identical(active_view(ly), "B")
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

test_that("plain named list auto-detected by new_dock_board", {
  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layout = list(
      Tab1 = list("a", "b"),
      Tab2 = list("a")
    )
  )

  views <- board_views(brd)
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
    layout = dock_layouts(V1 = list("a"))
  )

  expect_s3_class(board_views(brd), "dock_layouts")
  expect_true(is_dock_layout(board_views(brd)[["V1"]]))
})

test_that("board_views returns NULL for single-layout board", {
  brd <- new_dock_board(c(a = new_dataset_block()))
  expect_null(board_views(brd))
})

test_that("dock_layout getter returns active view layout", {
  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layout = list(
      First = list("a"),
      Second = list("a", "b")
    )
  )

  ly <- dock_layout(brd)
  expect_true(is_dock_layout(ly))
  # Active view is "First" which has 1 panel (block a)
  expect_length(ly$panels, 1L)
})
