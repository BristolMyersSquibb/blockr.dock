test_that("multi-view layouts via new_dock_board", {

  brd <- new_dock_board(
    blocks = c(
      a = new_dataset_block(),
      b = new_head_block(),
      c = new_head_block()
    ),
    layouts = list(
      Analysis = list("a", "b"),
      Overview = list("c")
    )
  )

  views <- board_layouts(brd)

  expect_s3_class(views, "dock_layouts")
  expect_true(is_dock_layouts(views))
  expect_length(views, 2L)
  expect_true(all(grepl("^dock_", names(views))))
  expect_identical(unname(view_names(views)), c("Analysis", "Overview"))
  expect_identical(active_name(views), "Analysis")
})

test_that("dock_layout marker selects active view", {

  brd <- new_dock_board(
    blocks = c(x = new_dataset_block(), y = new_head_block()),
    layouts = list(
      A = list("x"),
      B = dock_layout("y", active = TRUE)
    )
  )

  expect_identical(active_name(brd), "B")
})

test_that("auto-defaults first view active when none marked", {

  brd <- new_dock_board(
    blocks = c(x = new_dataset_block(), y = new_head_block()),
    layouts = list(A = list("x"), B = list("y"))
  )

  expect_identical(active_name(brd), "A")
})

test_that("rejects multiple active views", {

  expect_error(
    new_dock_board(
      blocks = c(x = new_dataset_block(), y = new_head_block()),
      layouts = list(
        A = dock_layout("x", active = TRUE),
        B = dock_layout("y", active = TRUE)
      )
    ),
    class = "dock_layouts_multiple_active"
  )
})

test_that("empty layouts arg yields a single Page view", {

  brd <- new_dock_board()
  views <- board_layouts(brd)

  expect_length(views, 1L)
  expect_identical(unname(view_names(views)), "Page")
})

test_that("active_view get/set on a dock_board", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(
      First = list("a"),
      Second = list("b")
    )
  )

  expect_identical(active_name(brd), "First")

  active_view(brd) <- vid(brd, "Second")
  expect_identical(active_name(brd), "Second")
  expect_identical(active_name(board_layouts(brd)), "Second")

  expect_error(
    {
      active_view(brd) <- "Nope"
    },
    class = "dock_view_not_found"
  )
})

test_that("active_view returns NULL when no view is active", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(First = list("a"), Second = list("b"))
  )

  views <- board_layouts(brd)
  views[[vid(views, "First")]] <- NULL

  expect_false(any_active_view(views))
  expect_null(active_view(views))
})

test_that("rejects invalid layout shapes at the new_dock_board boundary", {

  expect_error(
    new_dock_board(
      blocks = c(a = new_dataset_block(), b = new_head_block()),
      layouts = list(A = list("a"), list("b"))
    ),
    class = "dock_layouts_names_missing"
  )

  expect_error(
    new_dock_board(
      blocks = c(a = new_dataset_block(), b = new_head_block()),
      layouts = list(A = list("a"), A = list("b"))
    ),
    class = "dock_layouts_names_duplicated"
  )
})

test_that("layout_ids extracts IDs from layout spec", {

  expect_identical(
    sort(layout_ids(list("a", list("b", "c")))),
    c("a", "b", "c")
  )

  expect_null(layout_ids(list()))
})

test_that("multi-view layouts produce typed slots", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(
      Tab1 = list("a", "b"),
      Tab2 = list("a")
    )
  )

  views <- board_layouts(brd)
  expect_s3_class(views, "dock_layouts")
  expect_identical(unname(view_names(views)), c("Tab1", "Tab2"))
  expect_identical(active_name(views), "Tab1")

  expect_true(is_dock_layout(views[[vid(views, "Tab1")]]))
  expect_true(is_dock_layout(views[[vid(views, "Tab2")]]))
})

test_that("active attribute survives layout resolution", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(
      First = list("a"),
      Second = dock_layout("a", "b", active = TRUE)
    )
  )

  views <- board_layouts(brd)
  expect_identical(active_name(views), "Second")
  expect_true(is_dock_layout(views[[vid(views, "Second")]]))
})

test_that("default board has a single auto-named Page view", {

  brd <- new_dock_board(c(a = new_dataset_block()))
  views <- board_layouts(brd)

  expect_s3_class(views, "dock_layouts")
  expect_identical(unname(view_names(views)), "Page")
  expect_identical(active_name(views), "Page")
  expect_true(is_dock_layout(views[[vid(views, "Page")]]))
})

test_that("raw grid layout is wrapped in a single-page dock_layouts", {

  brd <- new_dock_board(
    c(a = new_dataset_block()),
    layouts = list("a")
  )

  views <- board_layouts(brd)
  expect_s3_class(views, "dock_layouts")
  expect_identical(unname(view_names(views)), "Page")
  expect_identical(active_name(views), "Page")
})

test_that("active_layout returns the active view's resolved layout", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(
      First = list("a"),
      Second = list("a", "b")
    )
  )

  ly <- active_layout(brd)
  expect_true(is_dock_layout(ly))
  expect_length(layout_panel_ids(ly), 1L)
})

test_that("as_dock_layouts.dock_layout wraps in single Page", {

  ly <- new_dock_layout()
  views <- as_dock_layouts(ly)

  expect_s3_class(views, "dock_layouts")
  expect_identical(unname(view_names(views)), "Page")
  expect_identical(active_name(views), "Page")
  expect_true(is_dock_layout(views[[vid(views, "Page")]]))
})

test_that("as_dock_layouts identity on dock_layouts", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    layouts = list(V1 = list("a"))
  )
  ly <- board_layouts(brd)

  expect_identical(as_dock_layouts(ly), ly)
})

test_that("view_name<- rewrites only the display name, not the id", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    layouts = list(Original = list("a"))
  )

  views <- board_layouts(brd)
  id <- vid(views, "Original")

  view_name(views[[id]]) <- "Renamed"

  expect_identical(names(views), id)
  expect_identical(view_name(views[[id]]), "Renamed")
})
