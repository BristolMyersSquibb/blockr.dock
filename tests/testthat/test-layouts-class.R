test_that("multi-view boards via new_dock_board", {

  brd <- new_dock_board(
    blocks = c(
      a = new_dataset_block(),
      b = new_head_block(),
      c = new_head_block()
    ),
    views = list(
      view_a = dock_view(c("a", "b"), name = "Analysis"),
      view_b = dock_view("c", name = "Overview")
    )
  )

  views <- board_views(brd)

  expect_s3_class(views, "dock_views")
  expect_true(is_dock_views(views))
  expect_length(views, 2L)
  # the list key is the (container's) id; the name lives on the object
  expect_identical(names(views), c("view_a", "view_b"))
  expect_identical(unname(view_names(views)), c("Analysis", "Overview"))
  expect_identical(active_name(views), "Analysis")
})

test_that("multi-view addresses a keyed extension by its list key", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    extensions = list(edit = new_edit_board_extension()),
    views = list(
      Edit = c("edit", "a"),
      Data = "b"
    )
  )

  views <- board_views(brd)

  expect_setequal(
    view_members(views[["Edit"]]),
    c("block_panel-a", "ext_panel-edit")
  )
  expect_setequal(view_members(views[["Data"]]), "block_panel-b")
})

test_that("new_dock_board(active=) selects the active view by id", {

  brd <- new_dock_board(
    blocks = c(x = new_dataset_block(), y = new_head_block()),
    views = list(A = "x", B = "y"),
    active = "B"
  )

  expect_identical(active_name(brd), "B")
})

test_that("auto-defaults first view active when none chosen", {

  brd <- new_dock_board(
    blocks = c(x = new_dataset_block(), y = new_head_block()),
    views = list(A = "x", B = "y")
  )

  expect_identical(active_name(brd), "A")
})

test_that("new_dock_board rejects an unknown active id", {

  expect_error(
    new_dock_board(
      blocks = c(x = new_dataset_block(), y = new_head_block()),
      views = list(A = "x", B = "y"),
      active = "nope"
    ),
    class = "dock_view_not_found"
  )
})

test_that("empty views arg yields a single auto-generated view", {

  brd <- new_dock_board()
  views <- board_views(brd)

  expect_length(views, 1L)
  expect_true(nzchar(names(views)))
  expect_identical(active_view(views), names(views))
})

test_that("a bare list of views yields one view per entry", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    views = list("a", "b")
  )

  views <- board_views(brd)
  expect_length(views, 2L)
  expect_true(all(nzchar(names(views))))
  expect_identical(view_members(views[[1L]]), "block_panel-a")
})

test_that("active_view get/set on a dock_board", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    views = list(First = "a", Second = "b")
  )

  expect_identical(active_name(brd), "First")

  active_view(brd) <- vid(brd, "Second")
  expect_identical(active_name(brd), "Second")
  expect_identical(active_view(board_views(brd)), vid(brd, "Second"))

  expect_error(
    {
      active_view(brd) <- "Nope"
    },
    class = "dock_view_not_found"
  )
})

test_that("active_view returns NULL when the active view is gone", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    views = list(First = "a", Second = "b")
  )

  views <- board_views(brd)

  # First is the (defaulted) active view; dropping it directly leaves the
  # container's active id dangling, which reads back as "no active view".
  views[[vid(views, "First")]] <- NULL

  expect_null(active_view(views))
})

test_that("rejects invalid view ids at the new_dock_board boundary", {

  # duplicate ids (list keys)
  expect_error(
    new_dock_board(
      blocks = c(a = new_dataset_block(), b = new_head_block()),
      views = list(A = "a", A = "b")
    ),
    class = "dock_views_ids_duplicated"
  )

  # an id (list key) with characters that aren't DOM / namespace safe
  expect_error(
    new_dock_board(
      blocks = c(a = new_dataset_block()),
      views = setNames(list("a"), "bad id")
    ),
    class = "dock_view_id_invalid"
  )
})

test_that("coerce_dock_views rejects an unsupported entry type", {

  expect_error(
    new_dock_board(
      blocks = c(a = new_dataset_block()),
      views = list(A = 1L)
    ),
    class = "dock_views_element_invalid"
  )
})

test_that("a keyless view gets a minted id", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    views = list(pinned = "a", "b")
  )

  ids <- names(board_views(brd))
  expect_true("pinned" %in% ids)
  expect_length(unique(ids), 2L)
})

test_that("ids with . and - separators (ids-package styles) are accepted", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    views = list(
      `verdant-aardvark` = "a",
      `swift.otter` = "b"
    )
  )

  views <- board_views(brd)
  expect_setequal(names(views), c("verdant-aardvark", "swift.otter"))
  expect_setequal(
    unname(view_names(views)),
    c("Verdant aardvark", "Swift otter")
  )
})

test_that("multi-view boards produce typed slots", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    views = list(
      Tab1 = dock_view(c("a", "b")),
      Tab2 = dock_view("a")
    ),
    grids = list(Tab1 = dock_grid("a", "b"), Tab2 = dock_grid("a"))
  )

  expect_s3_class(board_views(brd), "dock_views")
  expect_s3_class(board_grids(brd), "dock_grids")
  expect_identical(unname(view_names(board_views(brd))), c("Tab1", "Tab2"))
  expect_identical(active_name(board_views(brd)), "Tab1")

  expect_true(is_dock_grid(board_grids(brd)[["Tab1"]]))
  expect_true(is_dock_grid(board_grids(brd)[["Tab2"]]))
})

test_that("the active view survives construction", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    views = list(First = "a", Second = c("a", "b")),
    active = "Second"
  )

  views <- board_views(brd)
  expect_identical(active_name(views), "Second")
  expect_identical(
    view_members(views[["Second"]]),
    c("block_panel-a", "block_panel-b")
  )
})

test_that("default board has a single auto-generated, active view", {

  brd <- new_dock_board(c(a = new_dataset_block()))
  views <- board_views(brd)

  expect_s3_class(views, "dock_views")
  expect_length(views, 1L)
  expect_identical(active_view(views), names(views))
})

test_that("active_view_grid returns the active view's placement grid", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    views = list(First = "a", Second = c("a", "b"))
  )

  grid <- active_view_grid(brd)
  expect_true(is_dock_grid(grid))
  expect_length(layout_panel_ids(grid), 1L)
})

test_that("validate_dock_views rejects an active id that isn't present", {

  views <- board_views(
    new_dock_board(
      blocks = c(a = new_dataset_block(), b = new_head_block()),
      views = list(A = "a", B = "b")
    )
  )

  attr(views, "active") <- "ghost"

  expect_error(
    validate_dock_views(views),
    class = "dock_view_not_found"
  )
})

test_that("view_name<- rewrites only the display name, not the id", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    views = list(Original = "a")
  )

  views <- board_views(brd)
  id <- vid(views, "Original")

  view_name(views[[id]]) <- "Renamed"

  expect_identical(names(views), id)
  expect_identical(view_name(views[[id]]), "Renamed")
})

test_that("the list key is the view id, the name lives on the view", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    views = list(
      view_one = dock_view("a", name = "Analysis"),
      view_two = dock_view("b", name = "Overview")
    )
  )

  views <- board_views(brd)
  expect_identical(names(views), c("view_one", "view_two"))
  expect_identical(unname(view_names(views)), c("Analysis", "Overview"))
})

test_that("an unnamed view derives its display label from the id", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    views = list(my_view = "a")
  )

  views <- board_views(brd)
  expect_null(view_name(views[["my_view"]]))
  expect_identical(unname(view_names(views)), "My view")
})

test_that("view ids survive serialization", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    views = list(
      view_one = dock_view("a", name = "Analysis"),
      view_two = dock_view("b", name = "Overview")
    )
  )

  des <- board_views(blockr_deser(blockr_ser(brd)))
  expect_identical(names(des), c("view_one", "view_two"))
  expect_identical(unname(view_names(des)), c("Analysis", "Overview"))
})

test_that("str_value.dock_views renders one line per view, marking active", {

  brd <- new_dock_board(
    blocks = c(d = new_dataset_block(), h = new_head_block()),
    views = list(
      v1 = dock_view(c("d", "h")),
      v2 = dock_view("d")
    ),
    active = "v2"
  )

  views <- board_views(brd)

  expect_identical(
    str_value(views),
    paste(
      "<dock_views[2]>",
      "  v1: <dock_view> d, h",
      "  v2: <dock_view> d (active)",
      sep = "\n"
    )
  )

  expect_output(
    str(views),
    "v2: <dock_view> d (active)",
    fixed = TRUE
  )
})
