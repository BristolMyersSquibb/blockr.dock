test_that("multi-view layouts via new_dock_board", {

  brd <- new_dock_board(
    blocks = c(
      a = new_dataset_block(),
      b = new_head_block(),
      c = new_head_block()
    ),
    layouts = list(
      view_a = dock_layout("a", "b", name = "Analysis"),
      view_b = dock_layout("c", name = "Overview")
    )
  )

  views <- board_layouts(brd)

  expect_s3_class(views, "dock_layouts")
  expect_true(is_dock_layouts(views))
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
    layouts = list(
      Edit = dock_layout("edit", "a"),
      Data = dock_layout("b")
    )
  )

  views <- board_layouts(brd)

  expect_setequal(
    layout_panel_ids(views[["Edit"]]),
    c("block_panel-a", "ext_panel-edit_board_extension")
  )
  expect_setequal(layout_panel_ids(views[["Data"]]), "block_panel-b")
})

test_that("new_dock_board(active=) selects the active view by id", {

  brd <- new_dock_board(
    blocks = c(x = new_dataset_block(), y = new_head_block()),
    layouts = list(A = list("x"), B = list("y")),
    active = "B"
  )

  expect_identical(active_name(brd), "B")
})

test_that("auto-defaults first view active when none chosen", {

  brd <- new_dock_board(
    blocks = c(x = new_dataset_block(), y = new_head_block()),
    layouts = list(A = list("x"), B = list("y"))
  )

  expect_identical(active_name(brd), "A")
})

test_that("new_dock_board rejects an unknown active id", {

  expect_error(
    new_dock_board(
      blocks = c(x = new_dataset_block(), y = new_head_block()),
      layouts = list(A = list("x"), B = list("y")),
      active = "nope"
    ),
    class = "dock_view_not_found"
  )
})

test_that("empty layouts arg yields a single auto-generated view", {

  brd <- new_dock_board()
  views <- board_layouts(brd)

  expect_length(views, 1L)
  expect_true(nzchar(names(views)))
  expect_identical(active_view(views), names(views))
})

test_that("dock_layouts() with no views yields one auto-generated view", {

  views <- dock_layouts()

  expect_s3_class(views, "dock_layouts")
  expect_length(views, 1L)
  expect_true(nzchar(names(views)))
})

test_that("dock_layouts() unwraps a single list of views", {

  views <- dock_layouts(list(one = dock_layout("a"), two = dock_layout("b")))

  expect_identical(names(views), c("one", "two"))
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

test_that("active_view returns NULL when the active view is gone", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(First = list("a"), Second = list("b"))
  )

  views <- board_layouts(brd)

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
      layouts = list(A = list("a"), A = list("b"))
    ),
    class = "dock_layouts_ids_duplicated"
  )

  # an id (list key) with characters that aren't DOM / namespace safe
  expect_error(
    new_dock_board(
      blocks = c(a = new_dataset_block()),
      layouts = setNames(list(list("a")), "bad id")
    ),
    class = "dock_view_id_invalid"
  )
})

test_that("a keyless view gets a minted id", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(pinned = list("a"), dock_layout("b"))
  )

  ids <- names(board_layouts(brd))
  expect_true("pinned" %in% ids)
  expect_length(unique(ids), 2L)
})

test_that("ids with . and - separators (ids-package styles) are accepted", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(
      `verdant-aardvark` = dock_layout("a"),
      `swift.otter` = dock_layout("b")
    )
  )

  views <- board_layouts(brd)
  expect_setequal(names(views), c("verdant-aardvark", "swift.otter"))
  expect_setequal(
    unname(view_names(views)),
    c("Verdant aardvark", "Swift otter")
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

test_that("the active view survives layout resolution", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(
      First = list("a"),
      Second = dock_layout("a", "b")
    ),
    active = "Second"
  )

  views <- board_layouts(brd)
  expect_identical(active_name(views), "Second")
  expect_true(is_dock_layout(views[[vid(views, "Second")]]))
})

test_that("default board has a single auto-generated, active view", {

  brd <- new_dock_board(c(a = new_dataset_block()))
  views <- board_layouts(brd)

  expect_s3_class(views, "dock_layouts")
  expect_length(views, 1L)
  expect_identical(active_view(views), names(views))
  expect_true(is_dock_layout(views[[names(views)]]))
})

test_that("raw grid layout is wrapped in a single-page dock_layouts", {

  brd <- new_dock_board(
    c(a = new_dataset_block()),
    layouts = list("a")
  )

  views <- board_layouts(brd)
  expect_s3_class(views, "dock_layouts")
  expect_length(views, 1L)
  expect_identical(active_view(views), names(views))
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

test_that("as_dock_layouts.dock_layout wraps in a single active view", {

  ly <- new_dock_layout()
  views <- as_dock_layouts(ly)

  expect_s3_class(views, "dock_layouts")
  expect_length(views, 1L)
  expect_identical(active_view(views), names(views))
  expect_true(is_dock_layout(views[[names(views)]]))
})

test_that("as_dock_layouts identity on dock_layouts", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    layouts = list(V1 = list("a"))
  )
  ly <- board_layouts(brd)

  expect_identical(as_dock_layouts(ly), ly)
})

test_that("as_dock_layouts errors on an unsupported input type", {

  expect_error(
    as_dock_layouts(1L),
    class = "dock_layouts_coerce_invalid"
  )
})

test_that("validate_dock_layouts rejects an active id that isn't present", {

  views <- board_layouts(
    new_dock_board(
      blocks = c(a = new_dataset_block(), b = new_head_block()),
      layouts = list(A = list("a"), B = list("b"))
    )
  )

  attr(views, "active") <- "ghost"

  expect_error(
    validate_dock_layouts(views),
    class = "dock_view_not_found"
  )
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

test_that("the list key is the view id, the name lives on the layout", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(
      view_one = dock_layout("a", name = "Analysis"),
      view_two = dock_layout("b", name = "Overview")
    )
  )

  views <- board_layouts(brd)
  expect_identical(names(views), c("view_one", "view_two"))
  expect_identical(unname(view_names(views)), c("Analysis", "Overview"))
})

test_that("an unnamed view derives its display label from the id", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block()),
    layouts = list(my_view = list("a"))
  )

  views <- board_layouts(brd)
  expect_null(view_name(views[["my_view"]]))
  expect_identical(unname(view_names(views)), "My view")
})

test_that("view ids survive serialization", {

  brd <- new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    layouts = list(
      view_one = dock_layout("a", name = "Analysis"),
      view_two = dock_layout("b", name = "Overview")
    )
  )

  des <- blockr_deser(blockr_ser(brd))[["layouts"]]
  expect_identical(names(des), c("view_one", "view_two"))
  expect_identical(unname(view_names(des)), c("Analysis", "Overview"))
})
