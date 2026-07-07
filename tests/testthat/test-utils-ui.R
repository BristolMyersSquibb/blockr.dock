test_that("determine_active_views handles an uninitialised layout", {

  expect_identical(determine_active_views(NULL), character())

  leaf <- function(id, view) {
    list(type = "leaf", data = list(id = id, activeView = view))
  }

  layout <- list(
    grid = list(
      root = list(
        type = "branch",
        data = list(leaf("grp1", "view_a"), leaf("grp2", "view_b"))
      )
    )
  )

  expect_identical(
    determine_active_views(layout),
    c(grp1 = "view_a", grp2 = "view_b")
  )
})

test_that("determine_panel_pos places freely before the dock initialises", {

  dock <- list(
    layout = function() NULL,
    prev_active_group = function() NULL
  )

  expect_identical(
    determine_panel_pos(dock),
    list(direction = "right")
  )
})

test_that("show_panel() emits a views$show_panel intent on update", {

  brd <- new_dock_board(blocks = c(a = new_dataset_block()))

  captured <- NULL
  update <- function(x) captured <<- x

  show_panel("a", brd, update)

  expect_identical(
    captured,
    list(views = list(show_panel = list(id = "a", type = "block")))
  )
})

test_that("show_panel() rejects an id absent from the board", {

  brd <- new_dock_board(blocks = c(a = new_dataset_block()))

  expect_error(show_panel("missing", brd, function(x) NULL))
})
