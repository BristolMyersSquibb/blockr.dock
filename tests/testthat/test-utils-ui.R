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

test_that("group_front_panel resolves a group to its front panel, else NULL", {

  # The add-panel modal anchors an add `within` the clicked group by resolving
  # it to a member panel (the group's front / active tab).
  local_mocked_bindings(
    determine_active_views = function(layout) {
      c(grp1 = "block_panel-a", grp2 = "block_panel-b")
    }
  )

  dock <- list(layout = function() NULL)

  expect_identical(group_front_panel(dock, "grp2"), "block_panel-b")
  expect_null(group_front_panel(dock, "absent"))
})

test_that("empty_dock_prompt offers no add control when locked (#136)", {

  unlocked <- withr::with_options(
    list(blockr.locked = NULL),
    as.character(empty_dock_prompt(NS("x")))
  )
  expect_match(unlocked, 'id="x-empty_dock_add"', fixed = TRUE)

  locked <- withr::with_options(
    list(blockr.locked = TRUE),
    as.character(empty_dock_prompt(NS("x")))
  )
  expect_false(grepl("empty_dock_add", locked, fixed = TRUE))
  expect_match(locked, "lock-fill", fixed = TRUE)
})
