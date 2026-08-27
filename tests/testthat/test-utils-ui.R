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

test_that("a live active panel overrides its group's stale front (#361)", {

  leaf <- function(id, view, members) {
    list(
      type = "leaf",
      data = list(id = id, activeView = view, views = members)
    )
  }

  layout <- list(
    grid = list(
      root = list(
        type = "branch",
        data = list(
          leaf("grp1", "ext_panel-dag", "ext_panel-dag"),
          leaf(
            "grp2",
            "block_panel-a",
            c("block_panel-a", "block_panel-b", "block_panel-c")
          )
        )
      )
    )
  )

  # The echo alone fronts only block a -- the tab-switch-stays-blank bug: a bare
  # switch to b or c does not re-echo `_state`, so its block is never marked
  # visible.
  expect_identical(as.character(visible_block_ids(layout)), "a")

  # Folding in the client's live active panel fronts the selected block instead,
  # and only for the group that lists it.
  expect_identical(
    as.character(visible_block_ids(layout, "block_panel-c")),
    "c"
  )
  expect_identical(
    determine_active_views(layout, "block_panel-c"),
    c(grp1 = "ext_panel-dag", grp2 = "block_panel-c")
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

test_that("move_dom_elements batches a sweep into one message", {

  sent <- list()
  session <- list(
    sendCustomMessage = function(type, message) {
      sent[[length(sent) + 1L]] <<- list(type = type, message = message)
      invisible()
    }
  )

  move_dom_elements(c("#a", "#b", "#c"), "#off", session)

  expect_length(sent, 1L)
  expect_identical(sent[[1L]]$type, "move-element")
  expect_identical(
    sent[[1L]]$message,
    list(
      list(from = "#a", to = "#off"),
      list(from = "#b", to = "#off"),
      list(from = "#c", to = "#off")
    )
  )

  sent <- list()
  move_dom_elements(c("#a", "#b"), c("#p-a", "#p-b"), session)

  expect_length(sent, 1L)
  expect_identical(
    sent[[1L]]$message,
    list(
      list(from = "#a", to = "#p-a"),
      list(from = "#b", to = "#p-b")
    )
  )

  sent <- list()
  move_dom_elements(character(), "#off", session)

  expect_length(sent, 0L)
})

test_that("determine_panel_pos takes a narrow view's only group", {

  # That group fronts the DAG, which the wide path reserves -- leaving no
  # candidate, so the add would split a second group off and undo the collapse.
  local_mocked_bindings(
    determine_active_views = function(layout, ...) c(grp1 = "ext_panel-dag")
  )

  dock <- list(
    layout = function() NULL,
    prev_active_group = function() NULL
  )

  expect_identical(determine_panel_pos(dock), list(direction = "right"))

  dock[["narrow"]] <- TRUE

  expect_identical(
    determine_panel_pos(dock),
    list(referenceGroup = "grp1", direction = "within")
  )
})
