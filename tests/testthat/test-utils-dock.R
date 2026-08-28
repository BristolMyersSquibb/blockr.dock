test_that("panel tracking keeps the live_panels membership in step", {

  shiny::isolate({

    live_panels <- shiny::reactiveVal(character())

    track_panel_added(live_panels, "block_panel-a")
    track_panel_added(live_panels, "block_panel-b")
    track_panel_added(live_panels, "block_panel-a")

    expect_setequal(live_panels(), c("block_panel-a", "block_panel-b"))

    track_panel_removed(live_panels, "block_panel-a")

    expect_setequal(live_panels(), "block_panel-b")
  })
})

test_that("panel tracking is a no-op without a tracker", {
  expect_silent(track_panel_added(NULL, "block_panel-a"))
  expect_silent(track_panel_removed(NULL, "block_panel-a"))
})

test_that("add_block_panel records the panel in the dock tracker", {

  local_mocked_bindings(
    add_panel = function(...) invisible(),
    .package = "dockViewR"
  )

  shiny::isolate({

    dock <- list(proxy = NULL, live_panels = shiny::reactiveVal(character()))
    add_block_panel(c(a = new_dataset_block()), dock = dock)

    expect_setequal(dock$live_panels(), "block_panel-a")
  })
})

test_that("parking panels iterates object ids, not panel strings", {

  # The footgun: `for` over the classed vector `block_panel_ids()`
  # returns drops the panel-id class, so a plain "block_panel-a" string
  # would reach as_block_handle_id() and double-prefix
  # (block_handle-block_panel-a). The park/show loops convert to object
  # ids first via as_obj_id() — this guards that path.
  local_mocked_bindings(
    get_panels_ids = function(proxy) {
      c("block_panel-a", "block_panel-b", "ext_panel-x")
    },
    .package = "dockViewR"
  )

  block_handles <- character()
  for (oid in as_obj_id(block_panel_ids(proxy = NULL))) {
    block_handles <- c(block_handles, as.character(as_block_handle_id(oid)))
  }
  expect_setequal(block_handles, c("block_handle-a", "block_handle-b"))

  ext_handles <- character()
  for (oid in as_obj_id(ext_panel_ids(proxy = NULL))) {
    ext_handles <- c(ext_handles, as.character(as_ext_handle_id(oid)))
  }
  expect_identical(ext_handles, "ext_handle-x")
})

test_that("panel-id accessors are empty-safe", {

  local_mocked_bindings(
    get_panels_ids = function(proxy) character(),
    .package = "dockViewR"
  )

  expect_length(block_panel_ids(proxy = NULL), 0L)
  expect_length(ext_panel_ids(proxy = NULL), 0L)
})

test_that("is_narrow_viewport reads the reported width against a breakpoint", {

  expect_true(is_narrow_viewport(500))
  expect_false(is_narrow_viewport(1400))

  # Exactly at the breakpoint is wide: the collapse is for what falls below it.
  expect_false(is_narrow_viewport(narrow_breakpoint()))

  # A width that never arrived -- board_ui built after Shiny bound its inputs,
  # so the probe missed the initial batch -- renders the desktop grid.
  expect_false(is_narrow_viewport(NULL))
  expect_false(is_narrow_viewport(NA_real_))
  expect_false(is_narrow_viewport(c(500, 900)))
})

test_that("the group fraction is clamped to (0, 1], else the default", {

  expect_identical(narrow_group_fraction(), 0.8)

  withr::local_options(blockr.narrow_group_fraction = 0.5)
  expect_identical(narrow_group_fraction(), 0.5)

  withr::local_options(blockr.narrow_group_fraction = "0.5")
  expect_identical(narrow_group_fraction(), 0.5)

  for (bad in list(0, -1, 1.5, "half")) {
    withr::local_options(blockr.narrow_group_fraction = bad)
    expect_identical(narrow_group_fraction(), 0.8)
  }

  # A fraction of exactly 1 is a group per screenful, which is meaningful.
  withr::local_options(blockr.narrow_group_fraction = 1)
  expect_identical(narrow_group_fraction(), 1)
})

test_that("the stacked container is as tall as the rows it carries", {

  withr::local_options(blockr.narrow_group_fraction = 0.5)

  # Three full-height columns, each capped to half a viewport.
  expect_identical(
    narrow_stack_attrs(
      dock_grid("ext_panel-dag", "block_panel-a", "block_panel-b")
    ),
    list(style = "--blockr-stack-height: 150vh;")
  )

  # A short row shortens the stack rather than taking a uniform slot: the
  # quarter-height leaf contributes 25vh, not the 50vh cap the other two hit.
  expect_identical(
    narrow_stack_attrs(
      dock_grid("a", group("b", "c", sizes = c(0.25, 0.75)))
    ),
    list(style = "--blockr-stack-height: 125vh;")
  )

  # An empty view still needs a box, not a zero-height one.
  expect_match(narrow_stack_attrs(new_dock_grid())$style, "50vh", fixed = TRUE)
})

test_that("the breakpoint is tunable, and survives a string from the env", {

  withr::local_options(blockr.narrow_breakpoint = 600)

  expect_identical(narrow_breakpoint(), 600)
  expect_false(is_narrow_viewport(700))
  expect_true(is_narrow_viewport(500))

  withr::local_options(blockr.narrow_breakpoint = "600")
  expect_identical(narrow_breakpoint(), 600)

  withr::local_options(blockr.narrow_breakpoint = "wide-ish")
  expect_identical(narrow_breakpoint(), 900)
})
