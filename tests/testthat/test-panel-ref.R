test_that("blk() and plg() name the panel of a block and that of a plugin", {

  expect_true(is_panel_ref(blk("a")))
  expect_true(is_panel_ref(plg("blockr.dag-dag_plugin")))
  expect_false(is_panel_ref("a"))

  expect_output(print(blk("a")), "<panel_ref> blk(\"a\")", fixed = TRUE)
  expect_output(print(plg("dag")), "<panel_ref> plg(\"dag\")", fixed = TRUE)

  expect_error(blk(""), class = "dock_panel_ref_invalid")
  expect_error(plg(c("a", "b")), class = "dock_panel_ref_invalid")
  expect_error(blk(NA_character_), class = "dock_panel_ref_invalid")

  # A reference carries no placement hint, so one fails at the call site.
  expect_error(blk("a", side = "right"), "unused argument")
})

test_that("the functions that write a grid take a bare id for a block", {

  grid <- dock_grid(
    blk("a"),
    panels(plg("x"), "c", active = "c"),
    rail(blk("d"))
  )

  expect_identical(
    grid_panels(grid),
    list(blk("a"), plg("x"), blk("c"), blk("d"))
  )
  expect_identical(grid$children[[2L]]$active, blk("c"))

  expect_error(panels(list(1)), class = "dock_panel_ref_invalid")
})

test_that("an active panel given as a bare id is the panel of that block", {

  expect_identical(panels(blk("a"), "b", active = "b")$active, blk("b"))
  expect_identical(rail("a", "b", active = blk("b"))$active, blk("b"))

  # A bare id names a block, and never a plugin.
  expect_error(
    panels(blk("a"), plg("b"), active = "b"),
    class = "dock_panels_active_invalid"
  )
  expect_error(
    panels(blk("a"), active = plg("a")),
    class = "dock_panels_active_invalid"
  )
})
