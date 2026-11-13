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

test_that("the functions that write a grid keep each reference as written", {

  grid <- dock_grid(
    blk("a"),
    panels(plg("x"), "c", active = "c"),
    rail(blk("d"))
  )

  expect_identical(
    grid_panels(grid),
    list(blk("a"), plg("x"), new_panel_ref("c", "bare"), blk("d"))
  )
  expect_identical(grid$children[[2L]]$active, new_panel_ref("c", "bare"))

  expect_error(panels(list(1)), class = "dock_panel_ref_invalid")
})

test_that("an active panel given as a bare id is the reference of that id", {

  expect_identical(panels(blk("a"), plg("b"), active = "b")$active, plg("b"))
  expect_identical(
    rail("a", "b", active = blk("b"))$active,
    new_panel_ref("b", "bare")
  )

  expect_error(
    panels(blk("a"), active = plg("a")),
    class = "dock_panels_active_invalid"
  )
})
