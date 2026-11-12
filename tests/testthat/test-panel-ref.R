test_that("blk() is a typed ref that encodes to its canonical panel id", {

  r <- blk("a")

  expect_true(is_panel_ref(r))
  expect_false(is_panel_ref("a"))
  expect_identical(as.character(r), "block_panel-a")
  expect_output(print(r), "<panel_ref> block_panel-a")

  # A ref carries no placement hint, so one fails at the call site.
  expect_error(blk("a", side = "right"), "unused argument")
})

test_that("blk() is accepted in the functions that write a grid", {

  grid <- dock_grid(
    blk("a"),
    panels(blk("b"), "c", active = blk("b")),
    rail(blk("d"), active = blk("d"))
  )

  expect_identical(
    grid_panel_ids(grid),
    c("block_panel-a", "block_panel-b", "c", "block_panel-d")
  )
  expect_identical(grid[["children"]][[2L]][["active"]], "block_panel-b")
  expect_identical(grid[["rails"]][["left"]][["active"]], "block_panel-d")
})
