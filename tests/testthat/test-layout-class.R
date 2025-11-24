test_that("panel layout", {
  expect_snapshot(draw_panel_tree(list("a", list("b", "c"))))
})
