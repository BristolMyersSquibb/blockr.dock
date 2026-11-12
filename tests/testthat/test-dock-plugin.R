test_that("the dock plugin is a frontend of its own class", {

  x <- new_dock_plugin()

  expect_s3_class(
    x,
    c("dock_plugin", "frontend_plugin", "plugin"),
    exact = TRUE
  )
  expect_identical(plugin_class_id(x), "blockr.dock-dock_plugin")
  expect_length(plugin_state(x), 0L)
})
