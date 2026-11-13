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

test_that("the dock's state has version 2 and a schema beside its module", {

  expect_identical(plugin_version(new_dock_plugin()), 2L)

  schema <- read_json_schema("js/dock.schema.json", package = "blockr.dock")

  expect_named(
    schema[["properties"]],
    c("views", "folders", "active", "visible")
  )
})

test_that("core finds the dock's plugin class in the package's listing", {

  plugins <- available_plugins()

  expect_identical(
    plugins[plugins[["package"]] == "blockr.dock", "id"],
    "dock_plugin"
  )
})
