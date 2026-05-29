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
