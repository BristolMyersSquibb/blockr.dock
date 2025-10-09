test_that("ids", {

  blk_id <- "defunct_vole"

  blk_pnl_id <- as_block_panel_id(blk_id)

  expect_s3_class(blk_pnl_id, c("block_panel_id", "dock_panel_id", "dock_id"))

  expect_true(is_dock_id(blk_pnl_id))
  expect_true(is_dock_panel_id(blk_pnl_id))
  expect_true(is_block_panel_id(blk_pnl_id))

  expect_true(maybe_block_panel_id(blk_pnl_id))

  expect_identical(blk_pnl_id, as_block_panel_id(blk_pnl_id))
  expect_identical(blk_id, as_obj_id(blk_pnl_id))

  expect_error(
    as_ext_panel_id(blk_pnl_id),
    class = "invalid_ext_panel_id_coercion"
  )

  blk_hndl_id <- as_block_handle_id(blk_pnl_id)

  expect_s3_class(blk_hndl_id, c("block_handle_id", "dock_id"))

  expect_true(is_dock_id(blk_hndl_id))
  expect_true(is_block_handle_id(blk_hndl_id))

  expect_true(maybe_block_handle_id(blk_hndl_id))

  expect_identical(blk_hndl_id, as_block_handle_id(blk_hndl_id))
  expect_identical(blk_id, as_obj_id(blk_hndl_id))

  expect_error(
    as_ext_panel_id(blk_hndl_id),
    class = "invalid_ext_panel_id_coercion"
  )

  ext_id <- "extension"

  ext_pnl_id <- as_ext_panel_id(ext_id)

  expect_s3_class(ext_pnl_id, c("ext_panel_id", "dock_panel_id", "dock_id"))

  expect_true(is_dock_id(ext_pnl_id))
  expect_true(is_dock_panel_id(ext_pnl_id))
  expect_true(is_ext_panel_id(ext_pnl_id))

  expect_true(maybe_ext_panel_id(ext_pnl_id))

  expect_identical(ext_pnl_id, as_ext_panel_id(ext_pnl_id))
  expect_identical(ext_id, as_obj_id(ext_pnl_id))

  expect_error(
    as_block_panel_id(ext_pnl_id),
    class = "invalid_block_panel_id_coercion"
  )

  expect_error(
    as_block_handle_id(ext_pnl_id),
    class = "invalid_block_handle_id_coercion"
  )

  obj_ids <- c("foo", "bar", "baz")

  dock_pnl_ids <- map(
    do.call,
    c(as_ext_panel_id, as_block_panel_id, as_block_panel_id),
    lapply(obj_ids, list)
  )

  panel_ids <- chr_ply(dock_pnl_ids, unclass)

  expect_identical(dock_pnl_ids, as_dock_panel_id(panel_ids))
  expect_identical(as_obj_id(dock_pnl_ids), obj_ids)
})
