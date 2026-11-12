test_that("ids", {

  blk_id <- "defunct_vole"

  blk_pnl_id <- as_block_panel_id(blk_id)

  expect_s3_class(blk_pnl_id, c("block_panel_id", "dock_panel_id", "dock_id"))

  expect_true(is_dock_panel_id(blk_pnl_id))
  expect_true(is_block_panel_id(blk_pnl_id))

  expect_true(maybe_block_panel_id(blk_pnl_id))

  expect_identical(blk_pnl_id, as_block_panel_id(blk_pnl_id))
  expect_identical(blk_id, as_obj_id(blk_pnl_id))

  expect_warning(
    as_block_panel_id(unclass(blk_pnl_id)),
    class = "maybe_multiple_block_id_conversion"
  )

  obj_ids <- c("foo", "bar", "baz")

  dock_pnl_ids <- lapply(obj_ids, as_block_panel_id)

  panel_ids <- chr_ply(dock_pnl_ids, unclass)

  expect_identical(dock_pnl_ids, as_dock_panel_id(panel_ids))
  expect_identical(as_obj_id(dock_pnl_ids), obj_ids)

  expect_error(
    as_dock_panel_id("foo"),
    class = "invalid_dock_panel_id_coercion"
  )
})

test_that("the panel ids of blocks are those of their ids", {

  blks <- c(a = new_dataset_block(), b = new_head_block())

  expect_identical(
    unclass(as_dock_panel_id(blks)),
    c("block_panel-a", "block_panel-b")
  )
  expect_identical(as_obj_id(as_block_panel_id(blks)), names(blks))
})

test_that("panel_obj_ids() strips the prefix of a block's panel id alone", {

  expect_identical(
    panel_obj_ids(c("block_panel-a", "b", "ext_panel-dag")),
    c("a", "b", "ext_panel-dag")
  )
  expect_identical(panel_obj_ids(character()), character())
})

test_that("panel-id predicates are class checks, not string matches", {

  expect_true(is_block_panel_id(as_block_panel_id("a")))
  expect_true(is_dock_panel_id(as_block_panel_id("a")))

  # A bare wire-prefixed string is not a classed panel id -- the string-shape
  # test is `maybe_block_panel_id()`, kept distinct on purpose.
  expect_false(is_block_panel_id("block_panel-a"))
  expect_false(is_dock_panel_id("block_panel-a"))
})
