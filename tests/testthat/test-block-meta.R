test_that("block metadata", {

  meta1 <- blks_metadata(new_dataset_block())

  expect_s3_class(meta1, "data.frame")
  expect_true("color" %in% names(meta1))

  new_identity_block <- function() {
    new_transform_block(
      function(id, data) {
        moduleServer(
          id,
          function(input, output, session) {
            list(
              expr = reactive(quote(identity(data))),
              state = list()
            )
          }
        )
      },
      function(id) {
        tagList()
      },
      block_metadata = list(),
      class = "identity_block"
    )
  }

  meta2 <- blks_metadata(new_identity_block())

  expect_s3_class(meta2, "data.frame")
  expect_true("color" %in% names(meta1))

  meta3 <- blks_metadata(
    blocks(a = new_dataset_block(), b = new_identity_block())
  )

  expect_s3_class(meta3, "data.frame")
  expect_true("color" %in% names(meta1))

  # An unregistered block takes the default category, and with it the colour
  # blockr.ui gives a category outside its set.
  expect_identical(
    meta3$color,
    blockr.ui::category_color(c("input", "uncategorized"))
  )
})

test_that("blk_color() is deprecated for blockr.ui's category colours", {

  withr::local_options(rlib_warning_verbosity = "verbose")

  cats <- c("input", "plot", "not_a_category")

  expect_warning(
    res <- blk_color(cats),
    class = "deprecated_blk_color"
  )

  expect_identical(res, blockr.ui::category_color(cats))
})

test_that("blk_icon_data_uri() is deprecated", {

  withr::local_options(rlib_warning_verbosity = "verbose")

  meta <- blks_metadata(new_dataset_block())

  expect_warning(
    icon1 <- blk_icon_data_uri(meta[["icon"]], meta[["color"]]),
    class = "deprecated_blk_icon_data_uri"
  )

  expect_type(icon1, "character")
  expect_length(icon1, 1L)

  expect_warning(
    icon2 <- blk_icon_data_uri(meta[["icon"]], meta[["color"]],
                               mode = "inline"),
    class = "deprecated_blk_icon_data_uri"
  )

  expect_s3_class(icon2, "html")
  expect_length(icon2, 1L)
})
