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

  icon1 <- blk_icon_data_uri(meta1[["icon"]], meta1[["color"]])

  expect_type(icon1, "character")
  expect_length(icon1, 1L)

  icon2 <- blk_icon_data_uri(meta1[["icon"]], meta1[["color"]],
                             mode = "inline")

  expect_s3_class(icon2, "html")
  expect_length(icon2, 1L)
})
