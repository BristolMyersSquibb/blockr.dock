test_that("block metadata", {

  meta1 <- blks_metadata(new_dataset_block())

  expect_s3_class(meta1, "data.frame")
  expect_identical(nrow(meta1), 1L)
  expect_identical(ncol(meta1), 7L)
  expect_named(
    meta1,
    c("id", "name", "description", "category", "icon", "package", "color")
  )

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
      class = "identity_block"
    )
  }

  meta2 <- blks_metadata(new_identity_block())

  expect_s3_class(meta2, "data.frame")
  expect_identical(nrow(meta2), 1L)
  expect_identical(ncol(meta2), 7L)
  expect_named(
    meta2,
    c("id", "name", "description", "category", "icon", "package", "color")
  )

  meta3 <- blks_metadata(
    blocks(a = new_dataset_block(), b = new_identity_block())
  )

  expect_s3_class(meta3, "data.frame")
  expect_identical(nrow(meta3), 2L)
  expect_identical(ncol(meta3), 7L)
  expect_named(
    meta3,
    c("id", "name", "description", "category", "icon", "package", "color")
  )
  expect_identical(rownames(meta3), c("a", "b"))

  icon1 <- blk_icon_data_uri(meta1[["icon"]], meta1[["color"]])

  expect_type(icon1, "character")
  expect_length(icon1, 1L)

  icon2 <- blk_icon_data_uri(meta1[["icon"]], meta1[["color"]],
                             mode = "inline")

  expect_s3_class(icon2, "html")
  expect_length(icon2, 1L)
})
