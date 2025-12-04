test_that("action utils", {

  inps <- block_input_select(
    new_rbind_block(),
    "test",
    links(),
    mode = "inputs"
  )

  expect_identical(inps, "1")
})
