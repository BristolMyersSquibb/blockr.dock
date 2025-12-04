test_that("dock stacks", {

  stks <- stacks(
    a = new_stack(),
    b = new_dock_stack(),
    c = new_stack()
  )

  res <- as_dock_stacks(stks)

  expect_s3_class(res, "stacks")

  for (x in res) {
    expect_true(is_dock_stack(x))
    expect_s3_class(x, "dock_stack")
  }
})
