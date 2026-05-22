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

test_that("update_stack.dock_stack merges delta", {

  ds <- new_dock_stack(
    blocks = c("a", "b"),
    name = "Hi",
    color = "#ff0000"
  )

  res <- update_stack(ds, list(name = "Bye"))

  expect_s3_class(res, "dock_stack")
  expect_identical(stack_name(res), "Bye")
  expect_identical(stack_color(res), "#ff0000")
  expect_identical(stack_blocks(res), c("a", "b"))

  res <- update_stack(ds, list(color = "#00ff00"))

  expect_s3_class(res, "dock_stack")
  expect_identical(stack_color(res), "#00ff00")
  expect_identical(stack_name(res), "Hi")
  expect_identical(stack_blocks(res), c("a", "b"))

  res <- update_stack(ds, list(blocks = c("a", "b", "c")))

  expect_s3_class(res, "dock_stack")
  expect_identical(stack_blocks(res), c("a", "b", "c"))
  expect_identical(stack_name(res), "Hi")
  expect_identical(stack_color(res), "#ff0000")

  res <- update_stack(
    ds,
    list(name = "All", color = "#0000ff", blocks = "z")
  )

  expect_s3_class(res, "dock_stack")
  expect_identical(stack_name(res), "All")
  expect_identical(stack_color(res), "#0000ff")
  expect_identical(stack_blocks(res), "z")

  res <- update_stack(ds, list())

  expect_s3_class(res, "dock_stack")
  expect_identical(stack_name(res), "Hi")
  expect_identical(stack_color(res), "#ff0000")
  expect_identical(stack_blocks(res), c("a", "b"))
})
