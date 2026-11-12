test_that("navbar_item() validates its parts", {

  item <- navbar_item("help")

  expect_true(is_navbar_item(item))
  expect_false(is_navbar_items(item))
  expect_identical(item[["id"]], "help")
  expect_false(item[["fill"]])
  expect_false(item[["shrink"]])

  expect_error(navbar_item(""), class = "navbar_item_id_invalid")
  expect_error(navbar_item(c("a", "b")), class = "navbar_item_id_invalid")
  expect_error(navbar_item("a", fill = NA), class = "navbar_item_flex_invalid")
  expect_error(
    navbar_item("a", shrink = 1),
    class = "navbar_item_flex_invalid"
  )
})

test_that("navbar items combine with c() and subset with [ by id", {

  a <- navbar_item("a")
  b <- navbar_item("b")
  d <- navbar_item("d")

  items <- c(a, b)

  expect_true(is_navbar_items(items))
  expect_identical(names(items), c("a", "b"))

  # Items, collections and NULL combine in either order.
  expect_identical(names(c(items, d)), c("a", "b", "d"))
  expect_identical(names(c(d, items)), c("d", "a", "b"))
  expect_identical(names(c(items, NULL, d)), c("a", "b", "d"))
  expect_identical(names(c(items, list(d))), c("a", "b", "d"))

  # Reorder and drop by id or position.
  expect_identical(names(items[c("b", "a")]), c("b", "a"))
  expect_identical(names(c(items, d)[-2]), c("a", "d"))
  expect_identical(names(items[character()]), character())
  expect_identical(items[], items)
  expect_identical(items[["b"]], b)

  expect_error(items[c("a", "z")], class = "navbar_items_subset_invalid")
  expect_error(items[3], class = "navbar_items_contents_invalid")
  expect_error(c(items, a), class = "navbar_items_ids_invalid")
  expect_error(items[c("a", "a")], class = "navbar_items_ids_invalid")
  expect_error(c(items, "a"), class = "navbar_items_coercion_invalid")
})

test_that("default_navbar_items() are the dock's parts in order", {

  items <- default_navbar_items()

  expect_true(is_navbar_items(items))
  expect_identical(
    names(items),
    c("logo", "spacer", "views", "read_only", "options")
  )
  expect_true(items[["spacer"]][["fill"]])
})
