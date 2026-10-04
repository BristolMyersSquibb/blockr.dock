test_that("core's dark mode option gets the dock's checkbox, unbound", {

  ui <- dock_option_ui(new_dark_mode_option("dark"), "board")
  root <- xml2::read_html(as.character(htmltools::tagList(ui)))
  box <- xml2::xml_find_first(root, "//input[@type='checkbox']")

  # No id, so Shiny does not bind it: dark-mode.js reports "light" / "dark"
  # under the option's input id instead.
  expect_true(is.na(xml2::xml_attr(box, "id")))
  expect_identical(xml2::xml_attr(box, "data-input"), "board-dark_mode")
  expect_identical(xml2::xml_attr(box, "data-mode"), "dark")
  expect_identical(xml2::xml_attr(box, "checked"), "checked")

  unset <- dock_option_ui(new_dark_mode_option(NULL), "board")
  expect_match(as.character(unset), 'data-mode="auto"', fixed = TRUE)
})

test_that("core's switch options become the dock's checkboxes", {

  html <- as.character(dock_option_ui(new_filter_rows_option(TRUE), "board"))
  root <- xml2::read_html(html)
  box <- xml2::xml_find_first(root, "//input[@type='checkbox']")

  expect_identical(xml2::xml_attr(box, "id"), "board-filter_rows")
  expect_identical(xml2::xml_attr(box, "checked"), "checked")
  expect_match(html, "blockr-checkbox", fixed = TRUE)
  expect_no_match(html, "form-switch", fixed = TRUE)
  expect_no_match(html, "bslib", fixed = TRUE)
})

test_that("other options keep their own control", {

  expect_identical(
    as.character(dock_option_ui(new_page_size_option(5L), "board")),
    as.character(board_option_ui(new_page_size_option(5L), "board"))
  )
})

test_that("the compact option is a dock checkbox", {

  opt <- new_compact_option(TRUE)
  html <- as.character(board_option_ui(opt, "board"))

  expect_match(html, 'id="board-compact"', fixed = TRUE)
  expect_match(html, "blockr-checkbox", fixed = TRUE)
  expect_no_match(html, "form-switch", fixed = TRUE)
})
