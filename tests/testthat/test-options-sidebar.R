test_that("option_summary reads the value by default", {

  opt <- new_board_name_option()

  expect_identical(option_summary(opt, "Taming aracari"), "Taming aracari")
  expect_identical(option_summary(opt, 10L), "10")
  expect_null(option_summary(opt, NULL))
  expect_null(option_summary(opt, ""))
  expect_null(option_summary(opt, NA))
  expect_identical(option_summary(opt, list(list(1), list(2))), "2 items")
  expect_identical(option_summary(opt, 1:3), "3 items")
  # A list of single values (column roles) reads as the values.
  expect_identical(
    option_summary(opt, list(arm = "TRT", code = NULL, sev = "AETOXGR")),
    "TRT \u00b7 AETOXGR"
  )
  expect_identical(option_summary(opt, "a"), "a")

  # TRUE reads as the option's name, FALSE is left out.
  expect_identical(option_summary(opt, TRUE), "board name")
  expect_null(option_summary(opt, FALSE))
})

test_that("the dark mode summary names the appearance", {

  opt <- new_dark_mode_option()

  expect_identical(option_summary(opt, "light"), "Light")
  expect_identical(option_summary(opt, "dark"), "Dark")
  expect_identical(option_summary(opt, NULL), "System")
})

test_that("the compact summary shows only when it is on", {

  opt <- new_compact_option()

  expect_identical(option_summary(opt, TRUE), "Compact")
  expect_null(option_summary(opt, FALSE))
})

test_that("a category's summary joins its options' lines", {

  opts <- list(new_board_name_option(), new_dark_mode_option())
  vals <- list(board_name = "My board", dark_mode = "dark")

  expect_identical(category_summary(opts, vals), "My board \u00b7 Dark")

  # Nothing to show reads as not set.
  expect_identical(
    category_summary(list(new_board_name_option()), list(board_name = NULL)),
    "Not set"
  )

  # A method that errors does not take the sidebar down.
  bad <- structure(
    new_board_name_option(),
    class = c("bad_option", class(new_board_name_option()))
  )
  local_mocked_bindings(
    option_summary = function(x, value, ...) {
      if (inherits(x, "bad_option")) stop("boom") else "ok"
    }
  )
  expect_identical(category_summary(list(bad), list(board_name = "x")),
                   "Not set")
})

test_that("the sidebar lists categories and holds one page each", {

  options <- new_board_options(
    new_board_name_option(value = "Test board"),
    new_dark_mode_option(value = "light")
  )

  ui <- options_sidebar_ui("brd", options)
  html <- xml2::read_html(as.character(htmltools::tagList(ui)))

  rows <- xml2::xml_find_all(
    html, "//button[contains(@class, 'blockr-options-row')]"
  )
  expect_identical(
    xml2::xml_attr(rows, "data-category"),
    c("Board options", "Theme options")
  )
  # The row shows the category's noun and its summary.
  expect_identical(
    xml2::xml_text(
      xml2::xml_find_all(rows, ".//span[@class='blockr-options-row-name']")
    ),
    c("Board", "Theme")
  )
  expect_identical(
    xml2::xml_text(xml2::xml_find_all(
      rows, ".//span[@class='blockr-options-row-summary']"
    )),
    c("Test board", "Light")
  )

  pages <- xml2::xml_find_all(html, "//div[@class='blockr-options-page']")
  expect_identical(xml2::xml_attr(pages, "data-label"), c("Board", "Theme"))
  # Pages start hidden; the board name input sits in its page.
  expect_true(all(!is.na(xml2::xml_attr(pages, "hidden"))))
  expect_length(
    xml2::xml_find_all(pages[[1L]], ".//input[@id='brd-board_name']"),
    1L
  )
})
