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
  expect_identical(
    xml2::xml_text(
      xml2::xml_find_all(rows, ".//span[@class='blockr-options-row-name']")
    ),
    c("Board options", "Theme options")
  )

  pages <- xml2::xml_find_all(html, "//div[@class='blockr-options-page']")
  expect_identical(
    xml2::xml_attr(pages, "data-category"),
    c("Board options", "Theme options")
  )
  # Pages start hidden; the board name input sits in its page.
  expect_true(all(!is.na(xml2::xml_attr(pages, "hidden"))))
  expect_length(
    xml2::xml_find_all(pages[[1L]], ".//input[@id='brd-board_name']"),
    1L
  )
})

test_that("an option without a category goes under Other options", {

  options <- new_board_options(
    new_board_name_option(value = "Test board", category = NULL),
    new_dark_mode_option(value = "light")
  )

  expect_named(
    option_categories(options),
    c("Other options", "Theme options")
  )

  html <- xml2::read_html(
    as.character(htmltools::tagList(options_sidebar_ui("brd", options)))
  )
  expect_length(
    xml2::xml_find_all(
      html,
      "//div[@data-category='Other options']//input[@id='brd-board_name']"
    ),
    1L
  )
})
