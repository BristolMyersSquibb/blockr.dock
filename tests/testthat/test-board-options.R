test_that("the compact switch is a dock board option, off by default", {

  opts <- dock_board_options()

  expect_true("compact" %in% names(opts))
  expect_false(board_option_default(opts[["compact"]]))

  expect_true(board_option_default(new_compact_option(TRUE)))

  expect_error(
    new_compact_option("yes"),
    class = "board_options_compact_invalid"
  )
})

test_that("the dock's options are Shiny's checkboxes, not bslib's switches", {

  for (opt in list(new_compact_option(TRUE), new_sync_rails_option(TRUE))) {

    id <- board_option_id(opt)
    root <- xml2::read_html(as.character(board_option_ui(opt, "board")))
    box <- xml2::xml_find_all(root, "//div[@class='checkbox']/label/input")

    expect_length(box, 1L)
    expect_identical(xml2::xml_attr(box, "id"), paste0("board-", id))
    expect_identical(xml2::xml_attr(box, "checked"), "checked")
    expect_length(
      xml2::xml_find_all(root, "//*[contains(@class, 'form-switch')]"),
      0L
    )
  }
})

test_that("the rail sync switch is a dock board option, on by default", {

  opts <- dock_board_options()

  expect_true("sync_rails" %in% names(opts))
  expect_true(board_option_default(opts[["sync_rails"]]))

  expect_false(board_option_default(new_sync_rails_option(FALSE)))

  expect_error(
    new_sync_rails_option("yes"),
    class = "board_options_sync_rails_invalid"
  )
})

# The session an option's server runs in, with the input and custom messages it
# sends recorded: updateCheckboxInput() needs a session that is Shiny's.
recording_session <- function(options) {

  ms <- new_mock_session()

  ms$userData$board_options <- options
  ms$userData$inputs <- list()
  ms$userData$messages <- list()

  ms$sendInputMessage <- function(id, message) {
    sent <- ms$userData$inputs[[id]]
    ms$userData$inputs[[id]] <- c(sent, message$value)
    invisible()
  }

  ms$sendCustomMessage <- function(type, message) {
    sent <- ms$userData$messages[[type]]
    ms$userData$messages[[type]] <- c(sent, message)
    invisible()
  }

  ms
}

test_that("the compact option sets its box and tells the client", {

  value <- reactiveVal(TRUE)
  ms <- recording_session(list(compact = value))
  withr::defer(if (!ms$isClosed()) ms$close())

  with_mock_context(ms, board_option_server(new_compact_option(), session = ms))
  ms$flushReact()

  expect_identical(ms$userData$inputs$compact, TRUE)
  expect_identical(ms$userData$messages[["blockr-compact"]], TRUE)

  with_mock_context(ms, value(FALSE))
  ms$flushReact()

  expect_identical(ms$userData$inputs$compact, c(TRUE, FALSE))
  expect_identical(ms$userData$messages[["blockr-compact"]], c(TRUE, FALSE))
})

test_that("the rail sync option tells the client, which keeps rails in step", {

  value <- reactiveVal(TRUE)
  ms <- recording_session(list(sync_rails = value))
  withr::defer(if (!ms$isClosed()) ms$close())

  with_mock_context(
    ms,
    board_option_server(new_sync_rails_option(), session = ms)
  )
  ms$flushReact()

  expect_identical(ms$userData$inputs$sync_rails, TRUE)
  expect_identical(ms$userData$messages[["blockr-rail-sync"]], TRUE)
  expect_true(rails_synced(ms))

  with_mock_context(ms, value(FALSE))
  ms$flushReact()

  expect_identical(ms$userData$inputs$sync_rails, c(TRUE, FALSE))
  expect_identical(ms$userData$messages[["blockr-rail-sync"]], c(TRUE, FALSE))
  expect_false(rails_synced(ms))

  # A board without the option, such as one saved before it existed, keeps
  # each view's rails as they were left.
  expect_false(rails_synced(list(userData = new.env())))
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

test_that("Show code ends the list and opens a page of its own", {

  html <- xml2::read_html(
    as.character(htmltools::tagList(settings_body("brd", new_dock_board())))
  )

  row <- xml2::xml_find_all(
    html,
    "//div[@class='blockr-options-list']/button[last()]"
  )

  expect_length(row, 1L)
  expect_identical(xml2::xml_attr(row, "data-category"), ".code")
  expect_match(xml2::xml_attr(row, "class"), "blockr-options-row", fixed = TRUE)
  expect_identical(
    xml2::xml_text(
      xml2::xml_find_all(row, ".//span[@class='blockr-menu__label']")
    ),
    "Show code"
  )

  page <- xml2::xml_find_all(
    html,
    "//div[@data-category='.code' and @data-label='Code']"
  )

  expect_length(page, 1L)
  expect_identical(
    xml2::xml_attr(
      xml2::xml_find_first(page, ".//*[@data-open-input]"),
      "data-open-input"
    ),
    "brd-generate_code-code_mod"
  )
})

test_that("a locked board's sidebar keeps the code page and nothing else", {

  html <- withr::with_options(
    list(blockr.locked = TRUE),
    xml2::read_html(
      as.character(htmltools::tagList(settings_body("brd", new_dock_board())))
    )
  )

  expect_identical(
    xml2::xml_attr(
      xml2::xml_find_all(
        html,
        "//button[contains(@class, 'blockr-options-row')]"
      ),
      "data-category"
    ),
    ".code"
  )
})
