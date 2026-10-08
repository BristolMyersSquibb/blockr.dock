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

test_that("the rail sync option tells the client, which keeps rails in step", {

  ms <- new_mock_session()
  withr::defer(if (!ms$isClosed()) ms$close())

  sent <- list()

  value <- with_mock_context(ms, reactiveVal(TRUE))

  session <- list(
    ns = identity,
    userData = list2env(list(board_options = list(sync_rails = value))),
    onFlush = function(fun, once = TRUE) fun(),
    sendInputMessage = function(...) invisible(),
    sendCustomMessage = function(type, message) {
      if (identical(type, "blockr-rail-sync")) {
        sent[[length(sent) + 1L]] <<- message
      }
      invisible()
    }
  )

  with_mock_context(
    ms,
    board_option_server(new_sync_rails_option(), session = session)
  )
  ms$flushReact()

  expect_identical(sent, list(TRUE))
  expect_true(rails_synced(session))

  with_mock_context(ms, value(FALSE))
  ms$flushReact()

  expect_identical(sent, list(TRUE, FALSE))
  expect_false(rails_synced(session))

  # A board without the option, such as one saved before it existed, keeps
  # each view's rails as they were left.
  expect_false(rails_synced(list(userData = new.env())))
})

test_that("views as tabs is a dock board option, off by default", {

  opts <- dock_board_options()

  expect_true("view_tabs" %in% names(opts))
  expect_false(board_option_default(opts[["view_tabs"]]))
  expect_identical(board_option_label(opts[["view_tabs"]]), "Views as tabs")
  expect_identical(
    board_option_category(opts[["view_tabs"]]),
    "Theme options"
  )

  expect_true(board_option_default(new_view_tabs_option(TRUE)))

  withr::local_options(blockr.view_tabs = TRUE)
  expect_true(board_option_default(new_view_tabs_option()))

  expect_error(
    new_view_tabs_option("yes"),
    class = "board_options_view_tabs_invalid"
  )
})

test_that("a board saved with views as tabs restores with them", {

  opt <- blockr_deser(blockr_ser(new_view_tabs_option(TRUE)))

  expect_s3_class(opt, "view_tabs_option")
  expect_true(board_option_value(opt))
  expect_identical(board_option_label(opt), "Views as tabs")

  on <- new_dock_board(options = new_board_options(opt))

  expect_true(board_view_tabs(on))
  expect_false(board_view_tabs(new_dock_board()))

  # A board saved before the option existed has none.
  expect_null(
    board_view_tabs(
      new_dock_board(options = new_board_options(new_board_name_option()))
    )
  )
})

test_that("the views as tabs option tells the view menu, whose row sets it", {

  ms <- new_mock_session()
  withr::defer(if (!ms$isClosed()) ms$close())

  menu <- list()
  switched <- list()

  value <- with_mock_context(ms, reactiveVal(FALSE))
  input <- with_mock_context(ms, reactiveValues())

  session <- list(
    ns = identity,
    input = input,
    userData = list2env(list(board_options = list(view_tabs = value))),
    onFlush = function(fun, once = TRUE) fun(),
    sendInputMessage = function(id, message) {
      if (identical(id, "view_nav")) {
        menu[[length(menu) + 1L]] <<- message
      }
      invisible()
    },
    sendCustomMessage = function(type, message) {
      if (identical(type, "bslib.toggle-input-binary")) {
        switched[[length(switched) + 1L]] <<- message$value
      }
      invisible()
    }
  )

  with_mock_context(
    ms,
    board_option_server(
      new_view_tabs_option(),
      board = list(board = new_dock_board()),
      session = session
    )
  )
  ms$flushReact()

  expect_identical(menu, list(list(tabs = FALSE)))

  # "Show views as tabs" asks for the state it wants, which sets the option:
  # the menu shows the tabs and the option's switch turns on.
  with_mock_context(ms, input$view_nav_tabs <- TRUE)
  ms$flushReact()

  expect_true(with_mock_context(ms, value()))
  expect_identical(menu, list(list(tabs = FALSE), list(tabs = TRUE)))
  expect_identical(switched, list(FALSE, TRUE))

  # A change made from the switch reaches the menu the same way.
  with_mock_context(ms, value(FALSE))
  ms$flushReact()

  expect_identical(menu[[3L]], list(tabs = FALSE))
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

test_that("Show code ends the list as a row of its own", {

  html <- xml2::read_html(
    as.character(htmltools::tagList(settings_body("brd", new_dock_board())))
  )

  row <- xml2::xml_find_all(
    html,
    "//div[@class='blockr-options-list']/*[last()]/button"
  )

  expect_length(row, 1L)
  expect_identical(xml2::xml_attr(row, "id"), "brd-generate_code-code_mod")
  expect_match(
    xml2::xml_attr(row, "class"),
    "action-button blockr-menu__item",
    fixed = TRUE
  )
  expect_identical(
    xml2::xml_text(
      xml2::xml_find_all(row, ".//span[@class='blockr-menu__label']")
    ),
    "Show code"
  )
})
