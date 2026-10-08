navbar_test_item <- function(id, ...) {
  navbar_item(id, function(id, board) shiny::tags$span(class = id), ...)
}

test_that("navbar_item() validates its parts", {

  item <- navbar_test_item("help")

  expect_true(is_navbar_item(item))
  expect_false(is_navbar_items(item))

  expect_error(
    navbar_item("", function(id, board) NULL),
    class = "navbar_item_id_invalid"
  )
  expect_error(
    navbar_item(c("a", "b"), function(id, board) NULL),
    class = "navbar_item_id_invalid"
  )
  expect_error(navbar_item("a", NULL), class = "navbar_item_ui_invalid")
  expect_error(
    navbar_test_item("a", server = "srv"),
    class = "navbar_item_server_invalid"
  )
  expect_error(
    navbar_test_item("a", fill = NA),
    class = "navbar_item_flex_invalid"
  )
  expect_error(
    navbar_test_item("a", shrink = 1),
    class = "navbar_item_flex_invalid"
  )
})

test_that("navbar items combine with c() and subset with [ by id", {

  a <- navbar_test_item("a")
  b <- navbar_test_item("b")
  d <- navbar_test_item("d")

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

  expect_error(items[c("a", "z")], class = "navbar_items_subset_invalid")
  expect_error(items[3], class = "navbar_items_contents_invalid")
  expect_error(c(items, a), class = "navbar_items_ids_invalid")
  expect_error(items[c("a", "a")], class = "navbar_items_ids_invalid")
  expect_error(c(items, "a"), class = "navbar_items_coercion_invalid")
})

test_that("plugin_navbar_item() places a plugin under its own id", {

  expect_null(plugin_navbar_item(NULL))

  plg <- board_plugins(new_dock_board())[["preserve_board"]]
  item <- plugin_navbar_item(plg)

  expect_true(is_navbar_item(item))
  expect_identical(item[["id"]], "preserve_board")
  expect_identical(item[["ui"]], plugin_ui(plg))
  expect_null(item[["server"]])
  expect_true(item[["shrink"]])
  expect_false(item[["fill"]])

  # A plugin without a UI gives an item that leaves itself out.
  bare <- plugin_navbar_item(notify_user(ui = NULL))
  expect_null(bare[["ui"]]("test-notify_user", new_dock_board()))
})

test_that("default_navbar_items() are the dock's controls in order", {

  brd <- new_dock_board()
  plg <- board_plugins(brd)

  items <- default_navbar_items(brd, plg)

  expect_true(is_navbar_items(items))
  expect_identical(
    names(items),
    c("preserve_board", "spacer", "busy", "views", "read_only", "options")
  )
  expect_true(items[["spacer"]][["fill"]])
  expect_true(items[["preserve_board"]][["shrink"]])
  expect_identical(blockr_app_navbar(brd, plg), items)

  # Without the plugin there is no piece of it to place.
  expect_identical(
    names(default_navbar_items(brd, plg[c("edit_block", "generate_code")])),
    c("spacer", "busy", "views", "read_only", "options")
  )
})

test_that("custom_navbar() appends items to the default", {

  brd <- new_dock_board()
  plg <- board_plugins(brd)

  navbar <- custom_navbar(navbar_test_item("help"))

  expect_true(is.function(navbar))
  expect_identical(
    names(navbar(brd, plg)),
    c(names(default_navbar_items(brd, plg)), "help")
  )

  # An item with a default's id takes its place at the end, as
  # custom_plugins() does for plugins.
  expect_identical(
    names(custom_navbar(navbar_test_item("views"))(brd, plg)),
    c("preserve_board", "spacer", "busy", "read_only", "options", "views")
  )

  expect_error(custom_navbar("help"), class = "navbar_items_coercion_invalid")
})

test_that("the navbar function's return value is validated", {

  brd <- new_dock_board()
  plg <- board_plugins(brd)

  expect_error(
    resolve_navbar("x", brd, plg),
    class = "navbar_fun_invalid"
  )
  expect_error(
    resolve_navbar(function(x, plugins) navbar_test_item("a"), brd, plg),
    class = "navbar_items_structure_invalid"
  )

  # Built around the constructors, as a hand-rolled object could be.
  dup <- structure(
    list(navbar_test_item("a"), navbar_test_item("a")),
    class = "navbar_items"
  )
  expect_error(
    resolve_navbar(function(x, plugins) dup, brd, plg),
    class = "navbar_items_ids_invalid"
  )

  expect_error(
    board_ui("test", brd, plg, navbar = list()),
    class = "navbar_items_structure_invalid"
  )
})

test_that("board_ui() draws the navbar from its items", {

  skip_if_not_installed("xml2")

  brd <- new_dock_board(blocks = c(a = new_dataset_block()))
  plg <- board_plugins(brd)

  seen <- new.env()

  probe <- navbar_item(
    "probe",
    function(id, board) {
      seen$id <- id
      seen$board <- board
      shiny::tags$span(class = "probe")
    },
    shrink = TRUE
  )

  skipped <- navbar_item("skipped", function(id, board) NULL)

  items <- c(default_navbar_items(brd, plg), probe, skipped)

  doc <- xml2::read_html(
    as.character(board_ui("test", brd, plg, navbar = items))
  )

  wrappers <- xml2::xml_find_all(
    doc,
    "//div[contains(concat(' ', @class, ' '), ' blockr-navbar ')]/div"
  )

  # An item whose UI returns NULL is left out, and so is the read-only
  # indicator of an unlocked board.
  expect_identical(
    xml2::xml_attr(wrappers, "data-navbar-item"),
    c("preserve_board", "spacer", "busy", "views", "options", "probe")
  )

  cls <- set_names(
    xml2::xml_attr(wrappers, "class"),
    xml2::xml_attr(wrappers, "data-navbar-item")
  )

  expect_identical(
    cls[["spacer"]],
    "blockr-navbar-item blockr-navbar-item-fill"
  )
  expect_identical(
    cls[["preserve_board"]],
    "blockr-navbar-item blockr-navbar-item-shrink"
  )
  expect_identical(cls[["busy"]], "blockr-navbar-item")

  # An item's UI is called with `NS(board_id, item_id)` and the board.
  expect_identical(seen$id, "test-probe")
  expect_identical(seen$board, brd)

  # The plugin's piece is drawn under the namespace core serves the plugin
  # under, and the dock's own controls keep the board's: the board server
  # reads the view menu's input and the options button opens the board's
  # sidebar.
  expect_length(
    xml2::xml_find_all(doc, "//*[@id='test-preserve_board-restore']"),
    1L
  )
  expect_length(xml2::xml_find_all(doc, "//*[@id='test-view_nav']"), 1L)
  expect_identical(
    xml2::xml_attr(
      xml2::xml_find_first(doc, "//*[@data-blockr-sidebar-target]"),
      "data-blockr-sidebar-target"
    ),
    "test-settings_sidebar"
  )

  # The view menu carries its rule.
  expect_length(
    xml2::xml_find_all(
      doc,
      paste0(
        "//div[@data-navbar-item='views']",
        "/span[contains(concat(' ', @class, ' '), ' blockr-navbar-rule ')]"
      )
    ),
    1L
  )
})

test_that("a board without the plugin keeps its controls on the right", {

  skip_if_not_installed("xml2")

  brd <- new_dock_board()
  plg <- board_plugins(brd)[c("edit_block", "generate_code")]

  doc <- xml2::read_html(as.character(board_ui("test", brd, plg)))

  wrappers <- xml2::xml_find_all(
    doc,
    "//div[contains(concat(' ', @class, ' '), ' blockr-navbar ')]/div"
  )

  # The spacer leads, so it pushes the rest of the bar to the right.
  expect_identical(
    xml2::xml_attr(wrappers, "data-navbar-item"),
    c("spacer", "busy", "views", "options")
  )
})

test_that("the views item draws the views as tabs as the board's option is", {

  skip_if_not_installed("xml2")

  board <- function(...) {
    new_dock_board(
      blocks = c(a = new_dataset_block(), b = new_dataset_block()),
      views = list(First = "a", Second = "b"),
      active = "Second",
      ...
    )
  }

  draw <- function(brd) {
    xml2::read_html(as.character(board_ui("test", brd)))
  }

  line <- function(doc) {
    xml2::xml_find_first(
      doc,
      "//div[@data-navbar-item='views']/div[@id='test-view_nav-tabs']"
    )
  }

  row <- function(doc) {
    xml2::xml_find_first(
      doc,
      paste0(
        "//*[@id='test-view_nav']//button",
        "[contains(concat(' ', @class, ' '), ' blockr-view-tabs-toggle ')]"
      )
    )
  }

  # Off by default: the line is drawn hidden, a tab per view with the current
  # one marked, and the menu's row for it is unchecked.
  off <- draw(board())
  tabs <- xml2::xml_find_all(line(off), "./button")

  expect_false(is.na(xml2::xml_attr(line(off), "hidden")))
  expect_identical(xml2::xml_attr(tabs, "data-view-id"), c("First", "Second"))
  expect_identical(xml2::xml_text(tabs), c("First", "Second"))
  expect_identical(xml2::xml_attr(tabs, "aria-selected"), c("false", "true"))
  expect_identical(
    xml2::xml_attr(tabs, "class"),
    c("blockr-view-tab", "blockr-view-tab is-active")
  )
  expect_identical(xml2::xml_attr(row(off), "aria-checked"), "false")
  expect_identical(trimws(xml2::xml_text(row(off))), "Show views as tabs")

  # A board saved with the option on shows the line from its first paint.
  withr::with_options(list(blockr.view_tabs = TRUE), on <- draw(board()))

  expect_true(is.na(xml2::xml_attr(line(on), "hidden")))
  expect_identical(xml2::xml_attr(row(on), "aria-checked"), "true")

  # A board without the option draws neither.
  none <- draw(board(options = new_board_options(new_board_name_option())))

  expect_s3_class(line(none), "xml_missing")
  expect_s3_class(row(none), "xml_missing")

  # A locked board shows the tabs it was saved with, but cannot change the
  # option, so its menu has no row for it.
  withr::local_options(blockr.locked = TRUE)
  withr::with_options(list(blockr.view_tabs = TRUE), locked <- draw(board()))

  expect_true(is.na(xml2::xml_attr(line(locked), "hidden")))
  expect_s3_class(row(locked), "xml_missing")
})

test_that("the app UI resolves `navbar` and appends only unnamed `...`", {

  brd <- new_dock_board()
  plg <- board_plugins(brd)

  html <- as.character(
    blockr_app_ui(
      "test",
      brd,
      plg,
      blockr_app_options(brd),
      shiny::tags$div(class = "page-extra"),
      callbacks = shiny::tags$div(class = "server-extra"),
      navbar = custom_navbar(navbar_test_item("help"))
    )
  )

  expect_match(html, 'data-navbar-item="help"', fixed = TRUE)
  expect_match(html, "page-extra", fixed = TRUE)
  expect_false(grepl("server-extra", html, fixed = TRUE))

  # Without `navbar`, the board's default.
  html <- as.character(blockr_app_ui("test", brd, plg, blockr_app_options(brd)))

  expect_match(html, 'data-navbar-item="options"', fixed = TRUE)
  expect_false(grepl('data-navbar-item="help"', html, fixed = TRUE))
})

test_that("an item's server runs once under its UI's namespace", {

  brd <- new_dock_board()

  calls <- new.env()
  calls$n <- 0L

  item <- navbar_item(
    "probe",
    function(id, board) shiny::uiOutput(shiny::NS(id, "out")),
    function(id, board) {
      calls$n <- calls$n + 1L
      calls$board <- board
      shiny::moduleServer(id, function(input, output, session) {
        calls$ns <- session$ns("out")
      })
    }
  )

  testServer(
    blockr_app_server,
    {
      session$flushReact()

      expect_identical(calls$n, 1L)
      expect_identical(calls$ns, "my_board-probe-out")
      expect_identical(shiny::isolate(calls$board$board_id), "my_board")
    },
    args = list(
      id = "my_board",
      x = brd,
      plugins = board_plugins(brd),
      options = blockr_app_options(brd),
      navbar = custom_navbar(item)
    )
  )
})

test_that("navbar items lay out in one row in the browser", {

  skip_on_cran()

  app <- new_app_driver(
    system.file("examples", "navbar", "app.R", package = "blockr.dock"),
    name = "navbar",
    seed = 42,
    load_timeout = 30 * 1000,
    timeout = 20 * 1000
  )
  withr::defer(app$stop())

  wait_dock_loaded(app, 1)

  # The appended item's server filled its output under the item's namespace,
  # with the board it was handed.
  app$wait_for_js("document.querySelector('.navbar-filled') !== null")
  expect_identical(
    app$get_js("document.querySelector('.navbar-filled').textContent"),
    "my_board"
  )

  box <- function(item) {
    app$get_js(
      sprintf(
        paste0(
          "(function() {",
          "  var el = document.querySelector('[data-navbar-item=\"%s\"]');",
          "  var r = el.getBoundingClientRect();",
          "  return {position: getComputedStyle(el).position, left: r.left,",
          "    right: r.right, top: r.top, bottom: r.bottom,",
          "    scroll: el.scrollWidth, width: r.width};",
          "})()"
        ),
        item
      )
    )
  }

  bar <- function() {
    app$get_js(
      paste0(
        "(function() {",
        "  var el = document.querySelector('.blockr-navbar');",
        "  var r = el.getBoundingClientRect();",
        "  var pad = parseFloat(getComputedStyle(el).paddingRight);",
        "  return {right: r.right - pad};",
        "})()"
      )
    )
  }

  # The empty item takes no room rather than leave a box that flexbox would
  # count in the bar's gap: what follows the options button sits one gap after
  # it. It stays rendered, so Shiny does not suspend its output.
  expect_identical(box("empty")$position, "absolute")
  expect_identical(box("filled")$position, "static")
  expect_equal(box("filled")$left - box("options")$right, 8, tolerance = 0.01)

  # The spacer pushes the rest of the bar to its right edge.
  expect_equal(box("filled")$right, bar()$right, tolerance = 0.01)

  # On a narrow bar the plugin's piece gives way, its content shrinking with
  # it rather than spill over its neighbours, and the rest keeps its width and
  # stays on the bar, on one line. At this width the piece gets about 250px of
  # the 370px it takes on a wide bar, well above the 130px its content needs.
  piece_width <- box("preserve_board")$width
  options_width <- box("options")$width

  app$set_window_size(width = 560, height = 800)
  app$wait_for_js(
    sprintf(
      paste0(
        "document.querySelector('[data-navbar-item=\"preserve_board\"]')",
        ".getBoundingClientRect().width < %f"
      ),
      piece_width
    )
  )

  piece <- box("preserve_board")
  expect_lt(piece$width, piece_width)
  expect_lte(piece$scroll, ceiling(piece$width))
  expect_equal(box("options")$width, options_width, tolerance = 0.01)
  expect_lte(box("filled")$right, bar()$right + 0.5)

  mid <- function(b) (b$top + b$bottom) / 2
  expect_equal(mid(box("views")), mid(box("options")), tolerance = 0.01)
  expect_equal(mid(box("filled")), mid(box("options")), tolerance = 0.01)
})

test_that("the views show as tabs under the bar, in step with the menu", {

  skip_on_cran()

  app <- new_app_driver(
    system.file("examples", "view-tabs", "app.R", package = "blockr.dock"),
    name = "view-tabs",
    seed = 42,
    load_timeout = 30 * 1000,
    timeout = 20 * 1000
  )
  withr::defer(app$stop())

  wait_view_nav(app, 3)

  tabs <- function() {
    res <- app$get_js(
      paste0(
        "(function() {",
        "  var line = document.getElementById('my_board-view_nav-tabs');",
        "  var tabs = Array.from(line.children);",
        "  return {",
        "    hidden: line.hidden,",
        "    ids: tabs.map(function (t) { return t.dataset.viewId; }),",
        "    names: tabs.map(function (t) { return t.textContent; }),",
        "    active: tabs.filter(function (t) {",
        "      return t.classList.contains('is-active');",
        "    }).map(function (t) { return t.dataset.viewId; })",
        "  };",
        "})()"
      )
    )
    res[c("ids", "names", "active")] <- lapply(
      res[c("ids", "names", "active")],
      function(x) as.character(unlist(x))
    )
    res
  }

  wait_tabs <- function(cond) {
    wait_js(
      app,
      paste0(
        "(function() {",
        "  var line = document.getElementById('my_board-view_nav-tabs');",
        "  return ", cond, ";",
        "})()"
      ),
      function() utils::capture.output(str(tabs()))
    )
  }

  box <- function(sel) {
    app$get_js(
      sprintf(
        paste0(
          "(function() {",
          "  var r = document.querySelector('%s').getBoundingClientRect();",
          "  return {top: r.top, bottom: r.bottom, height: r.height};",
          "})()"
        ),
        sel
      )
    )
  }

  row <- "#my_board-view_nav .blockr-view-tabs-toggle"
  options <- "[data-navbar-item=\"options\"]"
  switched <- "document.getElementById('my_board-view_tabs').checked"

  checked <- function() {
    app$get_js(
      sprintf("document.querySelector('%s').getAttribute('aria-checked')", row)
    )
  }

  # A board saved with the option shows the tab line from the start, one tab
  # per view with the current one marked, and the view menu as a chevron
  # alone. The line sits under the bar's row, which is laid out as without
  # it, and the views give up its height.
  expect_false(tabs()$hidden)
  expect_identical(tabs()$ids, c("First", "Second", "Third"))
  expect_identical(tabs()$active, "First")
  expect_identical(checked(), "true")
  expect_identical(
    app$get_js(
      paste0(
        "getComputedStyle(document.querySelector(",
        "'.blockr-view-toggle-label')).display"
      )
    ),
    "none"
  )

  bar <- box(".blockr-navbar")
  line <- box("#my_board-view_nav-tabs")

  expect_equal(bar$height, 87, tolerance = 0.01)
  expect_equal(line$top, 48, tolerance = 0.01)
  expect_equal(line$bottom, 86, tolerance = 0.01)
  expect_equal(
    (box(options)$top + box(options)$bottom) / 2,
    47 / 2,
    tolerance = 0.01
  )
  expect_equal(box(".blockr-view-container")$top, 87, tolerance = 0.01)

  # A tab switches views as a pick in the menu does.
  app$run_js(
    paste0(
      "document.querySelector('#my_board-view_nav-tabs ",
      "[data-view-id=\"Second\"]').click()"
    )
  )
  wait_view_handle(app, "Second")
  wait_js(
    app,
    paste0(
      "document.querySelector('#my_board-view_nav .active')",
      ".dataset.viewId === 'Second'"
    ),
    function() utils::capture.output(print(read_view_nav(app)))
  )

  expect_identical(tabs()$active, "Second")

  # The menu's row turns the option off: the line goes, the bar is back to
  # its own height, and the option's switch follows. Then on again.
  app$run_js("document.querySelector('.blockr-view-toggle').click()")
  app$run_js(sprintf("document.querySelector('%s').click()", row))
  wait_tabs(paste0("line.hidden && !", switched))

  expect_identical(checked(), "false")
  expect_equal(box(".blockr-navbar")$height, 48, tolerance = 0.01)
  expect_equal(box(".blockr-view-container")$top, 48, tolerance = 0.01)

  app$run_js("document.querySelector('.blockr-view-toggle').click()")
  app$run_js(sprintf("document.querySelector('%s').click()", row))
  wait_tabs(paste0("!line.hidden && ", switched))

  expect_identical(checked(), "true")

  # Adding, renaming, reordering and removing a view reach the tabs through
  # the menu's messages.
  app$run_js(
    paste0(
      "Shiny.setInputValue('my_board-view_nav_add', Date.now(), ",
      "{priority: 'event'})"
    )
  )
  wait_tabs("line.children.length === 4")

  added <- setdiff(tabs()$ids, c("First", "Second", "Third"))

  wait_tabs(
    sprintf(
      "line.querySelector('.is-active').dataset.viewId === '%s'",
      added
    )
  )

  app$run_js(
    sprintf(
      paste0(
        "Shiny.setInputValue('my_board-view_nav_rename', ",
        "{id: '%s', to: 'Fourth'}, {priority: 'event'})"
      ),
      added
    )
  )
  wait_tabs("line.textContent.indexOf('Fourth') !== -1")

  expect_identical(tabs()$names, c("First", "Second", "Third", "Fourth"))

  app$run_js(
    sprintf(
      paste0(
        "Shiny.setInputValue('my_board-view_nav_reorder', ",
        "{order: ['Second', 'Third', '%s', 'First']}, {priority: 'event'})"
      ),
      added
    )
  )
  wait_tabs("line.firstElementChild.dataset.viewId === 'Second'")

  expect_identical(tabs()$ids, c("Second", "Third", added, "First"))

  app$run_js(
    paste0(
      "Shiny.setInputValue('my_board-view_nav_remove', 'Third', ",
      "{priority: 'event'})"
    )
  )
  wait_tabs("line.children.length === 3")

  expect_identical(tabs()$ids, c("Second", added, "First"))
  expect_identical(tabs()$ids, read_view_nav(app)$id)
})
