test_that("dummy board ui test", {

  ui <- board_ui(
    "test",
    new_dock_board(blocks = c(a = new_dataset_block()))
  )

  expect_s3_class(ui, "shiny.tag.list")
  # 18 base elements (blockr.ui's controls, the dock's tooltip hand-off, the
  # section tracker, the "+" menu, the compact switch, the view-tabs script
  # and the font switch among them) + the viewport probe + the restore-probe
  # slot. The probe slot is empty at this log level; `tagList()` keeps the
  # NULL and htmltools drops it at render.
  expect_length(ui, 20L)
})

test_that("the restore probe ships only under debug logging (#473)", {

  board <- new_dock_board(blocks = c(a = new_dataset_block()))

  deps <- function() {
    chr_xtr(htmltools::findDependencies(board_ui("test", board)), "name")
  }

  # A diagnostic that reaches a deployment is a diagnostic nobody asked for, so
  # the gate is the dependency itself rather than a dormant script.
  expect_false("blockr-dock-restore-probe" %in% deps())

  withr::local_options(blockr.log_level = "debug")

  expect_true("blockr-dock-restore-probe" %in% deps())
})

# Settings sidebar is mounted with pre-rendered content + a JS-trigger gear
# button (PR #2 amendment: static-content sidebar). The chain that used to
# live server-side (settings_observer → show_sidebar → settings_body) is
# replaced by markup, so the assertions move to the rendered tag tree.

test_that("settings sidebar mount is pre-rendered with the options list", {
  ui <- board_ui(
    "test",
    new_dock_board(blocks = c(a = new_dataset_block()))
  )
  html <- as.character(ui)

  # The settings sidebar exists with the namespaced DOM id and overlay
  # mode. The id is `NS("test", "settings_sidebar")` so two boards on
  # the same page don't collide.
  expect_match(html, 'id="test-settings_sidebar"', fixed = TRUE)
  expect_match(html, 'data-mode="overlay"', fixed = TRUE)

  # Its body slot carries the rendered options list and pages. We probe by
  # looking for the inputId of the default `board_name` option, which is
  # always present (contributed by blockr.core for any board).
  expect_match(html, 'id="test-board_name"', fixed = TRUE)

  # And the panel's title slot is populated at UI-build time.
  expect_match(
    html,
    '<h2 class="blockr-sidebar-title">Board options</h2>',
    fixed = TRUE
  )
})

test_that("gear button targets the settings sidebar via data-attribute", {
  ui <- board_ui(
    "test",
    new_dock_board(blocks = c(a = new_dataset_block()))
  )
  html <- as.character(ui)

  # The gear opens the settings sidebar purely client-side; no Shiny input
  # is wired, so no `id="test-settings_btn"` should appear.
  expect_match(
    html,
    'data-blockr-sidebar-target="test-settings_sidebar"',
    fixed = TRUE
  )
  expect_false(grepl('id="test-settings_btn"', html, fixed = TRUE))
})

test_that("caller-supplied `options` flows into the rendered settings body", {
  # Simulate `serve(board, options = custom_options(...))`: replace the
  # default `board_options` with a single synthetic option carrying a
  # unique id we can grep for.
  marker_id <- "marker_opt_for_test"
  custom <- blockr.core::as_board_options(
    list(
      blockr.core::new_board_option(
        id = marker_id,
        default = "hi",
        ui = function(id) {
          shiny::textInput(NS(id, marker_id), label = "Marker")
        },
        category = "custom",
        # `ctor` defaults to `sys.parent()`, which tries to resolve the
        # package of the calling closure. In a test scope that closure has
        # no namespace, so pass an explicit `pkg::fn` identifier instead.
        ctor = "blockr.core::new_board_option"
      )
    )
  )

  ui <- board_ui(
    "test",
    new_dock_board(blocks = c(a = new_dataset_block())),
    options = custom
  )
  html <- as.character(ui)

  # The custom option appears.
  expect_match(html, sprintf('id="test-%s"', marker_id), fixed = TRUE)
  # The default options that would have been recomputed are NOT
  # present — the override fully replaces the set.
  expect_false(grepl('id="test-board_name"', html, fixed = TRUE))
})

test_that("board_ui builds only the active view's block cards", {

  # The offcanvas mount carries an edit card for every block on screen at
  # startup, but not for blocks that live only in an off-screen view -- those
  # are inserted on first visit. This keeps first paint proportional to the
  # active view, not the whole board.
  brd <- new_dock_board(
    blocks = c(
      a = new_dataset_block(), b = new_head_block(), c = new_head_block()
    ),
    views = list(A = c("a", "b"), B = "c"),
    active = "A"
  )

  html <- as.character(board_ui("test", brd))

  expect_match(html, 'id="test-block_handle-a"', fixed = TRUE)
  expect_match(html, 'id="test-block_handle-b"', fixed = TRUE)
  expect_false(grepl('id="test-block_handle-c"', html, fixed = TRUE))
})

test_that("locked mode renders a navbar lock indicator", {

  brd <- new_dock_board(blocks = c(a = new_dataset_block()))

  unlocked_html <- withr::with_options(
    list(blockr.locked = NULL),
    as.character(board_ui("test", brd))
  )
  expect_false(grepl("blockr-lock-indicator", unlocked_html, fixed = TRUE))

  locked_html <- withr::with_options(
    list(blockr.locked = TRUE),
    as.character(board_ui("test", brd))
  )
  expect_match(locked_html, "blockr-lock-indicator", fixed = TRUE)
  expect_match(locked_html, 'role="status"', fixed = TRUE)
  expect_match(locked_html, "blockr-lock-indicator-label", fixed = TRUE)
  # Visible label, not just the aria-label / tooltip.
  expect_match(locked_html, ">Read-only<", fixed = TRUE)
})

dock_css <- function() {
  paste(
    readLines(
      system.file(
        "assets", "css", "blockr-dock.css",
        package = "blockr.dock",
        mustWork = TRUE
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )
}

test_that("the navbar leads with the blockr mark, with or without a menu", {

  brd <- new_dock_board(blocks = c(a = new_dataset_block()))

  by_class <- function(node, token) {
    xml2::xml_find_all(
      node,
      paste0(
        ".//*[contains(concat(' ', normalize-space(@class), ' '), ' ",
        token,
        " ')]"
      )
    )
  }

  # The default plugins offer no brand menu
  doc <- xml2::read_html(as.character(board_ui("test", brd)))
  brand <- by_class(doc, "blockr-navbar-brand")

  # One mark, in the brand slot, with the seven squares of the logo in stroke
  # order (the busy animation staggers on `--i`), and the status region the
  # old ring carried.
  expect_length(brand, 1)
  expect_identical(xml2::xml_attr(brand, "data-navbar-slot"), "brand")
  rects <- xml2::xml_find_all(brand[[1]], ".//*[local-name()='rect']")
  expect_length(rects, 7)
  expect_identical(
    xml2::xml_attr(rects, "style"),
    sprintf("--i:%d", 0:6)
  )
  status <- xml2::xml_find_all(brand[[1]], ".//*[@role='status']")
  expect_identical(xml2::xml_attr(status, "aria-label"), "Busy")

  # No plugin offers a menu, so the mark is not a button
  expect_length(xml2::xml_find_all(brand[[1]], ".//button"), 0)

  # Blocks still evaluate while read-only, so the mark keeps its place
  locked <- withr::with_options(
    list(blockr.locked = TRUE),
    by_class(
      xml2::read_html(as.character(board_ui("test", brd))),
      "blockr-navbar-brand"
    )
  )
  expect_length(locked, 1)
})

test_that("a plugin's brand menu moves under the mark", {

  ui <- tagList(
    div(
      class = "plugin-bar",
      div(class = "dropdown-menu blockr-navbar-brand-menu", "Workflows"),
      span(class = "plugin-name", "AE review")
    )
  )

  parts <- split_brand_menu(ui)
  expect_match(as.character(parts$menu), "Workflows", fixed = TRUE)
  expect_no_match(as.character(parts$rest), "blockr-navbar-brand-menu")
  expect_match(as.character(parts$rest), "plugin-name", fixed = TRUE)

  brand <- as.character(navbar_brand_ui(parts$menu))
  expect_match(brand, 'data-bs-toggle="dropdown"', fixed = TRUE)
  expect_match(brand, "Workflows", fixed = TRUE)

  # Without such an element the plugin's UI passes through untouched
  plain <- split_brand_menu(div(class = "plugin-bar"))
  expect_null(plain$menu)
  expect_match(as.character(plain$rest), "plugin-bar", fixed = TRUE)
})

test_that("the busy mark still moves under reduced motion", {

  # A stylesheet assertion because nothing else can catch this: the busy state
  # is only ever seen mid-flush, and CI never runs with the preference set. A
  # still mark during a long computation reads as a hung session, and Windows
  # reports the preference whenever "Animation effects" is off. Slower is
  # fine; stopped is not.
  css <- dock_css()

  reduced <- regmatches(
    css,
    regexpr(
      "(?s)@media \\(prefers-reduced-motion: reduce\\).*?\\n\\}",
      css,
      perl = TRUE
    )
  )

  expect_length(reduced, 1L)
  expect_match(reduced, "html.shiny-busy:has", fixed = TRUE)
  expect_match(reduced, "animation-duration: 3.9s", fixed = TRUE)
  expect_no_match(reduced, "animation:\\s*none")
})

test_that("the mark animates only on the busy scope, after the delay", {

  # Idle the mark is the plain logo: no animation on the base rule. The busy
  # rule waits for the display delay, so a flush that clears sooner shows
  # nothing.
  css <- dock_css()

  base <- regmatches(
    css,
    regexpr(
      "(?m)^\\.blockr-navbar-mark \\.blockr-mark rect \\{[^}]*\\}",
      css, perl = TRUE
    )
  )

  busy <- regmatches(
    css,
    regexpr(
      "(?m)^html\\.shiny-busy:has[^{]*\\.blockr-mark rect \\{[^}]*\\}",
      css, perl = TRUE
    )
  )

  expect_length(base, 1L)
  expect_no_match(base, "animation")
  expect_match(busy, "animation: blockr-mark-fill", fixed = TRUE)
  expect_match(busy, "var(--blockr-spinner-delay", fixed = TRUE)
})

test_that("the busy mark names its state on hover", {

  css <- dock_css()

  label <- regmatches(
    css,
    regexpr("(?m)^\\.blockr-navbar-brand::after \\{[^}]*\\}", css, perl = TRUE)
  )

  shown <- regmatches(
    css,
    regexpr(
      "(?m)^html\\.shiny-busy:has[^{]*\\.blockr-navbar-brand:hover::after \\{[^}]*\\}",
      css, perl = TRUE
    )
  )

  expect_match(label, "content: \"Computing\"", fixed = TRUE)
  expect_match(label, "opacity: 0", fixed = TRUE)
  expect_match(shown, "opacity: 1", fixed = TRUE)
})

test_that("navbar carries the spinner display delay from the option (#355)", {

  brd <- new_dock_board(blocks = c(a = new_dataset_block()))

  navbar_style <- function(opts) {
    html <- withr::with_options(opts, as.character(board_ui("test", brd)))
    navbar <- xml2::xml_find_first(
      xml2::read_html(html),
      paste0(
        ".//div[contains(concat(' ', normalize-space(@class), ' '), ",
        "' blockr-navbar ')]"
      )
    )
    xml2::xml_attr(navbar, "style")
  }

  # A 200 ms minimum-busy delay by default, fed into the CSS custom property
  # the spinner's show transition reads.
  expect_match(
    navbar_style(list(blockr.spinner_delay_ms = NULL)),
    "--blockr-spinner-delay: 500ms",
    fixed = TRUE
  )

  # A caller-set delay flows through as-is; 0 restores immediate display.
  expect_match(
    navbar_style(list(blockr.spinner_delay_ms = 50L)),
    "--blockr-spinner-delay: 50ms",
    fixed = TRUE
  )
  expect_match(
    navbar_style(list(blockr.spinner_delay_ms = 0L)),
    "--blockr-spinner-delay: 0ms",
    fixed = TRUE
  )

  # A nonsense value falls back to the default rather than emitting broken CSS.
  expect_match(
    navbar_style(list(blockr.spinner_delay_ms = -5L)),
    "--blockr-spinner-delay: 500ms",
    fixed = TRUE
  )
})

test_that("locked mode drops the board-options accordion (#135)", {

  brd <- new_dock_board(blocks = c(a = new_dataset_block()))

  html <- withr::with_options(
    list(blockr.locked = TRUE),
    as.character(board_ui("test", brd))
  )

  # The editable board_name input and the options accordion are gone -- both
  # write board state, which core's gate refuses while locked.
  expect_false(grepl('id="test-board_name"', html, fixed = TRUE))
  expect_false(grepl('id="test-board_options"', html, fixed = TRUE))

  # The read-only generated-code export stays available.
  expect_match(html, 'id="generate_code"', fixed = TRUE)
})

test_that("board_ui mounts the viewport probe with its binding", {

  ui <- board_ui(
    "test",
    new_dock_board(blocks = c(a = new_dataset_block()))
  )

  html <- as.character(ui)

  # The DOM id is namespaced (two boards on one page must not collide) and
  # carries the class the binding finds; the input name is `viewport_width`.
  expect_match(html, 'id="test-viewport_width"', fixed = TRUE)
  expect_match(html, "blockr-viewport-probe", fixed = TRUE)

  expect_true(
    "blockr-viewport-probe" %in%
      chr_xtr(htmltools::findDependencies(ui), "name")
  )
})

test_that("the shared stylesheet layer is blockr.ui's, not this package's", {

  ui <- board_ui(
    "test",
    new_dock_board(blocks = c(a = new_dataset_block()))
  )

  deps <- chr_xtr(htmltools::findDependencies(ui), "name")

  expect_true("blockr-theme" %in% deps)

  # Source order settles which side wins where the two sheets overlap, so the
  # shared layer has to resolve ahead of this package's own.
  expect_lt(match("blockr-theme", deps), match("blockr-fab", deps))

  css <- paste(
    readLines(
      system.file(
        "assets", "css", "blockr-dock.css",
        package = "blockr.dock",
        mustWork = TRUE
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_no_match(css, "(?m)^:root", perl = TRUE)
  expect_no_match(css, "(?m)^\\s*--blockr-[a-z0-9-]+\\s*:", perl = TRUE)

  unscoped <- c(
    "body", "label", "\\.form-control", "\\.btn-primary", "\\.tooltip",
    "\\.popover", "table\\.dataTable", "\\.g6-toolbar"
  )

  expect_no_match(
    css,
    paste0("(?m)^(", paste(unscoped, collapse = "|"), ")[ ,{:]"),
    perl = TRUE
  )
})
