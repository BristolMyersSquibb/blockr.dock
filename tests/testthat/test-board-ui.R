test_that("dummy board ui test", {

  ui <- board_ui(
    "test",
    new_dock_board(blocks = c(a = new_dataset_block()))
  )

  expect_s3_class(ui, "shiny.tag.list")
  # 15 base elements (blockr.ui's controls, the rename handler, the "+" menu
  # and the compact switch among them) + the viewport probe.
  expect_length(ui, 16L)
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

test_that("the chrome's tooltips are written for Blockr.tooltip (#494)", {

  brd <- new_dock_board(blocks = c(a = new_dataset_block()))

  render <- function(locked) {
    withr::with_options(
      list(blockr.locked = locked),
      xml2::read_html(as.character(board_ui("test", brd)))
    )
  }

  chrome <- paste0(
    "//*[", has_class("blockr-block-header"), " or ",
    has_class("blockr-navbar"), " or ", has_class("blockr-sidebar"), "]"
  )

  tooltips <- function(doc) {

    # No native title is left for the browser to show in place of the card.
    expect_length(
      xml2::xml_find_all(doc, paste0(chrome, "/descendant-or-self::*[@title]")),
      0L
    )

    # The card only describes, and only while it shows, so an element with
    # no text of its own has its name in an aria-label.
    tips <- xml2::xml_find_all(doc, "//*[@data-blockr-tooltip]")
    bare <- tips[!nzchar(trimws(xml2::xml_text(tips)))]
    expect_false(anyNA(xml2::xml_attr(bare, "aria-label")))

    xml2::xml_attr(tips, "data-blockr-tooltip")
  }

  unlocked <- c(
    "dataset block", "Preview", "More actions", "Remove page",
    "Board options", "Back", "Pin", "Close"
  )
  expect_true(all(unlocked %in% tooltips(render(NULL))))

  expect_true(
    "Editing is disabled by this deployment." %in% tooltips(render(TRUE))
  )
})

test_that("the logo leads the navbar as its busy indicator (#530)", {

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

  doc <- xml2::read_html(as.character(board_ui("test", brd)))
  logo <- by_class(doc, "blockr-navbar-logo")

  # The logo is the first item of the bar.
  expect_length(logo, 1)
  item <- xml2::xml_parent(logo[[1]])
  expect_identical(xml2::xml_attr(item, "data-navbar-item"), "logo")
  expect_length(xml2::xml_find_all(item, "preceding-sibling::*"), 0L)

  # Seven squares, each carrying its place in the order the R is drawn, on
  # which the busy animation staggers them. The drawing itself is hidden from
  # screen readers.
  svg <- xml2::xml_find_first(logo[[1]], ".//svg")
  expect_identical(xml2::xml_attr(svg, "aria-hidden"), "true")
  expect_identical(
    xml2::xml_attr(xml2::xml_find_all(svg, ".//rect"), "style"),
    paste0("--i:", 0:6)
  )

  # Announced like the lock indicator, by a status only screen readers see.
  status <- xml2::xml_find_all(logo[[1]], ".//*[@role='status']")
  expect_length(status, 1)
  expect_identical(xml2::xml_attr(status, "aria-label"), "Busy")
  expect_identical(xml2::xml_attr(status, "class"), "visually-hidden")

  # Blocks still evaluate while read-only, so the busy indicator survives
  # locked mode (unlike the editing chrome around it).
  locked <- withr::with_options(
    list(blockr.locked = TRUE),
    by_class(
      xml2::read_html(as.character(board_ui("test", brd))),
      "blockr-navbar-logo"
    )
  )
  expect_length(locked, 1)
})

test_that("the logo is whole when idle and still moves under reduced motion", {

  # Stylesheet assertions, because nothing else can catch these: the logo is
  # only ever seen animating mid-flush, and CI never runs with the preference
  # set. Idle, the logo must be whole, so the animation belongs on the busy
  # selector, not the base rule, and it waits the display delay. When the
  # reduced-motion block killed the old ring's animation outright, the ring
  # sat frozen -- indistinguishable from a hung session -- for every user
  # whose OS reports the preference (Windows does so whenever "Animation
  # effects" is off). Slower is fine here; stopped is not.
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

  base <- regmatches(
    css,
    regexpr(
      "(?m)^\\.blockr-navbar-logo \\.blockr-logo rect \\{[^}]*\\}",
      css,
      perl = TRUE
    )
  )

  busy <- regmatches(
    css,
    regexpr(
      "(?m)^html\\.shiny-busy:has[^{]*\\.blockr-logo rect \\{[^}]*\\}",
      css,
      perl = TRUE
    )
  )

  expect_length(base, 1L)
  expect_no_match(base, "animation")
  expect_length(busy, 1L)
  expect_match(busy, "animation: blockr-logo-fill", fixed = TRUE)
  expect_match(busy, "var(--blockr-spinner-delay, 0ms)", fixed = TRUE)

  reduced <- regmatches(
    css,
    regexpr(
      "(?s)@media \\(prefers-reduced-motion: reduce\\).*?\\n\\}",
      css,
      perl = TRUE
    )
  )

  expect_length(reduced, 1L)

  # Must slow the busy selector that carries the animation; on the bare
  # `.blockr-logo rect` the override is outspecified and does nothing.
  expect_match(reduced, "html.shiny-busy:has", fixed = TRUE)
  expect_match(reduced, "animation-duration: 3.9s", fixed = TRUE)
  expect_no_match(reduced, "animation:\\s*none")
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
  # the logo's busy animation waits.
  expect_match(
    navbar_style(list(blockr.spinner_delay_ms = NULL)),
    "--blockr-spinner-delay: 200ms",
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
    "--blockr-spinner-delay: 200ms",
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
  # The one exception is the card's inset, which blockr.ui's table preview
  # reads to run a table to the card edges.
  expect_no_match(
    css, "(?m)^\\s*--blockr-(?!card-inset\\b)[a-z0-9-]+\\s*:", perl = TRUE
  )

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
