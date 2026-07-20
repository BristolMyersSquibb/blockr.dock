test_that("dummy board ui test", {

  ui <- board_ui(
    "test",
    new_dock_board(blocks = c(a = new_dataset_block()))
  )

  expect_s3_class(ui, "shiny.tag.list")
  # 8 base elements + the two pre-rendered block-browser sidebars
  # (add_block_sidebar, append_block_sidebar).
  expect_length(ui, 10L)
})

# Settings sidebar is mounted with pre-rendered content + a JS-trigger gear
# button (PR #2 amendment: static-content sidebar). The chain that used to
# live server-side (settings_observer → show_sidebar → settings_body) is
# replaced by markup, so the assertions move to the rendered tag tree.

test_that("settings sidebar mount is pre-rendered with the options accordion", {
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

  # Its body slot carries the rendered options accordion. We probe by
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

test_that("navbar renders a busy spinner beside the gear (#345)", {

  brd <- new_dock_board(blocks = c(a = new_dataset_block()))

  spinner_of <- function(html) {
    xml2::xml_find_all(
      xml2::read_html(html),
      paste0(
        ".//span[contains(concat(' ', normalize-space(@class), ' '), ",
        "' blockr-navbar-spinner ')]"
      )
    )
  }

  spinner <- spinner_of(as.character(board_ui("test", brd)))

  # A CSS-only busy ring driven off `.shiny-busy`, announced like the lock
  # indicator beside it.
  expect_length(spinner, 1)
  expect_identical(xml2::xml_attr(spinner, "role"), "status")
  expect_identical(xml2::xml_attr(spinner, "aria-label"), "Busy")

  # Blocks still evaluate while read-only, so the spinner survives locked mode
  # (unlike the editing chrome it sits beside).
  locked <- withr::with_options(
    list(blockr.locked = TRUE),
    spinner_of(as.character(board_ui("test", brd)))
  )
  expect_length(locked, 1)
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
