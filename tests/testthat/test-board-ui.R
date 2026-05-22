test_that("dummy board ui test", {

  ui <- board_ui(
    "test",
    new_dock_board(blocks = c(a = new_dataset_block()))
  )

  expect_s3_class(ui, "shiny.tag.list")
  expect_length(ui, 8L)
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
