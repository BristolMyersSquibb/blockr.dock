# End-to-end tests of the example apps, through shinytest2.
# Core's client and the dock's run on the page in Chrome, and the app runs in
# another R process.

test_that("serve() with the dock plugin serves a page that the dock draws", {

  # The example runs the dock plugin alone, as one row of plugins.
  # R accepts that row only where blockr.dock lists the plugin's class in its
  # `._BLOCKR_MODULES_`.
  app <- local_example_app("empty")

  # The dock draws only once its module has imported dockview from the file
  # of the libraries beside it, which R serves with the module.
  expect_no_error(wait_for_dock(app))

  # The dock's stylesheet imports dockview's from a file of the libraries as
  # well.
  # Only dockview's rules place its element `.dv-dockview` relative.
  expect_identical(
    app$get_js(
      "getComputedStyle(document.querySelector('.dv-dockview')).position"
    ),
    "relative"
  )
})
