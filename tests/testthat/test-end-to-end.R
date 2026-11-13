# End-to-end tests of the example apps, through shinytest2.
# Core's client and the dock's run on the page in Chrome, and the app runs in
# another R process.

test_that("serve(new_board()) serves a page that the dock draws", {

  # The example passes serve() no plugins, so the dock runs only because
  # blockr.dock lists its plugin class in its `._BLOCKR_MODULES_`.
  app <- local_example_app("empty")

  expect_no_error(wait_for_dock(app))
})
