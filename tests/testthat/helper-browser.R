# What the end-to-end tests use.
# They drive Chrome through shinytest2.

# Starts an example app from `inst/examples`, by its name, with shinytest2, and
# stops it as the test ends.
# The example's script attaches blockr.dock with `library()`.
# The tests may run from the package's source, as `devtools::test()` runs
# them.
# Then shinytest2 has that call load the source with `pkgload::load_all()`.
# So the app runs the dock under test, and not an older install.
# Under `R CMD check`, the app runs the package that the check installed.
local_example_app <- function(name, frame = parent.frame()) {

  testthat::skip_on_cran()
  testthat::skip_if_not_installed("shinytest2")

  chrome <- try(chromote::default_chromote_object(), silent = TRUE)
  testthat::skip_if(inherits(chrome, "try-error"), "Chrome does not start.")

  app <- shinytest2::AppDriver$new(
    system.file("examples", name, "app.R", package = "blockr.dock"),
    load_timeout = 30 * 1000,
    timeout = 30 * 1000
  )

  withr::defer(app$stop(), envir = frame)

  app
}

# Waits until the page holds what the dock's frontend draws: a dockview, in
# the dock's element.
# After `timeout` seconds without it, it fails with the page's text.
# That text shows what the page holds in place of the dock, such as core's
# basic frontend or core's error.
wait_for_dock <- function(app, timeout = 30) {

  drawn <- try(
    app$wait_for_js(
      "document.querySelector('.blockr-dock .dv-shell') !== null",
      timeout = timeout * 1000
    ),
    silent = TRUE
  )

  if (inherits(drawn, "try-error")) {
    stop(
      "The dock drew no page within ", timeout, " seconds. The page shows:\n",
      app$get_js("document.body.innerText"),
      call. = FALSE
    )
  }

  invisible(app)
}
