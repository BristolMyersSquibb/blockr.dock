# A sidebar panel talks to the root session in both directions: a message out
# to the panel's input binding, the panel's own value echoed back in. Both
# ends are stubbed here so the payload a show ships and the state a handler
# reads can be asserted without a browser; the round trip itself is covered
# end-to-end at the bottom of this file.
fake_sidebar_session <- function(state = NULL) {

  sent <- new.env(parent = emptyenv())
  sent$msgs <- list()

  root <- list(
    input = list(panel = state),
    sendInputMessage = function(id, message) {
      sent$msgs <- c(sent$msgs, list(list(id = id, message = message)))
      invisible(NULL)
    }
  )

  list(rootScope = function() root, messages = function() sent$msgs)
}

test_that("a show stamps the writing action as the panel's owner", {
  session <- fake_sidebar_session()

  show_sidebar(
    "panel",
    title = "Edit stack s1",
    owner = "edit_stack_action",
    session = session
  )

  msgs <- session$messages()

  expect_length(msgs, 1L)
  expect_identical(msgs[[1L]][["id"]], "panel")
  expect_identical(msgs[[1L]][["message"]][["owner"]], "edit_stack_action")
})

test_that("a show without an owner clears the stamp", {
  # Ownership rides on every show, so a writer that does not declare itself
  # takes the previous owner off the panel rather than leaving it stale.
  session <- fake_sidebar_session()

  show_sidebar("panel", session = session)

  msg <- session$messages()[[1L]][["message"]]

  expect_true("owner" %in% names(msg))
  expect_null(msg[["owner"]])
})

test_that("a hide leaves the stamp alone", {
  # The body survives a close, so what is in the panel is still whoever last
  # wrote it. Only a show can change that.
  session <- fake_sidebar_session()

  hide_sidebar("panel", session = session)

  expect_false("owner" %in% names(session$messages()[[1L]][["message"]]))
})

test_that("panel state reports the owner beside open and pinned", {
  expect_identical(
    sidebar_state("panel", session = fake_sidebar_session()),
    list(open = FALSE, pinned = FALSE, owner = NULL)
  )

  echoed <- list(open = TRUE, pinned = TRUE, owner = "add_link_action")

  expect_identical(
    sidebar_state("panel", session = fake_sidebar_session(echoed)),
    echoed
  )
})

test_that("ownership requires an open panel and a matching stamp", {
  owns <- function(...) {
    owns_open_sidebar(
      "panel", "edit_stack_action",
      session = fake_sidebar_session(list(...))
    )
  }

  expect_true(owns(open = TRUE, owner = "edit_stack_action"))
  expect_false(owns(open = TRUE, owner = "add_link_action"))
  expect_false(owns(open = FALSE, owner = "edit_stack_action"))
  expect_false(owns(open = TRUE))
})

test_that("re-showing a pinned panel restamps its owner", {
  session <- fake_sidebar_session(
    list(open = TRUE, pinned = TRUE, owner = "edit_stack_action")
  )

  keep_or_hide_sidebar(
    "panel",
    ui = NULL,
    title = "Edit stack s1",
    owner = "edit_stack_action",
    session = session
  )

  msg <- session$messages()[[1L]][["message"]]

  expect_identical(msg[["action"]], "show")
  expect_identical(msg[["owner"]], "edit_stack_action")
})

# The stamp is only useful if it survives the trip through the browser: R
# ships it with the body swap, the binding parks it on the panel and reports
# it back in the panel's value. No unit test sees that seam, and a consumer
# reading a stamp that never arrives cannot tell the difference between "no
# owner" and "the mechanism is dead", so drive it through a real app. The
# fixture fires the actions straight off the trigger bundle, which is the
# path a consumer's context menu takes and the one that declares nothing.
test_that("a panel reports the action that wrote its body", {

  skip_on_cran()

  app <- new_app_driver(
    system.file("examples", "sidebar-owner", "app.R", package = "blockr.dock"),
    name = "sidebar-owner",
    seed = 42,
    load_timeout = 30 * 1000,
    timeout = 30 * 1000
  )
  withr::defer(app$stop())

  app$wait_for_idle()

  panel <- "my_board-actions_sidebar"

  stamped_owner <- function() {
    xml2::xml_attr(
      xml2::xml_find_first(
        xml2::read_html(app$get_html(paste0("#", panel))),
        paste0("//div[@id='", panel, "']")
      ),
      "data-blockr-sidebar-owner"
    )
  }

  expect_null(app$get_value(input = panel)$owner)
  expect_true(is.na(stamped_owner()))

  app$click("my_board-ext_fire-add_link")
  app$wait_for_idle()

  expect_identical(app$get_value(input = panel)$owner, "add_link_action")
  expect_identical(stamped_owner(), "add_link_action")

  # A second surface fills the same panel. Nothing declared which panel it
  # writes, so a stale stamp here is what a consumer would re-fire into.
  app$click("my_board-ext_fire-edit_stack")
  app$wait_for_idle()

  expect_identical(app$get_value(input = panel)$owner, "edit_stack_action")
  expect_identical(stamped_owner(), "edit_stack_action")
})
