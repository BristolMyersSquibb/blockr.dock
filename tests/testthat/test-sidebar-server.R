# A sidebar panel talks to the root session in both directions: a message out
# to the panel's input binding, the panel's own value echoed back in. Both
# ends are stubbed here so the payload a show ships and the state a handler
# reads can be asserted without a browser; the round trip itself is covered
# end-to-end at the bottom of this file. `ns` stands in for the writing
# module, which is where the owner stamp is read from. `panels` is the root
# input the echo comes back in: a single panel for the handlers that target
# one, several (keyed by the ids a board mounts) for the ownership query,
# which reads across them.
fake_sidebar_session <- function(state = NULL,
                                 ns = "my_board-edit_stack_action",
                                 panels = list(panel = state)) {

  sent <- new.env(parent = emptyenv())
  sent$msgs <- list()

  root <- list(
    input = panels,
    sendInputMessage = function(id, message) {
      sent$msgs <- c(sent$msgs, list(list(id = id, message = message)))
      invisible(NULL)
    }
  )

  list(
    ns = NS(ns),
    rootScope = function() root,
    messages = function() sent$msgs
  )
}

test_that("a show stamps the writing module as the panel's owner", {
  session <- fake_sidebar_session()

  show_sidebar("panel", title = "Edit stack s1", session = session)

  msgs <- session$messages()

  expect_length(msgs, 1L)
  expect_identical(msgs[[1L]][["id"]], "panel")
  expect_identical(
    msgs[[1L]][["message"]][["owner"]], "my_board-edit_stack_action"
  )
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

  echoed <- list(
    open = TRUE, pinned = TRUE, owner = "my_board-add_link_action"
  )

  expect_identical(
    sidebar_state("panel", session = fake_sidebar_session(echoed)),
    echoed
  )
})

test_that("ownership requires an open panel and a matching stamp", {
  # Asked from `my_board-edit_stack_action`, the fake session's module.
  owns <- function(...) {
    owns_open_sidebar("panel", session = fake_sidebar_session(list(...)))
  }

  expect_true(owns(open = TRUE, owner = "my_board-edit_stack_action"))
  expect_false(owns(open = TRUE, owner = "my_board-add_link_action"))
  expect_false(owns(open = FALSE, owner = "my_board-edit_stack_action"))
  expect_false(owns(open = TRUE))
})

test_that("a query reports the panel an action currently holds", {

  session <- fake_sidebar_session(
    panels = list(
      `my_board-actions_sidebar` = list(
        open = TRUE, pinned = TRUE, owner = "my_board-edit_stack_action"
      ),
      `my_board-add_block_sidebar` = list(
        open = FALSE, pinned = FALSE, owner = "my_board-add_block_action"
      )
    )
  )

  owned_by <- function(action, board_id = "my_board") {
    sidebar_owned_by(action, board_id, session = session)
  }

  expect_identical(
    owned_by("edit_stack_action"),
    list(panel = "my_board-actions_sidebar", open = TRUE, pinned = TRUE)
  )

  # A panel it wrote and left closed is still its own: ownership is the stamp,
  # the flags are what the consumer gates on.
  expect_identical(
    owned_by("add_block_action"),
    list(panel = "my_board-add_block_sidebar", open = FALSE, pinned = FALSE)
  )

  expect_null(owned_by("add_link_action"))

  # Keyed by board as well: the same action id on another board matches
  # nothing, which is what composing the stamp here buys.
  expect_null(owned_by("edit_stack_action", "other_board"))
})

test_that("the query covers every sidebar a board mounts", {
  # Both sides compose from `board_sidebar_ids()`, so a panel added to the
  # mounts is one the query already scans -- assert they cannot drift apart.
  html <- xml2::read_html(
    as.character(board_ui("my_board", new_dock_board()))
  )
  panels <- xml2::xml_find_all(
    html,
    paste0(
      "//div[contains(concat(' ', normalize-space(@class), ' '),",
      " ' blockr-sidebar ')]"
    )
  )

  expect_setequal(
    xml2::xml_attr(panels, "id"),
    NS("my_board", board_sidebar_ids())
  )
})

test_that("re-showing a pinned panel restamps its owner", {
  session <- fake_sidebar_session(
    list(open = TRUE, pinned = TRUE, owner = "my_board-edit_stack_action")
  )

  keep_or_hide_sidebar(
    "panel",
    ui = NULL,
    title = "Edit stack s1",
    session = session
  )

  msg <- session$messages()[[1L]][["message"]]

  expect_identical(msg[["action"]], "show")
  expect_identical(msg[["owner"]], "my_board-edit_stack_action")
})

# The stamp is only useful if it survives the trip through the browser: R
# ships it with the body swap, the binding parks it on the panel and reports
# it back in the panel's value, and the query has to compose an id that
# matches what came back. No unit test sees that seam, and a consumer reading
# a stamp that never arrives cannot tell the difference between "no owner"
# and "the mechanism is dead", so drive it through a real app. The fixture
# fires the actions straight off the trigger bundle, which is the path a
# consumer's context menu takes and the one that declares nothing; it reports
# what the query says for the action about to fire, read just before it does.
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

  # The fixture exports the query result from its extension module, so the
  # export name is namespaced the same way the module's inputs are.
  owned <- function() app$get_value(export = "my_board-ext_fire-owned")

  expect_null(app$get_value(input = panel)$owner)
  expect_true(is.na(stamped_owner()))
  expect_null(owned())

  # The stamp is the writing module's namespaced id, which for a board action
  # is `NS(<board id>, <action id>)` -- the composition `sidebar_owned_by()`
  # makes so that a consumer does not have to.
  app$click("my_board-ext_fire-add_link")
  app$wait_for_idle()

  expect_identical(
    app$get_value(input = panel)$owner, "my_board-add_link_action"
  )
  expect_identical(stamped_owner(), "my_board-add_link_action")

  # Pin it, so the panel survives the clicks that follow and the flag a
  # re-target gates on is the one the query has to carry back.
  app$click(selector = paste0("#", panel, " .blockr-sidebar-pin"))
  app$wait_for_idle()

  expect_true(app$get_value(input = panel)$pinned)

  app$click("my_board-ext_fire-add_link")
  app$wait_for_idle()

  expect_identical(owned(), list(panel = panel, open = TRUE, pinned = TRUE))

  # A second surface fills the same panel. Nothing declared which panel it
  # writes, so a stale stamp here is what a consumer would re-fire into --
  # and the query says so: at the moment it fires, edit_stack holds nothing.
  app$click("my_board-ext_fire-edit_stack")
  app$wait_for_idle()

  expect_identical(
    app$get_value(input = panel)$owner, "my_board-edit_stack_action"
  )
  expect_identical(stamped_owner(), "my_board-edit_stack_action")
  expect_null(owned())

  # Ownership moved with the write, so the query follows it: edit_stack now
  # answers for the panel...
  app$click("my_board-ext_fire-edit_stack")
  app$wait_for_idle()

  expect_identical(owned(), list(panel = panel, open = TRUE, pinned = TRUE))

  # ... and add_link, which filled the very same panel earlier, no longer
  # does. That flip is what a consumer re-fires (or does not) on.
  app$click("my_board-ext_fire-add_link")
  app$wait_for_idle()

  expect_null(owned())
})
