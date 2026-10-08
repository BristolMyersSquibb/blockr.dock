# A sidebar panel talks to the root session in both directions: a message out
# to the panel's input binding, the panel's own value echoed back in. Both
# ends are stubbed here so the payload a show ships and the state a handler
# reads can be asserted without a browser; the round trip itself is covered
# end-to-end at the bottom of this file. `ns` stands in for the writing
# module, which is where the owner stamp is read from. `inputs` is the root
# input the echo comes back in: a single panel for the handlers that target
# one by id, or a whole input set for the ownership query, which searches it.
fake_sidebar_session <- function(state = NULL,
                                 ns = "my_board-edit_stack_action",
                                 inputs = list(panel = state)) {

  sent <- new.env(parent = emptyenv())
  sent$msgs <- list()

  root <- list(
    input = do.call(reactiveValues, inputs),
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
    open = TRUE, pinned = TRUE, owner = "my_board-edit_inputs_action"
  )

  expect_identical(
    sidebar_state("panel", session = fake_sidebar_session(echoed)),
    echoed
  )
})
test_that("a query reports the panel an action currently holds", {

  session <- fake_sidebar_session(
    inputs = list(
      # A plain input, and one whose value is the very string a stamp would
      # be: only a panel's value can answer for a panel. Sorts ahead of the
      # panels, so a query that took it would be caught here.
      `my_board-a_text_input` = "my_board-edit_stack_action",
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

test_that("a panel outside the board's own mounts answers the same way", {
  # An extension mounting its own sidebar and writing to it from its module.
  # The panel is not one `board_ui()` places and its id follows no scheme the
  # query knows, but the stamp is composed the same way, so it is found the
  # same way -- the set of panels is not something to enumerate.
  session <- fake_sidebar_session(
    inputs = list(
      `my_board-ext_notes-scratch` = list(
        open = TRUE, pinned = FALSE, owner = "my_board-ext_notes"
      )
    )
  )

  expect_identical(
    sidebar_owned_by("ext_notes", "my_board", session = session),
    list(panel = "my_board-ext_notes-scratch", open = TRUE, pinned = FALSE)
  )
})

