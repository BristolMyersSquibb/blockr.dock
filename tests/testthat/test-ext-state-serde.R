# Extension state used to vanish on save. `serialize_board.dock_board()`
# mapped over `list(...)`, but the extensions arrive as ONE named plugin
# argument holding all of them -- board_server returns `extensions = ext_res`
# and core flattens that into the plugin args by a single level, so `...`
# carries `actions` and `extensions`, not `dag` and `outline`. Every
# `x[["state"]]` was therefore NULL and each extension wrote an empty payload
# while still writing its object and constructor, so a saved board looked
# complete and carried none of its extensions' state.

fake_ext <- function(name) {
  new_dock_extension(
    server = function(id, ...) function(input, output, session) list(),
    ui = function(id) shiny::div(),
    name = name,
    class = paste0(name, "_extension")
  )
}

ser_with <- function(board, ...) {
  serialize_board(
    board,
    blocks = list(), id = NULL, dock = NULL, view_data = NULL,
    ..., session = NULL
  )
}

ext_payload <- function(ser, name) {
  ser[["payload"]][["extensions"]][["payload"]][[name]][["payload"]]
}

test_that("extension state reaches the serialized board", {
  board <- new_dock_board(
    blocks = c(a = new_dataset_block("iris")),
    extensions = list(notes = fake_ext("notes"))
  )

  ser <- ser_with(
    board,
    actions = list(),
    extensions = list(notes = list(state = list(content = "hello", n = 3L)))
  )

  expect_identical(ext_payload(ser, "notes"), list(content = "hello", n = 3L))
})

test_that("a FALSE in extension state survives, rather than defaulting back", {
  # The shape that made this visible: an outline where one block is excluded
  # from the report. Losing the payload restored every block to the
  # constructor default, which is "included".
  board <- new_dock_board(
    blocks = c(a = new_dataset_block("iris")),
    extensions = list(notes = fake_ext("notes"))
  )

  ser <- ser_with(
    board,
    actions = list(),
    extensions = list(notes = list(state = list(
      flags = list(a = TRUE, b = FALSE)
    )))
  )

  expect_identical(
    ext_payload(ser, "notes")$flags,
    list(a = TRUE, b = FALSE)
  )
})

test_that("reactive extension state is evaluated, not stored as a closure", {
  board <- new_dock_board(
    blocks = c(a = new_dataset_block("iris")),
    extensions = list(notes = fake_ext("notes"))
  )

  shiny::isolate({
    rv <- shiny::reactiveVal("live value")
    ser <- ser_with(
      board,
      actions = list(),
      extensions = list(notes = list(state = list(content = rv)))
    )
    expect_identical(ext_payload(ser, "notes"), list(content = "live value"))
  })
})

test_that("other plugin arguments are not mistaken for extensions", {
  # `actions` sits in the same `...`; walking it as though it were an
  # extension is what produced the empty payloads.
  board <- new_dock_board(
    blocks = c(a = new_dataset_block("iris")),
    extensions = list(notes = fake_ext("notes"))
  )

  ser <- ser_with(
    board,
    actions = list(state = list(should = "not appear")),
    extensions = list(notes = list(state = list(content = "mine")))
  )

  expect_identical(ext_payload(ser, "notes"), list(content = "mine"))
  expect_null(ser[["payload"]][["extensions"]][["payload"]][["actions"]])
})

test_that("an extension with no state still serializes its constructor", {
  board <- new_dock_board(
    blocks = c(a = new_dataset_block("iris")),
    extensions = list(notes = fake_ext("notes"))
  )

  ser <- ser_with(board, actions = list(), extensions = list())

  node <- ser[["payload"]][["extensions"]][["payload"]][["notes"]]
  expect_length(node[["payload"]], 0L)
  expect_false(is.null(node[["constructor"]]))
})
