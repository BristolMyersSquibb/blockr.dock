# The stack menu publishes its committed selection into the parent
# session's namespace as `menu-commit` (list(blocks, nonce)); the
# panel-level form fields are real Shiny inputs at `menu-stack_name`,
# `menu-stack_color`, and `menu-stack_id`. Drive them directly to
# simulate the user creating / editing a stack.
local_mocked_sidebar <- function(env = parent.frame()) {
  local_mocked_bindings(
    show_sidebar         = function(...) invisible(list(...)),
    hide_sidebar         = function(...) invisible(list(...)),
    .env = env
  )
}

set_menu <- function(session, blocks, name, color, id, nonce) {
  session$setInputs(
    `menu-stack_name` = name,
    `menu-stack_color` = color,
    `menu-stack_id` = id,
    `menu-commit` = list(blocks = blocks, nonce = nonce)
  )
}

# The form is server-rendered into the menu's `form` slot, so its current
# state is read off that output rather than off the panel markup.
form_field <- function(html, key) {
  doc <- xml2::read_html(as.character(html))
  node <- xml2::xml_find_first(
    doc, paste0("//input[contains(@id, 'menu-", key, "')]")
  )
  xml2::xml_attr(node, "value")
}

board_with_stack <- function(board, id, blocks) {
  new_dock_board(
    board_blocks(board),
    stacks = do.call(stacks, set_names(list(blocks), id))
  )
}

test_that("stack menu ui defers the form to a server-rendered slot", {
  board <- new_dock_board(c(a = new_dataset_block("iris")))
  doc <- xml2::read_html(as.character(stack_menu_ui("mid", board)))

  slot <- xml2::xml_find_first(
    doc,
    paste0(
      "//*[contains(concat(' ', normalize-space(@class), ' '),",
      " ' blockr-stack-menu-form-slot ')]"
    )
  )
  expect_identical(xml2::xml_attr(slot, "id"), "mid-form")
  # No field is snapshotted into the panel markup, so nothing can go stale.
  expect_length(xml2::xml_find_all(doc, "//input[@id='mid-stack_id']"), 0L)
  expect_length(xml2::xml_find_all(doc, "//input[@id='mid-stack_name']"), 0L)
})

test_that("a stack menu card leads with the block's mark", {
  board <- new_dock_board(c(a = new_dataset_block("iris")))
  doc <- xml2::read_html(as.character(stack_menu_ui("mid", board)))

  mark <- xml2::xml_find_all(
    doc,
    paste0(
      "//*[", has_class("blockr-stack-menu-card"), "][@data-block-type='a']",
      "/*[", has_class("blockr-block-browser-card-header"), "]",
      "/span[", has_class("blockr-block-mark"), "]"
    )
  )

  # A list row's mark is blockr.ui's at its plain 24px.
  expect_length(mark, 1L)
  expect_identical(xml2::xml_attr(mark, "class"), "blockr-block-mark")
  expect_identical(xml2::xml_attr(mark, "data-category"), "input")
})

test_that("the colour field pairs a native picker with the bound hex input", {
  doc <- xml2::read_html(
    as.character(color_field_tag(NS("mid"), "#abc"))
  )

  hex <- xml2::xml_find_first(doc, "//input[@id='mid-stack_color']")
  expect_identical(xml2::xml_attr(hex, "type"), "text")
  expect_identical(xml2::xml_attr(hex, "value"), "#abc")

  # The picker carries no id: `input$stack_color` stays the hex field, so
  # the spec the menu commits is unchanged. It does need the expanded form
  # of the same colour - `<input type="color">` cannot hold the shorthand.
  swatch <- xml2::xml_find_first(doc, "//input[@type='color']")
  expect_identical(xml2::xml_attr(swatch, "value"), "#aabbcc")
  expect_identical(xml2::xml_attr(swatch, "id"), NA_character_)

  expect_length(xml2::xml_find_all(doc, "//input[@type='range']"), 0L)
})

# The picker only reports through the hex field beside it, and `type="color"`
# is not matched by Shiny's text-input binding, so the chain that carries a
# picked colour to the server -- picker writes the hex field, the hex field's
# "change" reaches the binding, the binding sends `stack_color` -- exists
# nowhere but in the browser. A break there is silent: the form still renders
# and still commits, just never the colour the user picked.
test_that("remove stack action", {
  r_board <- reactiveValues(
    board = new_dock_board(
      c(a = new_dataset_block("iris"), b = new_head_block()),
      stacks = stacks(a = "a")
    )
  )
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        remove_stack_action(
          trigger = reactive("a"),
          board = r_board,
          update = r_update
        )
      )
    },
    {
      session$flushReact()
      upd <- r_update()
      expect_length(upd, 1L)
      expect_named(upd, "stacks")
      expect_named(upd$stacks, "rm")
      expect_identical(upd$stacks$rm, "a")
    }
  )
})
