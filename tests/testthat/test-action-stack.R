# The stack menu publishes its committed selection into the parent
# session's namespace as `menu-commit` (list(blocks, nonce)); the
# panel-level form fields are real Shiny inputs at `menu-stack_name`,
# `menu-stack_color`, and `menu-stack_id`. Drive them directly to
# simulate the user creating / editing a stack.
local_mocked_sidebar <- function(env = parent.frame()) {
  local_mocked_bindings(
    show_sidebar         = function(...) invisible(list(...)),
    keep_or_hide_sidebar = function(...) invisible(list(...)),
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

test_that("add stack action: valid commit creates one stack", {
  local_mocked_sidebar()
  r_board <- reactiveValues(
    board = new_board(
      c(a = new_dataset_block("iris"), b = new_head_block())
    ),
    board_id = "my_board"
  )
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        add_stack_action(
          trigger = reactive(TRUE),
          board = r_board,
          update = r_update
        )
      )
    },
    {
      session$flushReact()
      set_menu(
        session,
        blocks = c("a", "b"),
        name = "My stack",
        color = "#ff0000",
        id = "s1",
        nonce = 1L
      )

      upd <- r_update()
      expect_length(upd, 1L)
      expect_named(upd, "stacks")
      expect_named(upd$stacks, "add")
      expect_named(upd$stacks$add, "s1")
      expect_s3_class(upd$stacks$add, "stacks")
      expect_identical(stack_blocks(upd$stacks$add[["s1"]]), c("a", "b"))
      expect_identical(stack_name(upd$stacks$add[["s1"]]), "My stack")
      expect_identical(stack_color(upd$stacks$add[["s1"]]), "#ff0000")
    }
  )
})

test_that("add stack action: invalid inputs short-circuit before update", {
  local_mocked_sidebar()
  r_board <- reactiveValues(
    board = new_board(c(a = new_dataset_block("iris"))),
    board_id = "b"
  )
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        add_stack_action(
          trigger = reactive(TRUE),
          board = r_board,
          update = r_update
        )
      )
    },
    {
      session$flushReact()

      # Empty id -> rejected.
      set_menu(
        session,
        blocks = "a", name = "N", color = "#ffffff", id = "",
        nonce = 1L
      )
      expect_length(r_update(), 0L)

      # Empty name -> rejected.
      set_menu(
        session,
        blocks = "a", name = "", color = "#ffffff", id = "s1",
        nonce = 2L
      )
      expect_length(r_update(), 0L)

      # Invalid colour -> rejected.
      set_menu(
        session,
        blocks = "a", name = "N", color = "not-a-hex", id = "s1",
        nonce = 3L
      )
      expect_length(r_update(), 0L)
    }
  )
})

test_that("edit stack action: valid commit modifies the existing stack", {
  local_mocked_sidebar()
  r_board <- reactiveValues(
    board = new_dock_board(
      c(a = new_dataset_block("iris"), b = new_head_block()),
      stacks = stacks(s1 = "a")
    ),
    board_id = "b"
  )
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        "edit_stack_action",
        edit_stack_action(
          trigger = reactive("s1"),
          board = r_board,
          update = r_update
        )
      )
    },
    {
      session$flushReact()
      set_menu(
        session,
        blocks = c("a", "b"),
        name = "Updated",
        color = "#00ff00",
        id = NULL,
        nonce = 1L
      )

      upd <- r_update()
      expect_length(upd, 1L)
      expect_named(upd, "stacks")
      expect_named(upd$stacks, "mod")
      expect_named(upd$stacks$mod, "s1")
      # `mod` entries are partial-arg deltas (named list of constructor
      # argument values), not full `stacks` objects.
      expect_type(upd$stacks$mod, "list")
      expect_false(inherits(upd$stacks$mod, "stacks"))
      expect_identical(upd$stacks$mod[["s1"]]$blocks, c("a", "b"))
      expect_identical(upd$stacks$mod[["s1"]]$name, "Updated")
      expect_identical(upd$stacks$mod[["s1"]]$color, "#00ff00")
    }
  )
})

test_that("edit stack action: board change with no active edit is inert", {
  # Reproduces the create-stack crash: with no edit in progress
  # (`trigger()` is NULL), a board change from another action must not
  # error in the auto-close observer, and must not close the sidebar.
  hide_calls <- list()
  local_mocked_bindings(
    show_sidebar = function(...) invisible(NULL),
    keep_or_hide_sidebar = function(...) invisible(NULL),
    hide_sidebar = function(id, ...) {
      hide_calls[[length(hide_calls) + 1L]] <<- id
      invisible(NULL)
    },
    sidebar_state = function(id, ...) {
      list(open = TRUE, pinned = TRUE, owner = "edit_stack_action")
    }
  )

  r_board <- reactiveValues(
    board = new_dock_board(c(a = new_dataset_block("iris"))),
    board_id = "b"
  )

  testServer(
    function(id, ...) {
      moduleServer(
        "edit_stack_action",
        edit_stack_action(
          trigger = reactive(NULL),
          board = r_board,
          update = reactiveVal(list())
        )
      )
    },
    {
      session$flushReact()
      # Another action mutates the board (e.g. creates a stack).
      r_board$board <- new_dock_board(
        c(a = new_dataset_block("iris")),
        stacks = stacks(s1 = "a")
      )
      expect_no_error(session$flushReact())
      expect_length(hide_calls, 0L)
    }
  )
})

test_that("edit stack action: removing the edited stack closes the sidebar", {
  # Open the edit menu for `s1`, then remove `s1` from the board: the
  # board observer must close the sidebar immediately (not wait for an
  # "Update" click), and never error in `lookup_stack()`.
  hide_calls <- list()
  local_mocked_bindings(
    show_sidebar = function(...) invisible(NULL),
    keep_or_hide_sidebar = function(...) invisible(NULL),
    hide_sidebar = function(id, ...) {
      hide_calls[[length(hide_calls) + 1L]] <<- id
      invisible(NULL)
    },
    # The auto-close is gated on this action owning the open panel.
    sidebar_state = function(id, ...) {
      list(open = TRUE, pinned = TRUE, owner = "edit_stack_action")
    }
  )

  r_board <- reactiveValues(
    board = new_dock_board(
      c(a = new_dataset_block("iris"), b = new_head_block()),
      stacks = stacks(s1 = "a")
    ),
    board_id = "b"
  )
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        "edit_stack_action",
        edit_stack_action(
          trigger = reactive("s1"),
          board = r_board,
          update = r_update
        )
      )
    },
    {
      session$flushReact()
      expect_length(hide_calls, 0L)

      # Remove the edited stack -> sidebar closes live.
      r_board$board <- new_dock_board(board_blocks(r_board$board))
      session$flushReact()

      expect_gte(length(hide_calls), 1L)
      expect_identical(hide_calls[[1L]], "b-actions_sidebar")
    }
  )
})

test_that("edit stack action: a form written by another action stays open", {
  # Same removal, but the shared panel has since been filled by another
  # action: closing it here would take down a form this handler no longer
  # has anything in.
  hide_calls <- list()
  local_mocked_bindings(
    show_sidebar = function(...) invisible(NULL),
    keep_or_hide_sidebar = function(...) invisible(NULL),
    hide_sidebar = function(id, ...) {
      hide_calls[[length(hide_calls) + 1L]] <<- id
      invisible(NULL)
    },
    sidebar_state = function(id, ...) {
      list(open = TRUE, pinned = TRUE, owner = "add_link_action")
    }
  )

  r_board <- reactiveValues(
    board = new_dock_board(
      c(a = new_dataset_block("iris"), b = new_head_block()),
      stacks = stacks(s1 = "a")
    ),
    board_id = "b"
  )

  testServer(
    function(id, ...) {
      moduleServer(
        "edit_stack_action",
        edit_stack_action(
          trigger = reactive("s1"),
          board = r_board,
          update = reactiveVal(list())
        )
      )
    },
    {
      session$flushReact()

      r_board$board <- new_dock_board(board_blocks(r_board$board))
      session$flushReact()

      expect_length(hide_calls, 0L)
    }
  )
})

test_that("stack actions write the sidebar from their own module", {
  # `show_sidebar()` reads the panel's owner off the session it is called
  # with, so a write has to happen in the action's own reactive domain. A
  # write deferred into a flush callback would run under the root session
  # and stamp that instead, which this records.
  wrote_from <- list()
  local_mocked_bindings(
    show_sidebar = function(...) {
      wrote_from[[length(wrote_from) + 1L]] <<- get_session()$ns(NULL)
      invisible(NULL)
    },
    keep_or_hide_sidebar = function(...) invisible(NULL),
    hide_sidebar = function(...) invisible(NULL)
  )

  r_board <- reactiveValues(
    board = new_dock_board(
      c(a = new_dataset_block("iris")),
      stacks = stacks(s1 = "a")
    ),
    board_id = "b"
  )

  fire_action(add_stack_action, TRUE, r_board)
  fire_action(edit_stack_action, "s1", r_board)

  expect_identical(wrote_from, list("add_stack_action", "edit_stack_action"))
})

test_that("edit stack action: invalid colour / block id short-circuit", {
  local_mocked_sidebar()
  r_board <- reactiveValues(
    board = new_dock_board(
      c(a = new_dataset_block("iris"), b = new_head_block()),
      stacks = stacks(s1 = "a")
    ),
    board_id = "b"
  )
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        "edit_stack_action",
        edit_stack_action(
          trigger = reactive("s1"),
          board = r_board,
          update = r_update
        )
      )
    },
    {
      session$flushReact()

      # Empty name -> rejected.
      set_menu(
        session,
        blocks = c("a", "b"), name = "", color = "#abcdef", id = NULL,
        nonce = 1L
      )
      expect_length(r_update(), 0L)

      # Invalid colour -> rejected.
      set_menu(
        session,
        blocks = c("a", "b"), name = "N", color = "not-hex", id = NULL,
        nonce = 2L
      )
      expect_length(r_update(), 0L)
    }
  )
})

test_that("add stack action: a commit re-seeds the id the board just took", {
  # A pinned panel stays open across creates, so the form has to move off
  # the id it just used - otherwise the next commit is rejected as a
  # duplicate. The seed comes from the merged board, which the form reads
  # by being bound to it rather than rebuilt after the commit.
  local_mocked_sidebar()
  r_board <- reactiveValues(
    board = new_dock_board(
      c(a = new_dataset_block("iris"), b = new_head_block())
    ),
    board_id = "b"
  )
  r_update <- reactiveVal(list())

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        add_stack_action(
          trigger = reactive(TRUE),
          board = r_board,
          update = r_update
        )
      )
    },
    {
      session$flushReact()
      seeded <- form_field(output$`menu-form`$html, "stack_id")
      seeded_name <- form_field(output$`menu-form`$html, "stack_name")
      expect_true(nzchar(seeded))

      set_menu(
        session,
        blocks = "a", name = seeded_name, color = "#ff0000", id = seeded,
        nonce = 1L
      )
      expect_named(r_update()$stacks$add, seeded)

      r_board$board <- board_with_stack(r_board$board, seeded, "a")
      session$flushReact()

      # The whole form re-seeds for the next stack: `rand_names()` excludes
      # what the board already carries, so neither value can repeat.
      expect_false(
        form_field(output$`menu-form`$html, "stack_id") %in%
          board_stack_ids(r_board$board)
      )
      expect_false(
        identical(
          form_field(output$`menu-form`$html, "stack_name"), seeded_name
        )
      )
    }
  )
})

test_that("add stack action: a foreign board change keeps the entered form", {
  # Create-mode fields are the user's, not the board's: an unrelated board
  # change must not swap a typed id / name for another random suggestion,
  # nor a picked colour for the default.
  local_mocked_sidebar()
  r_board <- reactiveValues(
    board = new_dock_board(c(a = new_dataset_block("iris"))),
    board_id = "b"
  )

  testServer(
    function(id, ...) {
      moduleServer(
        id,
        add_stack_action(
          trigger = reactive(TRUE),
          board = r_board,
          update = reactiveVal(list())
        )
      )
    },
    {
      session$flushReact()
      session$setInputs(
        `menu-stack_id` = "mine",
        `menu-stack_name` = "My stack",
        `menu-stack_color` = "#3366cc"
      )

      # Another action adds a block; the suggested id is still free, so
      # nothing was created from this form.
      r_board$board <- new_dock_board(
        c(a = new_dataset_block("iris"), b = new_head_block())
      )
      session$flushReact()

      expect_identical(form_field(output$`menu-form`$html, "stack_id"), "mine")
      expect_identical(
        form_field(output$`menu-form`$html, "stack_name"), "My stack"
      )
      expect_identical(
        form_field(output$`menu-form`$html, "stack_color"), "#3366cc"
      )
    }
  )
})

test_that("edit stack action: the form follows the edited stack", {
  # In edit mode the fields show board state, so a rename landing from
  # anywhere refreshes them in place - no id field, since the id is fixed.
  local_mocked_sidebar()
  r_board <- reactiveValues(
    board = new_dock_board(
      c(a = new_dataset_block("iris")),
      stacks = stacks(
        s1 = new_dock_stack("a", name = "First", color = "#ff0000")
      )
    ),
    board_id = "b"
  )

  testServer(
    function(id, ...) {
      moduleServer(
        "edit_stack_action",
        edit_stack_action(
          trigger = reactive("s1"),
          board = r_board,
          update = reactiveVal(list())
        )
      )
    },
    {
      session$flushReact()
      expect_identical(
        form_field(output$`menu-form`$html, "stack_name"), "First"
      )
      expect_identical(
        form_field(output$`menu-form`$html, "stack_color"), "#ff0000"
      )
      expect_true(is.na(form_field(output$`menu-form`$html, "stack_id")))

      r_board$board <- new_dock_board(
        board_blocks(r_board$board),
        stacks = stacks(
          s1 = new_dock_stack("a", name = "Renamed", color = "#00ff00")
        )
      )
      session$flushReact()

      expect_identical(
        form_field(output$`menu-form`$html, "stack_name"), "Renamed"
      )
      expect_identical(
        form_field(output$`menu-form`$html, "stack_color"), "#00ff00"
      )
    }
  )
})

test_that("add stack action: a commit closes an unpinned panel only", {
  # The panel body is never re-pushed after a commit - the menu tracks the
  # board itself - so all the action does is close an unpinned panel.
  calls <- function(pinned) {
    seen <- list(show = 0L, hide = 0L)
    local_mocked_bindings(
      show_sidebar = function(...) seen$show <<- seen$show + 1L,
      hide_sidebar = function(...) seen$hide <<- seen$hide + 1L,
      keep_or_hide_sidebar = function(...) {
        stop("the panel must not be rebuilt after a commit")
      },
      sidebar_state = function(id, ...) list(open = TRUE, pinned = pinned)
    )

    r_board <- reactiveValues(
      board = new_dock_board(c(a = new_dataset_block("iris"))),
      board_id = "b"
    )

    testServer(
      function(id, ...) {
        moduleServer(
          id,
          add_stack_action(
            trigger = reactive(TRUE),
            board = r_board,
            update = reactiveVal(list())
          )
        )
      },
      {
        session$flushReact()
        set_menu(
          session,
          blocks = "a", name = "N", color = "#ffffff", id = "s1", nonce = 1L
        )
      }
    )

    seen
  }

  expect_identical(calls(pinned = FALSE), list(show = 1L, hide = 1L))
  expect_identical(calls(pinned = TRUE), list(show = 1L, hide = 0L))
})

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
test_that("a picked colour reaches the server through the hex field", {

  skip_on_cran()

  app <- new_app_driver(
    system.file("examples", "sidebar-owner", "app.R", package = "blockr.dock"),
    name = "stack-color",
    seed = 42,
    load_timeout = 30 * 1000,
    timeout = 30 * 1000
  )
  withr::defer(app$stop())

  app$wait_for_idle()
  app$click("my_board-ext_fire-edit_stack")

  swatch <- ".blockr-stack-menu-swatch"
  wait_js(
    app,
    sprintf("document.querySelector('%s') !== null", swatch),
    function() paste("sidebar html:", app$get_html("#my_board-actions_sidebar"))
  )

  doc <- xml2::read_html(
    app$get_html("#my_board-edit_stack_action-menu-form")
  )
  field_value <- function(xpath) {
    xml2::xml_attr(xml2::xml_find_first(doc, xpath), "value")
  }

  expect_identical(
    field_value("//input[@type='color']"),
    field_value("//input[contains(@id, 'stack_color')]")
  )

  # Black is the case the sliders could not express at all: saturation was
  # pinned at 60% and lightness floored at 20.
  app$run_js(
    paste0(
      "var sw = document.querySelector('", swatch, "');",
      "sw.value = '#000000';",
      "sw.dispatchEvent(new Event('input', {bubbles: true}));",
      "sw.dispatchEvent(new Event('change', {bubbles: true}));"
    )
  )
  app$wait_for_idle()

  expect_identical(
    app$get_value(input = "my_board-edit_stack_action-menu-stack_color"),
    "#000000"
  )

  # And back the other way, so a pasted brand colour shows in the picker.
  app$run_js(
    "var hex = document.querySelector('.blockr-stack-menu-hex');
     hex.value = '#abc';
     hex.dispatchEvent(new Event('input', {bubbles: true}));"
  )

  expect_identical(
    app$get_js(paste0("document.querySelector('", swatch, "').value")),
    "#aabbcc"
  )
})

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
