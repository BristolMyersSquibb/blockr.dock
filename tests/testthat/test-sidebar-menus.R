# The menus split across the wire: R builds the markup and validates a commit,
# the client owns everything in between -- filtering, card selection, which port
# a click takes, and whether the panel closes. None of that is reachable from
# `testServer`, so these cases drive the real board. The fixture fires the
# action triggers directly, which is the path a consumer's context menu takes.

app_path <- function(example) {
  system.file("examples", example, "app.R", package = "blockr.dock")
}

menus_app <- function(name, example = "sidebar-menus") {

  app <- new_app_driver(
    app_path(example),
    name = name,
    seed = 42,
    load_timeout = 30 * 1000,
    timeout = 30 * 1000
  )

  app$wait_for_idle()
  app
}

fixture <- function(x) paste0("my_board-ext_menus-", x)

exported <- function(app, what) app$get_value(export = fixture(what))

js_count <- function(app, selector) {
  app$get_js(sprintf("document.querySelectorAll(%s).length", shQuote(selector)))
}

card <- function(type, dir = NULL) {

  sel <- paste0(".blockr-block-browser-card[data-block-type=", type, "]")

  if (is.null(dir)) {
    return(sel)
  }

  paste0(".blockr-link-menu-direction[data-direction=", dir, "] ", sel)
}

click_sel <- function(app, selector) {
  app$run_js(sprintf("document.querySelector(%s).click()", shQuote(selector)))
}

# Filtering is pure client work, so the CDP call returns with the card set
# already reconciled -- no wait, and none of the flake one would bring.
type_search <- function(app, scope, query) {
  app$run_js(
    sprintf(
      paste0(
        "(function(){",
        "var e=document.querySelector('%s .blockr-block-browser-search');",
        "e.value='%s';",
        "e.dispatchEvent(new Event('input', {bubbles: true}));})()"
      ),
      scope, query
    )
  )
}

set_field <- function(app, selector, value) {
  app$run_js(
    sprintf(
      paste0(
        "(function(){var e=document.querySelector(%s);",
        "e.value='%s';",
        "e.dispatchEvent(new Event('input', {bubbles: true}));",
        "e.dispatchEvent(new Event('change', {bubbles: true}));})()"
      ),
      shQuote(selector), value
    )
  )
}

panel_open <- function(app, panel) {
  app$get_js(
    sprintf(
      "document.getElementById('%s').classList.contains('blockr-sidebar-open')",
      panel
    )
  )
}

# An unpinned commit closes the panel, and that close is issued by the action
# *after* `update()` has been applied -- so it is the one client-visible fact
# that says the board has moved. Gating on it beats an idle wait, which samples
# a lull partway through the round trip.
wait_panel <- function(app, panel, open, timeout = 30 * 1000) {

  cond <- sprintf(
    paste0(
      "document.getElementById('%s')",
      ".classList.contains('blockr-sidebar-open') === %s"
    ),
    panel, if (open) "true" else "false"
  )

  diagnose <- function() {
    sprintf(
      "[sidebar] %s state=%s",
      panel,
      app$get_js(
        sprintf("JSON.stringify(Shiny.shinyapp.$inputValues['%s'])", panel)
      )
    )
  }

  wait_js(app, cond, diagnose, timeout)
}

wait_sel <- function(app, selector, present = TRUE, diagnose = NULL,
                     timeout = 30 * 1000) {

  cond <- sprintf(
    "(document.querySelector(%s) !== null) === %s",
    shQuote(selector), if (present) "true" else "false"
  )

  if (is.null(diagnose)) {
    diagnose <- function() {
      sprintf(
        "[selector] %s cards=%s",
        selector,
        js_count(app, ".blockr-block-browser-card")
      )
    }
  }

  wait_js(app, cond, diagnose, timeout)
}

add_panel <- "my_board-add_block_sidebar"
actions_panel <- "my_board-actions_sidebar"

test_that("the add browser filters its cards as the user types", {

  skip_on_cran()

  app <- menus_app("browser-search")
  withr::defer(app$stop())

  app$click(fixture("add_block"))
  wait_panel(app, add_panel, open = TRUE)

  scope <- paste0("#", add_panel)
  all_cards <- js_count(app, paste0(scope, " .blockr-block-browser-card"))
  expect_gt(all_cards, 1L)

  visible <- function() {
    js_count(app, paste0(scope, " .blockr-block-browser-card:not(.hidden)"))
  }

  expect_identical(visible(), all_cards)

  type_search(app, scope, "head")
  filtered <- visible()

  expect_gt(filtered, 0L)
  expect_lt(filtered, all_cards)

  type_search(app, scope, "zzz_matches_nothing")
  expect_identical(visible(), 0L)

  type_search(app, scope, "")
  expect_identical(visible(), all_cards)
})

test_that("a card-body click adds the block without expanding its form", {

  skip_on_cran()

  app <- menus_app("browser-click-add")
  withr::defer(app$stop())

  expect_setequal(exported(app, "blocks"), c("a", "b", "m", "r", "s"))

  app$click(fixture("add_block"))
  wait_panel(app, add_panel, open = TRUE)

  click_sel(
    app,
    paste0(card("dataset_block"), " .blockr-block-browser-card-header")
  )

  # The commit closes the unpinned panel, which is the board's own signal that
  # the update landed.
  wait_panel(app, add_panel, open = FALSE)

  added <- setdiff(exported(app, "blocks"), c("a", "b", "m", "r", "s"))

  expect_length(added, 1L)
  expect_true(nzchar(added))
})

test_that("the chevron opens a card's form and the in-card button commits it", {

  skip_on_cran()

  app <- menus_app("browser-edit-add")
  withr::defer(app$stop())

  app$click(fixture("add_block"))
  wait_panel(app, add_panel, open = TRUE)

  head_card <- card("head_block")
  click_sel(app, paste0(head_card, " .blockr-block-browser-card-chevron"))

  expect_true(
    app$get_js(
      sprintf(
        "document.querySelector(%s).classList.contains('card-expanded')",
        shQuote(head_card)
      )
    )
  )

  # Expanding is client-only: nothing has been committed yet.
  expect_setequal(exported(app, "blocks"), c("a", "b", "m", "r", "s"))

  set_field(
    app,
    paste0(head_card, " .blockr-block-browser-field-id input"),
    "my_custom_id"
  )
  click_sel(app, paste0(head_card, " .blockr-block-browser-card-add"))

  wait_panel(app, add_panel, open = FALSE)

  expect_setequal(
    exported(app, "blocks"),
    c("a", "b", "m", "r", "s", "my_custom_id")
  )
})

test_that("a click inside a card's form does not commit the block", {

  skip_on_cran()

  app <- menus_app("browser-form-noadd")
  withr::defer(app$stop())

  app$click(fixture("add_block"))
  wait_panel(app, add_panel, open = TRUE)

  data_card <- card("dataset_block")
  click_sel(app, paste0(data_card, " .blockr-block-browser-card-chevron"))
  click_sel(app, paste0(data_card, " .blockr-block-browser-field-id input"))

  app$wait_for_idle()

  expect_true(panel_open(app, add_panel))
  expect_setequal(exported(app, "blocks"), c("a", "b", "m", "r", "s"))
})

# The add catalogue is pre-rendered once in `board_ui()` and the action only
# toggles the panel, so a reopen must not rebuild it. Tagging the live node and
# finding the tag intact afterwards is what distinguishes a toggle from a
# re-render; nothing server-side can tell them apart.
test_that("reopening the add browser does not rebuild it", {

  skip_on_cran()

  app <- menus_app("browser-prerender")
  withr::defer(app$stop())

  browser_sel <- paste0("#", add_panel, " .blockr-block-browser")

  app$click(fixture("add_block"))
  wait_panel(app, add_panel, open = TRUE)

  app$run_js(
    sprintf("document.querySelector(%s).dataset.persist='yes'",
            shQuote(browser_sel))
  )

  click_sel(app, paste0("#", add_panel, " .blockr-sidebar-close"))
  wait_panel(app, add_panel, open = FALSE)

  app$click(fixture("add_block"))
  wait_panel(app, add_panel, open = TRUE)

  expect_identical(
    app$get_js(
      sprintf(
        "document.querySelector(%s).dataset.persist", shQuote(browser_sel)
      )
    ),
    "yes"
  )
})

press_esc <- function(app) {
  app$run_js(
    paste0(
      "document.activeElement.dispatchEvent(",
      "new KeyboardEvent('keydown', {key: 'Escape', bubbles: true}))"
    )
  )
}

# The panel's dismiss listener is on `mousedown`, not `click`, so a synthetic
# click would never reach it.
click_outside <- function(app) {
  app$run_js(
    "document.body.dispatchEvent(new MouseEvent('mousedown', {bubbles: true}))"
  )
}

panel_state <- function(app, panel) {

  app$wait_for_idle()
  state <- app$get_value(input = panel)

  state[c("open", "pinned")]
}

test_that("Escape and an outside click close an unpinned panel", {

  skip_on_cran()

  app <- menus_app("sidebar-dismiss")
  withr::defer(app$stop())

  expect_identical(
    panel_state(app, add_panel),
    list(open = FALSE, pinned = FALSE)
  )

  app$click(fixture("add_block"))
  wait_panel(app, add_panel, open = TRUE)

  expect_identical(
    panel_state(app, add_panel),
    list(open = TRUE, pinned = FALSE)
  )

  press_esc(app)
  wait_panel(app, add_panel, open = FALSE)

  app$click(fixture("add_block"))
  wait_panel(app, add_panel, open = TRUE)

  click_outside(app)
  wait_panel(app, add_panel, open = FALSE)

  expect_identical(
    panel_state(app, add_panel),
    list(open = FALSE, pinned = FALSE)
  )
})

test_that("a pinned panel survives Escape and an outside click", {

  skip_on_cran()

  app <- menus_app("sidebar-pinned")
  withr::defer(app$stop())

  app$click(fixture("add_block"))
  wait_panel(app, add_panel, open = TRUE)

  click_sel(app, paste0("#", add_panel, " .blockr-sidebar-pin"))
  app$wait_for_idle()

  expect_identical(
    panel_state(app, add_panel),
    list(open = TRUE, pinned = TRUE)
  )

  press_esc(app)
  click_outside(app)
  app$wait_for_idle()

  expect_identical(
    panel_state(app, add_panel),
    list(open = TRUE, pinned = TRUE)
  )

  # The close button is the one dismissal that overrides a pin.
  click_sel(app, paste0("#", add_panel, " .blockr-sidebar-close"))
  wait_panel(app, add_panel, open = FALSE)

  expect_false(panel_state(app, add_panel)$open)
})

# An overlay panel floats on open and only reflows the board once pinned, which
# it does by writing a CSS variable and a class on `<html>`. Both are set from
# the client and read by nothing on the server.
test_that("an overlay panel reflows the board only once pinned", {

  skip_on_cran()

  app <- menus_app("sidebar-overlay")
  withr::defer(app$stop())

  pushed <- function() {
    app$get_js(
      "document.documentElement.classList.contains('blockr-html-pushed-right')"
    )
  }

  # The client measures the panel to write this, so it lands sub-pixel
  # (420.00006103515625px). Comparing it against the measured panel states the
  # actual contract -- the board is reflowed by exactly the width taken from it.
  width <- function() {
    as.numeric(
      sub(
        "px$",
        "",
        app$get_js(
          paste0(
            "document.documentElement.style",
            ".getPropertyValue('--blockr-sidebar-width-right')"
          )
        )
      )
    )
  }

  panel_width <- function() {
    app$get_js(
      sprintf(
        "document.getElementById('%s').getBoundingClientRect().width",
        add_panel
      )
    )
  }

  app$click(fixture("add_block"))
  wait_panel(app, add_panel, open = TRUE)

  expect_false(pushed())

  click_sel(app, paste0("#", add_panel, " .blockr-sidebar-pin"))
  app$wait_for_idle()

  expect_true(pushed())
  expect_gt(width(), 0)

  # The variable is snapshotted at pin time and re-measured after the pin has
  # reflowed the board, so the two agree only to layout's sub-pixel rounding.
  expect_equal(width(), panel_width(), tolerance = 1e-6)

  click_sel(app, paste0("#", add_panel, " .blockr-sidebar-pin"))
  app$wait_for_idle()

  expect_false(pushed())
  expect_equal(width(), 0)
})

port_select <- function(card_sel) {
  paste0(card_sel, " .blockr-block-browser-field-block-input select")
}

# A committed link leaves the pool in place rather than re-rendering it, so the
# option list is what says the sync landed. Comparing the serialised array keeps
# the wait and the assertion on the same fact.
port_values_js <- function(card_sel) {
  sprintf(
    paste0(
      "JSON.stringify(Array.from(document.querySelectorAll(%s))",
      ".map(function(o){return o.value}))"
    ),
    shQuote(paste0(port_select(card_sel), " option"))
  )
}

wait_ports <- function(app, card_sel, expected, timeout = 30 * 1000) {
  wait_js(
    app,
    sprintf("%s === '%s'", port_values_js(card_sel), expected),
    function() sprintf("[ports] %s", app$get_js(port_values_js(card_sel))),
    timeout
  )
}

pin_actions <- function(app) {
  click_sel(app, paste0("#", actions_panel, " .blockr-sidebar-pin"))
  app$wait_for_idle()
}

test_that("an outgoing card commits a link out of the anchor", {

  skip_on_cran()

  app <- menus_app("link-outgoing")
  withr::defer(app$stop())

  expect_identical(exported(app, "links"), character())

  app$click(fixture("link_a"))
  wait_panel(app, actions_panel, open = TRUE)

  click_sel(app, card("b", "outgoing"))
  wait_panel(app, actions_panel, open = FALSE)

  expect_identical(exported(app, "links"), "a>b>data")
})

test_that("an incoming card commits a link into the anchor", {

  skip_on_cran()

  app <- menus_app("link-incoming")
  withr::defer(app$stop())

  app$click(fixture("link_m"))
  wait_panel(app, actions_panel, open = TRUE)

  click_sel(app, card("a", "incoming"))
  wait_panel(app, actions_panel, open = FALSE)

  expect_identical(exported(app, "links"), "a>m>x")
})

test_that("the link menu filters across both direction sections", {

  skip_on_cran()

  app <- menus_app("link-search")
  withr::defer(app$stop())

  app$click(fixture("link_m"))
  wait_panel(app, actions_panel, open = TRUE)

  scope <- paste0("#", actions_panel)

  expect_identical(
    js_count(app, paste0(scope, " .blockr-link-menu-direction")), 2L
  )

  all_cards <- js_count(app, paste0(scope, " .blockr-block-browser-card"))
  visible <- function() {
    js_count(app, paste0(scope, " .blockr-block-browser-card:not(.hidden)"))
  }

  expect_identical(visible(), all_cards)

  type_search(app, scope, "zzz_matches_nothing")
  expect_identical(visible(), 0L)

  type_search(app, scope, "")
  expect_identical(visible(), all_cards)
})

# A pinned panel is the documented way to wire several links in a row. The menu
# reconciles its own cards against the board instead of re-rendering, so a
# target whose only port is now taken has to leave the DOM on its own.
test_that("a wired single-port target leaves a pinned menu's pool", {

  skip_on_cran()

  app <- menus_app("link-pool-sync")
  withr::defer(app$stop())

  app$click(fixture("link_a"))
  wait_panel(app, actions_panel, open = TRUE)
  pin_actions(app)

  before <- js_count(
    app, paste0("#", actions_panel, " .blockr-block-browser-card")
  )

  click_sel(app, card("b", "outgoing"))
  wait_sel(app, card("b", "outgoing"), present = FALSE)

  expect_true(panel_open(app, actions_panel))
  expect_identical(exported(app, "links"), "a>b>data")
  expect_lt(
    js_count(app, paste0("#", actions_panel, " .blockr-block-browser-card")),
    before
  )
})

test_that("a repeat commit through one card takes the next free port", {

  skip_on_cran()

  app <- menus_app("link-next-port")
  withr::defer(app$stop())

  app$click(fixture("link_a"))
  wait_panel(app, actions_panel, open = TRUE)
  pin_actions(app)

  merge_card <- card("m", "outgoing")
  wait_ports(app, merge_card, '["x","y"]')

  click_sel(app, merge_card)
  wait_ports(app, merge_card, '["y"]')

  expect_identical(exported(app, "links"), "a>m>x")

  click_sel(app, merge_card)
  wait_sel(app, merge_card, present = FALSE)

  expect_setequal(exported(app, "links"), c("a>m>x", "a>m>y"))
})

# The name / colour / id fields are a `uiOutput` the server fills on a later
# round trip, so an open panel does not yet mean a usable form. Confirming
# before it lands commits a NULL name, which the validator rejects without
# closing anything -- so the close wait then spends its whole budget.
wait_stack_form <- function(app, action) {
  wait_sel(app, paste0("#my_board-", action, "-menu-stack_name"))
}

stack_card_selected <- function(app, id) {
  app$get_js(
    sprintf(
      "document.querySelector(%s).classList.contains('card-selected')",
      shQuote(card(id))
    )
  )
}

# The menu holds its selection in a client-side list, separate from which cards
# the search leaves visible. Filtering to nothing and back tells the two apart:
# a selection stored on the visible cards alone would not survive it.
test_that("the stack menu holds its selection across a search that hides it", {

  skip_on_cran()

  app <- menus_app("stack-create")
  withr::defer(app$stop())

  app$click(fixture("add_stack"))
  wait_panel(app, actions_panel, open = TRUE)
  wait_stack_form(app, "add_stack_action")

  scope <- paste0("#", actions_panel)
  selected <- function() {
    js_count(app, paste0(scope, " .blockr-block-browser-card.card-selected"))
  }
  visible <- function() {
    js_count(app, paste0(scope, " .blockr-block-browser-card:not(.hidden)"))
  }

  # Blocks r and s are stacked already, so the create pool offers a, b and m.
  expect_identical(visible(), 3L)
  expect_identical(selected(), 0L)

  click_sel(app, card("a"))
  click_sel(app, card("b"))

  expect_identical(selected(), 2L)

  type_search(app, scope, "zzz_matches_nothing")

  expect_identical(visible(), 0L)
  expect_identical(selected(), 2L)

  type_search(app, scope, "")

  expect_identical(visible(), 3L)
  expect_identical(selected(), 2L)

  set_field(app, "#my_board-add_stack_action-menu-stack_name", "My stack")
  click_sel(app, ".blockr-stack-menu-confirm")
  wait_panel(app, actions_panel, open = FALSE)

  stacks <- exported(app, "stacks")
  added <- setdiff(names(stacks), "s1")

  expect_length(added, 1L)
  expect_setequal(unlst(stacks[[added]]), c("a", "b"))
})

test_that("the edit flow arrives with the stack's members selected", {

  skip_on_cran()

  app <- menus_app("stack-edit")
  withr::defer(app$stop())

  app$click(fixture("edit_stack"))
  wait_panel(app, actions_panel, open = TRUE)
  wait_stack_form(app, "edit_stack_action")

  expect_true(stack_card_selected(app, "r"))
  expect_true(stack_card_selected(app, "s"))
  expect_false(stack_card_selected(app, "a"))

  click_sel(app, card("r"))

  expect_false(stack_card_selected(app, "r"))

  click_sel(app, ".blockr-stack-menu-confirm")
  wait_panel(app, actions_panel, open = FALSE)

  expect_setequal(unlst(exported(app, "stacks")[["s1"]]), "s")
})

# The inputs menu is the one that keeps its panel open across commits, so there
# is no close to gate on. Its rows are a `uiOutput` re-rendered from the board
# after every commit, which makes the row list itself the client-visible proof
# that an edit landed. Its board is a fixture of its own: only a variadic block
# grows rows carrying a remove button and a name field, and only fixed link ids
# let an assertion name the slot that moved.
inputs_app <- function(name) menus_app(name, "sidebar-inputs")

inputs_row <- function(link_id) {
  paste0(".blockr-inputs-row[data-link-id=", link_id, "]")
}

name_field <- function(link_id) {
  paste0(inputs_row(link_id), " .blockr-inputs-name-input")
}

row_ids <- function(app) {
  unlst(
    app$get_js(
      paste0(
        "Array.from(document.querySelectorAll('.blockr-inputs-row'))",
        ".map(function (r) { return r.getAttribute('data-link-id') })"
      )
    )
  )
}

rows_diag <- function(app) {
  sprintf("[inputs] rows=%s", paste(row_ids(app), collapse = ","))
}

# The row list arrives on a round trip of its own, so an open panel does not
# yet mean a row to drive.
wait_inputs_row <- function(app, link_id, present = TRUE) {
  wait_sel(app, inputs_row(link_id), present, function() rows_diag(app))
}

open_inputs_menu <- function(app) {
  app$click(fixture("edit_inputs"))
  wait_panel(app, actions_panel, open = TRUE)
  wait_inputs_row(app, "l1")
}

# A re-render rebuilds the field from the board, so the `value` attribute is
# what says the rename came back -- the client-side `.value` the commit was
# typed into never touches it.
wait_input_name <- function(app, link_id, name, timeout = 30 * 1000) {

  probe <- sprintf("document.querySelector(%s)", shQuote(name_field(link_id)))

  cond <- sprintf(
    paste0(
      "(function(){var e=%s;",
      "return e !== null && e.getAttribute('value') === %s})()"
    ),
    probe, shQuote(name)
  )

  wait_js(app, cond, function() rows_diag(app), timeout)
}

focus_field <- function(app, selector) {
  app$run_js(sprintf("document.querySelector(%s).focus()", shQuote(selector)))
}

field_focused <- function(app, selector) {
  app$get_js(
    sprintf(
      "document.activeElement === document.querySelector(%s)",
      shQuote(selector)
    )
  )
}

# Chrome raises `change` only for a value the user themselves edited, so a
# scripted `e.value = ...` reaches the rename handler but leaves Enter with
# nothing to commit. Typing through CDP sets that flag, and `insertText` fires
# `input` alone -- nothing reaches the server until the key press.
type_into <- function(app, selector, text) {
  focus_field(app, selector)
  app$get_chromote_session()$Input$insertText(text = text)
}

press_enter <- function(app) {
  app$get_chromote_session()$Input$dispatchKeyEvent(
    type = "keyDown",
    key = "Enter",
    code = "Enter",
    windowsVirtualKeyCode = 13,
    nativeVirtualKeyCode = 13,
    text = "\r"
  )
}

test_that("a remove click commits the row it was clicked on", {

  skip_on_cran()

  app <- inputs_app("inputs-remove")
  withr::defer(app$stop())

  open_inputs_menu(app)

  expect_identical(row_ids(app), c("l1", "l2", "l3"))

  click_sel(app, paste0(inputs_row("l2"), " .blockr-inputs-remove"))
  wait_inputs_row(app, "l2", present = FALSE)

  expect_identical(row_ids(app), c("l1", "l3"))
  expect_identical(exported(app, "links"), c("l1:a>", "l3:c>"))

  # Unlike the other menus, this one is built to take several edits in a row,
  # so a commit leaves the panel where it is.
  expect_true(panel_open(app, actions_panel))
})

test_that("a name committed on a row renames that row's input", {

  skip_on_cran()

  app <- inputs_app("inputs-rename")
  withr::defer(app$stop())

  open_inputs_menu(app)

  expect_identical(exported(app, "links"), c("l1:a>", "l2:b>", "l3:c>"))

  set_field(app, name_field("l2"), "middle")
  wait_input_name(app, "l2", "middle")

  expect_identical(exported(app, "links"), c("l1:a>", "l2:b>middle", "l3:c>"))
})

# Enter is the one commit path with no server-side counterpart: a user who
# types a name and never clicks away is carried entirely by the keydown
# listener. What that listener adds is the focus change rather than the commit
# -- Chrome raises `change` on Enter of its own accord (measured on a bare
# input: one `change`, focus kept). The handler suppresses that round with
# `preventDefault()` and blurs instead, so one `change` still arrives and the
# field is released with it.
test_that("Enter commits the typed name and takes focus out of the field", {

  skip_on_cran()

  app <- inputs_app("inputs-enter")
  withr::defer(app$stop())

  open_inputs_menu(app)

  focus_field(app, name_field("l3"))
  expect_true(field_focused(app, name_field("l3")))

  # An untouched field has nothing to commit, so the release stands on its own
  # here -- no round trip follows to re-render the field out from under it.
  press_enter(app)

  expect_false(field_focused(app, name_field("l3")))

  app$wait_for_idle()

  expect_identical(exported(app, "links"), c("l1:a>", "l2:b>", "l3:c>"))

  type_into(app, name_field("l3"), "typed")
  app$wait_for_idle()

  expect_identical(exported(app, "links"), c("l1:a>", "l2:b>", "l3:c>"))

  press_enter(app)
  wait_input_name(app, "l3", "typed")

  expect_identical(exported(app, "links"), c("l1:a>", "l2:b>", "l3:c>typed"))
})
