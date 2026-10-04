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

actions_panel <- "my_board-actions_sidebar"

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
    panel_state(app, actions_panel),
    list(open = FALSE, pinned = FALSE)
  )

  app$click(fixture("add_stack"))
  wait_panel(app, actions_panel, open = TRUE)

  expect_identical(
    panel_state(app, actions_panel),
    list(open = TRUE, pinned = FALSE)
  )

  press_esc(app)
  wait_panel(app, actions_panel, open = FALSE)

  app$click(fixture("add_stack"))
  wait_panel(app, actions_panel, open = TRUE)

  click_outside(app)
  wait_panel(app, actions_panel, open = FALSE)

  expect_identical(
    panel_state(app, actions_panel),
    list(open = FALSE, pinned = FALSE)
  )
})

test_that("a pinned panel survives Escape and an outside click", {

  skip_on_cran()

  app <- menus_app("sidebar-pinned")
  withr::defer(app$stop())

  app$click(fixture("add_stack"))
  wait_panel(app, actions_panel, open = TRUE)

  click_sel(app, paste0("#", actions_panel, " .blockr-sidebar-pin"))
  app$wait_for_idle()

  expect_identical(
    panel_state(app, actions_panel),
    list(open = TRUE, pinned = TRUE)
  )

  press_esc(app)
  click_outside(app)
  app$wait_for_idle()

  expect_identical(
    panel_state(app, actions_panel),
    list(open = TRUE, pinned = TRUE)
  )

  # The close button is the one dismissal that overrides a pin.
  click_sel(app, paste0("#", actions_panel, " .blockr-sidebar-close"))
  wait_panel(app, actions_panel, open = FALSE)

  expect_false(panel_state(app, actions_panel)$open)
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
        actions_panel
      )
    )
  }

  app$click(fixture("add_stack"))
  wait_panel(app, actions_panel, open = TRUE)

  expect_false(pushed())

  click_sel(app, paste0("#", actions_panel, " .blockr-sidebar-pin"))
  app$wait_for_idle()

  expect_true(pushed())
  expect_gt(width(), 0)

  # The variable is snapshotted at pin time and re-measured after the pin has
  # reflowed the board, so the two agree only to layout's sub-pixel rounding.
  expect_equal(width(), panel_width(), tolerance = 1e-6)

  click_sel(app, paste0("#", actions_panel, " .blockr-sidebar-pin"))
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

# The "+" menu, the block's "…" menu, the section toggles and the options
# pages are client code over a server round trip, so these drive the board.

menu_sel <- "body > .blockr-menu"

# The open menu's caption and box, and the box of the element it should hang
# off when one is named.
menu_box <- function(app, anchor = NULL) {
  app$get_js(
    sprintf(
      paste0(
        "(function(){",
        "var m = document.querySelector('body > .blockr-menu');",
        "var r = m.getBoundingClientRect();",
        "var a = %s;",
        "var b = a ? a.getBoundingClientRect() : null;",
        "var c = m.querySelector('.blockr-menu__caption');",
        "return {caption: c ? c.textContent : null, top: r.top,",
        "  center: r.left + r.width / 2, viewport: window.innerWidth,",
        "  anchorBottom: b ? b.bottom : null};",
        "})()"
      ),
      if (is.null(anchor)) "null" else anchor
    )
  )
}

press <- function(app, key, target = "document.activeElement") {
  app$run_js(
    sprintf(
      paste0(
        "%s.dispatchEvent(",
        "new KeyboardEvent('keydown', {key: '%s', bubbles: true}))"
      ),
      target, key
    )
  )
}

# The first block menu button on screen; cards behind a tab have no box.
shown_menu_btn <- paste0(
  "[...document.querySelectorAll('.blockr-block-menu-btn')]",
  ".find(b => b.getBoundingClientRect().width > 0)"
)

test_that("a pick from the + menu adds the block", {

  skip_on_cran()

  app <- menus_app("plus-menu-pick")
  withr::defer(app$stop())

  app$click(fixture("add_block"))
  wait_sel(app, paste(menu_sel, ".blockr-menu__filter-input"))

  # An action fired from code names no gesture, so the menu takes the fixed
  # spot: centred, under the navbar.
  box <- menu_box(app)
  expect_identical(box$caption, "Add a block")
  expect_lt(abs(box$center - box$viewport / 2), 2)
  expect_lt(box$top, 100)

  app$run_js(
    paste0(
      "var f = document.querySelector('body > .blockr-menu ",
      ".blockr-menu__filter-input');",
      "f.value = 'head';",
      "f.dispatchEvent(new Event('input', {bubbles: true}));"
    )
  )
  press(app, "Enter")

  app$wait_for_value(
    export = fixture("blocks"),
    ignore = list(c("a", "b", "m", "r", "s"))
  )

  added <- setdiff(exported(app, "blocks"), c("a", "b", "m", "r", "s"))
  expect_length(added, 1L)
})

test_that("append opens the + menu at the block's menu button", {

  skip_on_cran()

  app <- menus_app("plus-menu-anchor")
  withr::defer(app$stop())

  btn <- app$get_js(paste0(shown_menu_btn, ".id"))

  # From the keyboard, as the one path the pointer cannot stand in for: the
  # down arrow opens the "…" menu on its first row, Controls.
  app$run_js(sprintf("document.getElementById('%s').focus()", btn))
  press(app, "ArrowDown")
  wait_sel(app, menu_sel)
  press(app, "ArrowDown")
  press(app, "ArrowDown")
  press(app, "Enter")

  wait_js(
    app,
    paste0(
      "(function(){var c = document.querySelector(",
      "'body > .blockr-menu .blockr-menu__caption');",
      "return c !== null && c.textContent.indexOf('Append to') === 0;})()"
    ),
    function() "[plus-menu] no append menu"
  )

  box <- menu_box(app, sprintf("document.getElementById('%s')", btn))
  expect_gte(box$top, box$anchorBottom)
  expect_lt(box$top - box$anchorBottom, 12)

  # Escape hands the focus back to the button the gesture started on.
  press(app, "Escape")
  wait_sel(app, menu_sel, present = FALSE)
  expect_identical(app$get_js("document.activeElement.id"), btn)
})

test_that("Rename in the block menu opens the title's field", {

  skip_on_cran()

  app <- menus_app("menu-rename")
  withr::defer(app$stop())

  btn <- app$get_js(paste0(shown_menu_btn, ".id"))
  field <- sub("block_menu$", "block_name_in", btn)

  app$run_js(sprintf("document.getElementById('%s').click()", btn))
  wait_sel(app, menu_sel)

  app$run_js(
    paste0(
      "[...document.querySelectorAll('", menu_sel, " .blockr-menu__item')]",
      ".find(r => r.textContent.trim() === 'Rename').click()"
    )
  )

  wait_js(
    app,
    sprintf("document.activeElement.id === '%s'", field),
    function() {
      paste("[rename] focus on", app$get_js("document.activeElement.id"))
    }
  )

  expect_identical(app$get_js("document.activeElement.id"), field)
})

test_that("the controls toggle in the block menu flips the card's section", {

  skip_on_cran()

  app <- menus_app("section-toggle")
  withr::defer(app$stop())

  btn <- app$get_js(paste0(shown_menu_btn, ".id"))
  sections <- sub("block_menu$", "collapse_blk_sections", btn)

  open <- function() {
    app$get_js(
      sprintf(
        "document.getElementById('%s').getAttribute('data-sections')",
        sections
      )
    )
  }

  reported <- function() {
    app$get_js(
      sprintf(
        "JSON.stringify(Shiny.shinyapp.$inputValues['%s'])",
        sections
      )
    )
  }

  expect_identical(open(), "inputs outputs")

  app$run_js(sprintf("document.getElementById('%s').click()", btn))
  wait_sel(app, menu_sel)

  expect_identical(
    app$get_js(
      paste0(
        "document.querySelector('body > .blockr-menu .blockr-menu__item')",
        ".getAttribute('aria-checked')"
      )
    ),
    "true"
  )

  wait_reported <- function(value) {
    wait_js(
      app,
      sprintf(
        "JSON.stringify(Shiny.shinyapp.$inputValues['%s']) === '%s'",
        sections, value
      ),
      function() paste("[sections] reported", reported())
    )
  }

  # The card folds the section itself, without waiting for the server.
  wait_hidden <- function(section) {
    sel <- sprintf(
      "#%s .blockr-block-section[data-value=%s]",
      sub("collapse_blk_sections$", "blk_sections", sections), section
    )
    wait_js(
      app,
      sprintf("document.querySelector('%s').hidden === true", sel),
      function() paste("[sections] still open:", section)
    )
  }

  click_sel(app, paste(menu_sel, ".blockr-menu__item"))

  expect_identical(open(), "outputs")
  wait_reported("[\"outputs\"]")
  wait_hidden("inputs")

  # The preview keeps its own button.
  click_sel(app, sprintf("#%s [data-section=outputs]", sections))

  expect_identical(open(), "")
  wait_reported("null")
  wait_hidden("outputs")
})

test_that("the options sidebar pages back to its list", {

  skip_on_cran()

  app <- menus_app("options-pages")
  withr::defer(app$stop())

  sidebar <- "my_board-settings_sidebar"
  theme_row <- ".blockr-options-row[data-category=\"Theme options\"]"

  state <- function() {
    app$get_js(
      sprintf(
        paste0(
          "(function(){var s = document.getElementById('%s');",
          "return {open: s.classList.contains('blockr-sidebar-open'),",
          "paged: s.classList.contains('blockr-sidebar-paged'),",
          "title: s.querySelector('.blockr-sidebar-title').textContent,",
          "focus: document.activeElement.getAttribute('data-category')};})()"
        ),
        sidebar
      )
    )
  }

  click_sel(app, "button[aria-label=\"Board options\"]")
  wait_panel(app, sidebar, open = TRUE)

  click_sel(app, theme_row)
  expect_mapequal(
    state()[c("open", "paged", "title")],
    list(open = TRUE, paged = TRUE, title = "Theme options")
  )

  # Escape on a page goes back to the list, onto the row it came from, so
  # the next Escape closes the sidebar.
  press(app, "Escape")
  expect_mapequal(
    state(),
    list(
      open = TRUE, paged = FALSE, title = "Board options",
      focus = "Theme options"
    )
  )

  press(app, "Escape")
  wait_panel(app, sidebar, open = FALSE)

  # Closing on a page opens on the list next time.
  click_sel(app, "button[aria-label=\"Board options\"]")
  wait_panel(app, sidebar, open = TRUE)
  click_sel(app, theme_row)
  click_sel(app, paste0("#", sidebar, " .blockr-sidebar-close"))
  wait_panel(app, sidebar, open = FALSE)

  expect_mapequal(
    state()[c("paged", "title")],
    list(paged = FALSE, title = "Board options")
  )
})

test_that("Show code in the options sidebar opens the code page", {

  skip_on_cran()

  app <- menus_app("show-code")
  withr::defer(app$stop())

  click_sel(app, "button[aria-label=\"Board options\"]")
  wait_panel(app, "my_board-settings_sidebar", open = TRUE)
  click_sel(app, "#my_board-settings_sidebar .blockr-options-code")

  # The page asks for every block to be built, then shows the script or says
  # why it cannot; no dialog opens.
  wait_sel(
    app,
    paste(
      "#my_board-settings_sidebar .blockr-options-code-page:not([hidden])",
      ":is(.blockr-code-script, .blockr-code-note)"
    )
  )
  expect_identical(
    app$get_js(
      paste0(
        "document.querySelector('#my_board-settings_sidebar ",
        ".blockr-sidebar-title').textContent"
      )
    ),
    "Code"
  )
  expect_identical(app$get_js("document.querySelectorAll('.modal').length"), 0L)
})

test_that("the compact switch turns the headers into eyebrows", {

  skip_on_cran()

  app <- menus_app("compact-switch")
  withr::defer(app$stop())

  compact <- function() {
    app$get_js("document.documentElement.classList.contains('blockr-compact')")
  }

  expect_false(compact())

  click_sel(app, "button[aria-label=\"Board options\"]")
  wait_panel(app, "my_board-settings_sidebar", open = TRUE)
  click_sel(app, ".blockr-options-row[data-category=\"Theme options\"]")
  click_sel(app, "#my_board-compact")

  wait_js(
    app,
    "document.documentElement.classList.contains('blockr-compact')",
    function() "[compact] the root never took .blockr-compact"
  )
  expect_true(compact())
})
