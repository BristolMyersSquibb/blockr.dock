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

# The board options panel: with the link and stack actions on menus (#544),
# it is the side panel the dismissal and pin tests open.
options_panel <- "my_board-settings_sidebar"
open_options <- function(app) {
  click_sel(app, '[data-blockr-sidebar-target="my_board-settings_sidebar"]')
}

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
    panel_state(app, options_panel),
    list(open = FALSE, pinned = FALSE)
  )

  open_options(app)
  wait_panel(app, options_panel, open = TRUE)

  expect_identical(
    panel_state(app, options_panel),
    list(open = TRUE, pinned = FALSE)
  )

  press_esc(app)
  wait_panel(app, options_panel, open = FALSE)

  open_options(app)
  wait_panel(app, options_panel, open = TRUE)

  click_outside(app)
  wait_panel(app, options_panel, open = FALSE)

  expect_identical(
    panel_state(app, options_panel),
    list(open = FALSE, pinned = FALSE)
  )
})

test_that("a pinned panel survives Escape and an outside click", {

  skip_on_cran()

  app <- menus_app("sidebar-pinned")
  withr::defer(app$stop())

  open_options(app)
  wait_panel(app, options_panel, open = TRUE)

  click_sel(app, paste0("#", options_panel, " .blockr-sidebar-pin"))
  app$wait_for_idle()

  expect_identical(
    panel_state(app, options_panel),
    list(open = TRUE, pinned = TRUE)
  )

  press_esc(app)
  click_outside(app)
  app$wait_for_idle()

  expect_identical(
    panel_state(app, options_panel),
    list(open = TRUE, pinned = TRUE)
  )

  # The close button is the one dismissal that overrides a pin.
  click_sel(app, paste0("#", options_panel, " .blockr-sidebar-close"))
  wait_panel(app, options_panel, open = FALSE)

  expect_false(panel_state(app, options_panel)$open)
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
        options_panel
      )
    )
  }

  open_options(app)
  wait_panel(app, options_panel, open = TRUE)

  expect_false(pushed())

  click_sel(app, paste0("#", options_panel, " .blockr-sidebar-pin"))
  app$wait_for_idle()

  expect_true(pushed())
  expect_gt(width(), 0)

  # The variable is snapshotted at pin time and re-measured after the pin has
  # reflowed the board, so the two agree only to layout's sub-pixel rounding.
  expect_equal(width(), panel_width(), tolerance = 1e-6)

  click_sel(app, paste0("#", options_panel, " .blockr-sidebar-pin"))
  app$wait_for_idle()

  expect_false(pushed())
  expect_equal(width(), 0)
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

test_that("the rule above the preview follows the sections above it", {

  skip_on_cran()

  app <- menus_app("section-rule", "card-sections")
  withr::defer(app$stop())

  card_id <- function(blk, what) {
    paste0("my_board-block_", blk, "-edit_block-", what)
  }

  ruled <- function(blk) {
    app$get_js(
      sprintf(
        paste0(
          "getComputedStyle(document.querySelector('#%s > ",
          "[data-value=outputs] > .blockr-block-section-body'))",
          ".backgroundImage !== 'none'"
        ),
        card_id(blk, "blk_sections")
      )
    )
  }

  # The card's "…" menu toggles the inputs with this event; a second one
  # `again` ms later reverses the fold while it runs.
  toggle_inputs <- function(again = NULL) {
    app$run_js(
      sprintf(
        paste0(
          "(function () {",
          "var t = document.getElementById('%s');",
          "var go = function () {",
          "t.dispatchEvent(",
          "new CustomEvent('blockr-section:toggle', { detail: 'inputs' }));",
          "};",
          "go();%s",
          "})()"
        ),
        card_id("a", "collapse_blk_sections"),
        if (is.null(again)) "" else sprintf(" setTimeout(go, %d);", again)
      )
    )
  }

  settled <- function(sections, hidden) {
    wait_js(
      app,
      sprintf(
        paste0(
          "document.getElementById('%s').getAttribute('data-sections') ",
          "=== '%s' && !document.querySelector('#%s > .is-folding') && ",
          "document.querySelector('#%s > [data-value=inputs]').hidden === %s"
        ),
        card_id("a", "collapse_blk_sections"), sections,
        card_id("a", "blk_sections"), card_id("a", "blk_sections"),
        if (hidden) "true" else "false"
      ),
      function() paste("[sections] not settled at", sections)
    )
  }

  # An rbind block has no inputs, so nothing sits above its preview.
  expect_false(ruled("v"))
  expect_true(ruled("a"))

  toggle_inputs()
  settled("outputs", hidden = TRUE)
  expect_false(ruled("a"))

  toggle_inputs()
  settled("outputs inputs", hidden = FALSE)
  expect_true(ruled("a"))

  # Folded away and straight back, the inputs never close.
  toggle_inputs(again = 50)
  settled("outputs inputs", hidden = FALSE)
  expect_true(ruled("a"))
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

test_that("Show code in the options sidebar opens the code dialog", {

  skip_on_cran()

  app <- menus_app("show-code")
  withr::defer(app$stop())

  click_sel(app, "button[aria-label=\"Board options\"]")
  wait_panel(app, "my_board-settings_sidebar", open = TRUE)
  click_sel(app, "#my_board-settings_sidebar .blockr-options-code")

  wait_sel(app, ".modal .modal-title")
  expect_identical(
    app$get_js("document.querySelector('.modal .modal-title').textContent"),
    "Generated code"
  )
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

# The link actions open a menu in place of the sidebar form (#544).
menu_row_click <- function(app, label) {
  app$run_js(
    sprintf(
      paste0(
        "[...document.querySelectorAll('body > .blockr-menu .blockr-menu__item')]",
        ".find(r => r.textContent.trim().startsWith(%s)).click();"
      ),
      encodeString(label, quote = "'")
    )
  )
}

test_that("Connect to links a block, asking for the input where there is a choice", {

  skip_on_cran()

  app <- menus_app("connect-menu")
  withr::defer(app$stop())

  app$click(fixture("link_a"))
  wait_sel(app, menu_sel)

  expect_identical(
    app$get_js("document.querySelector('body > .blockr-menu .blockr-menu__caption').textContent"),
    "Connect Dataset"
  )

  menu_row_click(app, "Merge")
  wait_sel(app, paste(menu_sel, ".blockr-menu__caption"))
  app$wait_for_js(
    "document.querySelector('body > .blockr-menu .blockr-menu__caption').textContent.startsWith('Into which input')"
  )

  menu_row_click(app, "y")

  app$wait_for_value(export = fixture("links"), ignore = list(character()))
  expect_identical(exported(app, "links"), "a>m>y")
})

# The stack's menu (#544): Blocks… ticks members and applies them when the menu
# closes; Escape drops the ticks. Rename… opens a field in a second menu.
test_that("the stack menu's Blocks applies its ticks on an outside click only", {

  skip_on_cran()

  app <- menus_app("stack-menu-blocks")
  withr::defer(app$stop())

  open_blocks <- function() {
    app$click(fixture("edit_stack"))
    wait_sel(app, menu_sel)
    menu_row_click(app, "Blocks")
    app$wait_for_js(
      "!!document.querySelector('body > .blockr-menu.blockr-menu--multi')"
    )
  }

  open_blocks()
  menu_row_click(app, "Dataset")
  press(app, "Escape")
  app$wait_for_idle()
  expect_setequal(unlst(exported(app, "stacks")$s1), c("r", "s"))

  # Escape went back to the stack's menu; close it and start again.
  press(app, "Escape")
  open_blocks()
  before <- exported(app, "stacks")
  menu_row_click(app, "Dataset")
  # Blockr.layer reads the pointer, not the mouse.
  app$run_js(
    "document.body.dispatchEvent(new PointerEvent('pointerdown', {bubbles: true}))"
  )
  app$wait_for_value(export = fixture("stacks"), ignore = list(before))
  expect_setequal(unlst(exported(app, "stacks")$s1), c("r", "s", "a"))
})

test_that("the stack menu's Rename opens a field that renames on Enter", {

  skip_on_cran()

  app <- menus_app("stack-menu-rename")
  withr::defer(app$stop())

  before <- exported(app, "stack_names")
  app$click(fixture("edit_stack"))
  wait_sel(app, menu_sel)
  menu_row_click(app, "Rename")
  wait_sel(app, ".blockr-action-menu-field__input")

  app$run_js(
    paste0(
      "var f = document.querySelector('.blockr-action-menu-field__input');",
      "f.value = 'Heads';",
      "f.dispatchEvent(new Event('input', {bubbles: true}));"
    )
  )
  press(app, "Enter", "document.querySelector('.blockr-action-menu-field__input')")

  app$wait_for_value(export = fixture("stack_names"), ignore = list(before))
  expect_identical(unlst(exported(app, "stack_names")$s1), "Heads")
})
