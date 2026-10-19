test_that("Import and Export are one row of quiet 30px buttons (#531)", {

  ui <- ser_deser_ui("ser", new_board())

  root <- xml2::read_html(as.character(htmltools::tagList(ui)))

  btns <- xml2::xml_find_all(
    root,
    paste0("//div[", has_class("blockr-serdes"), "]/*")
  )

  expect_identical(xml2::xml_name(btns), c("label", "a"))
  expect_identical(trimws(xml2::xml_text(btns)), c("Import", "Export"))

  cls <- strsplit(xml2::xml_attr(btns, "class"), " ", fixed = TRUE)

  for (x in cls) {
    expect_contains(x, c("blockr-btn", "blockr-btn--quiet", "blockr-btn--s"))
    expect_false(any(c("btn", "btn-default", "btn-sm") %in% x))
  }

  # Import is a label around the file input whose id blockr.core's server
  # reads, with nothing else of `fileInput()`: no field label, text field or
  # progress bar.
  file <- xml2::xml_find_all(root, "//input")

  expect_length(file, 1L)
  expect_identical(xml2::xml_attr(file, "type"), "file")
  expect_identical(xml2::xml_attr(file, "id"), "ser-restore")
  expect_identical(xml2::xml_parent(file[[1L]]), btns[[1L]])

  expect_length(xml2::xml_find_all(root, "//label"), 1L)
  expect_length(
    xml2::xml_find_all(root, paste0("//*[", has_class("progress"), "]")),
    0L
  )

  # Export is Shiny's download link, under the id the server fills. The link
  # holds its name and no icon, which the stylesheet used to hide (#72).
  expect_identical(xml2::xml_attr(btns[[2L]], "id"), "ser-serialize")
  expect_contains(cls[[2L]], "shiny-download-link")
  expect_identical(xml2::xml_name(xml2::xml_children(btns[[2L]])), "span")
})

# Chrome reports a file chooser to the test as an event in place of opening
# it, and reports no other until that one is answered, which turning the
# interception off and on again does.
intercept_file_chooser <- function(app) {
  cdp <- app$get_chromote_session()
  cdp$Page$setInterceptFileChooserDialog(enabled = FALSE)
  cdp$Page$setInterceptFileChooserDialog(enabled = TRUE)
}

# The id of the file input whose chooser `trigger()` opens, or `NULL` if none
# opens. The input events go out without waiting for Chrome's reply, which an
# opening chooser holds back.
file_chooser_for <- function(app, trigger) {

  cdp <- app$get_chromote_session()

  opened <- cdp$Page$fileChooserOpened(timeout_ = 10, wait_ = FALSE)
  trigger()

  res <- tryCatch(cdp$wait_for(opened), error = function(e) NULL)

  if (is.null(res)) {
    return(NULL)
  }

  attrs <- unlst(
    cdp$DOM$describeNode(backendNodeId = res$backendNodeId)$node$attributes
  )

  attrs[c(FALSE, TRUE)][match("id", attrs[c(TRUE, FALSE)])]
}

press_key <- function(app, key, text = NULL, modifiers = 0L) {

  vk <- c(Tab = 9L, Enter = 13L)[[key]]

  for (type in c("keyDown", "keyUp")) {
    app$get_chromote_session()$Input$dispatchKeyEvent(
      type = type,
      key = key,
      code = key,
      windowsVirtualKeyCode = vk,
      nativeVirtualKeyCode = vk,
      text = if (identical(type, "keyDown")) text,
      modifiers = modifiers,
      wait_ = FALSE
    )
  }
}

# A click through Chrome's input events rather than a scripted one, which
# carries no user activation, and so opens no file chooser.
click_centre <- function(app, selector) {

  pos <- app$get_js(
    sprintf(
      paste0(
        "(function() {",
        "  var r = document.querySelector('%s').getBoundingClientRect();",
        "  return {x: r.left + r.width / 2, y: r.top + r.height / 2};",
        "})()"
      ),
      selector
    )
  )

  for (type in c("mousePressed", "mouseReleased")) {
    app$get_chromote_session()$Input$dispatchMouseEvent(
      type = type,
      x = pos$x,
      y = pos$y,
      button = "left",
      clickCount = 1L,
      wait_ = FALSE
    )
  }
}

test_that("Import opens the file chooser, by click and by key (#531)", {

  skip_on_cran()

  app <- new_app_driver(
    system.file("examples", "empty", "app.R", package = "blockr.dock"),
    name = "serdes-import",
    seed = 42,
    load_timeout = 30 * 1000,
    timeout = 20 * 1000
  )
  withr::defer(app$stop())

  wait_dock_loaded(app, 0)

  import <- ".blockr-serdes > label"
  restore <- "my_board-preserve_board-restore"
  serialize <- "my_board-preserve_board-serialize"

  outline <- function() {
    app$get_js(
      sprintf(
        "getComputedStyle(document.querySelector('%s')).outlineStyle",
        import
      )
    )
  }

  intercept_file_chooser(app)

  expect_identical(
    file_chooser_for(app, function() click_centre(app, import)),
    restore
  )

  # The click leaves the file input with the focus, and no ring on Import.
  expect_identical(app$get_js("document.activeElement.id"), restore)
  expect_identical(outline(), "none")

  intercept_file_chooser(app)

  # The file input is out of sight but not out of the keyboard's reach:
  # Shift+Tab from Export, which takes the focus once its output binds, lands
  # on it, and Import draws the focus ring.
  app$wait_for_js(
    sprintf(
      "!document.getElementById('%s').hasAttribute('tabindex')",
      serialize
    )
  )
  app$run_js(sprintf("document.getElementById('%s').focus()", serialize))

  press_key(app, "Tab", modifiers = 8L)
  app$wait_for_js(sprintf("document.activeElement.id === '%s'", restore))

  expect_identical(outline(), "solid")

  expect_identical(
    file_chooser_for(app, function() press_key(app, "Enter", text = "\r")),
    restore
  )
})
