# The nav is one thing rendered in two places: a navbar dropdown or a left
# sidebar. Both read `view_tree()` and both carry the `blockr-view-nav` class
# and the `view_nav` id the input binding and the server's pushes address, so
# what is asserted here is mostly that the two stay the same nav.

chaptered_board <- function() {
  new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_head_block()),
    views = list(
      demog = dock_view("a", "Demographics", "Setup"),
      ae    = dock_view("b", "AE overview", "Safety"),
      labs  = dock_view("a", "Lab overview", "Safety"),
      hep   = dock_view("b", "Hepatic", c("Safety", "Lab overview")),
      appx  = dock_view("a", "Appendix")
    ),
    active = "labs"
  )
}

flat_board <- function() {
  new_dock_board(
    blocks = c(a = new_dataset_block()),
    views = list(one = dock_view("a", "One"), two = dock_view("a", "Two"))
  )
}

nav_html <- function(ui) {
  xml2::read_html(paste0("<body>", as.character(ui), "</body>"))
}

by_class <- function(doc, token) {
  xml2::xml_find_all(
    doc,
    sprintf(
      "//*[contains(concat(' ', normalize-space(@class), ' '), ' %s ')]", token
    )
  )
}

test_that("the dropdown heads one chapter level and leaves a flat board flat", {

  doc <- nav_html(view_nav_ui("brd", board_views(chaptered_board())))
  headers <- by_class(doc, "blockr-view-chapter")

  # One level only. A sub-chapter header in a menu sits directly under its
  # parent's last item with no gesture attached, so it reads as a repeat of
  # the page above rather than as a level; the indent states the nesting
  # instead. The sidebar, where a header is a control, heads all of them.
  expect_identical(
    xml2::xml_text(by_class(doc, "blockr-view-chapter-label")),
    c("Setup", "Safety")
  )

  # A chapter is keyed by its whole path, never by its last label: two
  # chapters may end in the same word under different parents.
  expect_identical(
    xml2::xml_attr(headers, "data-chapter-key"),
    c("Setup", "Safety")
  )

  # Headers and items are siblings in one flat list, so every gesture the
  # binding delegates keeps finding items where it always did.
  items <- by_class(doc, "blockr-view-item")
  expect_identical(
    xml2::xml_attr(items, "data-view-id"),
    c("demog", "ae", "labs", "hep", "appx")
  )
  expect_identical(
    xml2::xml_attr(items, "data-view-chapter"),
    c("Setup", "Safety", "Safety", "Safety / Lab overview", "")
  )

  # The toggle states the active view's place, since a name alone stops
  # identifying a page once two chapters can each hold an "Overview".
  expect_identical(
    xml2::xml_text(by_class(doc, "blockr-view-toggle-chapter")), "Safety"
  )
  expect_identical(
    xml2::xml_text(by_class(doc, "blockr-view-toggle-label")), "Lab overview"
  )

  # With nothing grouped the nav is the list it always was.
  plain <- nav_html(view_nav_ui("brd", board_views(flat_board())))
  expect_length(by_class(plain, "blockr-view-chapter"), 0L)
  expect_identical(
    xml2::xml_text(by_class(plain, "blockr-view-toggle-chapter")), ""
  )
})

test_that("the sidebar is the same nav in another place", {

  brd <- chaptered_board()

  drop <- nav_html(view_nav_ui("brd", board_views(brd)))
  side <- nav_html(view_sidebar_ui("brd", board_views(brd)))

  nav_id <- function(doc) {
    xml2::xml_attr(by_class(doc, "blockr-view-nav"), "id")
  }
  view_ids <- function(doc) {
    xml2::xml_attr(by_class(doc, "blockr-view-item"), "data-view-id")
  }

  # Same id, so the server's add / remove / rename / arrangement pushes land
  # in whichever surface is on screen without knowing which it is.
  expect_identical(nav_id(side), nav_id(drop))
  expect_identical(view_ids(side), view_ids(drop))

  # A header is a collapse control here, so it renders a twisty and a count,
  # and every level is worth a line; in the dropdown it is an inert label,
  # renders neither, and only the top level is headed.
  expect_length(by_class(side, "blockr-view-chapter-twisty"), 3L)
  expect_length(by_class(drop, "blockr-view-chapter"), 2L)
  expect_identical(
    xml2::xml_text(by_class(side, "blockr-view-chapter-count")),
    c("1", "3", "1")
  )
  expect_length(by_class(drop, "blockr-view-chapter-twisty"), 0L)

  # The add button sits inside the nav, because the gesture is delegated off
  # the nav element and a button outside it is never reached.
  expect_length(by_class(side, "blockr-view-nav") |> by_class("blockr-view-add"), 1L)
})

test_that("the nav mode is an app-author option, checked once", {

  expect_identical(view_nav_mode(), "dropdown")

  withr::with_options(
    list(blockr.view_nav = "sidebar"),
    expect_identical(view_nav_mode(), "sidebar")
  )

  withr::with_options(
    list(blockr.view_nav = "drawer"),
    expect_error(view_nav_mode(), class = "dock_view_nav_mode_invalid")
  )
})

test_that("board_ui renders one nav, in the place the option names", {

  brd <- chaptered_board()

  drop <- nav_html(board_ui("brd", brd))

  expect_length(by_class(drop, "blockr-view-dropdown"), 1L)
  expect_length(by_class(drop, "blockr-view-sidebar"), 0L)
  expect_length(by_class(drop, "blockr-view-crumb"), 0L)

  side <- withr::with_options(
    list(blockr.view_nav = "sidebar"), nav_html(board_ui("brd", brd))
  )

  expect_length(by_class(side, "blockr-view-sidebar"), 1L)
  expect_length(by_class(side, "blockr-view-dropdown"), 0L)

  # Exactly one nav either way: two would both be found by the binding and
  # both report.
  expect_length(by_class(side, "blockr-view-nav"), 1L)

  # The navbar keeps the active view's path where it cannot scroll away.
  expect_identical(
    xml2::xml_text(by_class(side, "blockr-view-crumb-chapter")), "Safety"
  )
  expect_identical(
    xml2::xml_text(by_class(side, "blockr-view-crumb-name")), "Lab overview"
  )
})
