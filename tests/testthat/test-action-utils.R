test_that("action utils", {

  inps <- block_input_select(
    new_rbind_block(),
    "test",
    links(),
    mode = "inputs"
  )

  expect_identical(inps, "")
})

selectize_config <- function(x) {

  script <- htmltools::tagQuery(x)$find("script")$selectedTags()[[1L]]

  jsonlite::fromJSON(
    as.character(script$children[[1L]]),
    simplifyVector = FALSE
  )
}

select_attribs <- function(x) {
  htmltools::tagQuery(x)$find("select")$selectedTags()[[1L]]$attribs
}

picker_board <- function() {
  new_board(
    c(
      a = new_dataset_block("iris"),
      b = new_dataset_block("mtcars"),
      c = new_dataset_block("airquality")
    )
  )
}

test_that("the board block picker offers every block, styling attached", {

  ui <- board_block_select("pick", picker_board(), label = "Source block")
  cfg <- selectize_config(ui)

  expect_identical(chr_xtr(cfg$options, "value"), c("a", "b", "c"))
  expect_identical(cfg$valueField, "value")
  expect_identical(cfg$labelField, "label")
  expect_identical(
    unlist(cfg$searchField),
    c("label", "description", "searchtext")
  )
  expect_true(is.character(cfg$render))

  html <- as.character(htmltools::renderTags(ui)$html)

  expect_match(html, ".block-option {", fixed = TRUE)
  expect_match(html, "Source block", fixed = TRUE)
})

test_that("the picker offers only the blocks it is given", {

  cfg <- selectize_config(
    board_block_select("pick", picker_board(), c("c", "a"))
  )

  expect_identical(chr_xtr(cfg$options, "value"), c("c", "a"))
})

test_that("the item cap decides single versus multi select", {

  brd <- picker_board()

  single <- board_block_select("pick", brd)
  multi <- board_block_select("pick", brd, max_items = NULL)

  expect_null(select_attribs(single)[["multiple"]])
  expect_identical(select_attribs(multi)[["multiple"]], "multiple")

  expect_equal(selectize_config(single)$maxItems, 1)
  expect_null(selectize_config(multi)$maxItems)
})

test_that("the picker preselects, ignoring an empty or missing selection", {

  brd <- picker_board()

  sel <- function(selected, ...) {
    selectize_config(board_block_select("pick", brd, selected = selected, ...))
  }

  expect_identical(unlist(sel("b")$items), "b")
  expect_identical(
    unlist(sel(c("a", "c"), max_items = NULL)$items),
    c("a", "c")
  )

  expect_length(sel(NULL)$items, 0L)
  expect_length(sel("")$items, 0L)
  expect_length(sel(NA_character_)$items, 0L)
})

test_that("caller options merge over the picker defaults", {

  cfg <- selectize_config(
    board_block_select(
      "pick",
      picker_board(),
      "a",
      options = list(
        placeholder = "Choose...",
        plugins = list("remove_button"),
        dropdownParent = "body"
      )
    )
  )

  expect_identical(cfg$placeholder, "Choose...")
  expect_identical(cfg$dropdownParent, "body")
  expect_true("remove_button" %in% unlist(cfg$plugins))

  expect_identical(chr_xtr(cfg$options, "value"), "a")
  expect_identical(cfg$valueField, "value")
})

test_that("the inputs sidebar picker keeps its own selectize options", {

  cfg <- selectize_config(
    edit_inputs_source_select("src", picker_board(), c("a", "b"), "a")
  )

  expect_true("remove_button" %in% unlist(cfg$plugins))
  expect_identical(cfg$dropdownParent, "body")
  expect_equal(cfg$maxItems, 1)
  expect_identical(unlist(cfg$items), "a")
  expect_identical(chr_xtr(cfg$options, "value"), c("a", "b"))
})
