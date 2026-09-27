# Extracted from test-board-ui.R:430

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "blockr.dock", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
dock_css <- function() {
  paste(
    readLines(
      system.file(
        "assets", "css", "blockr-dock.css",
        package = "blockr.dock",
        mustWork = TRUE
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )
}

# test -------------------------------------------------------------------------
ui <- board_ui(
    "test",
    new_dock_board(blocks = c(a = new_dataset_block()))
  )
deps <- chr_xtr(htmltools::findDependencies(ui), "name")
expect_true("blockr-theme" %in% deps)
expect_lt(match("blockr-theme", deps), match("blockr-fab", deps))
css <- paste(
    readLines(
      system.file(
        "assets", "css", "blockr-dock.css",
        package = "blockr.dock",
        mustWork = TRUE
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )
expect_no_match(css, "(?m)^:root", perl = TRUE)
expect_no_match(css, "(?m)^\\s*--blockr-[a-z0-9-]+\\s*:", perl = TRUE)
