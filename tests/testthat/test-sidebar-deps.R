# Each dependency hard-codes a stylesheet and a script path. Nothing else
# checks those files are on disk, so a renamed asset leaves the page
# rendering unstyled and unbound with every other test still green.
sidebar_deps <- function() {
  list(
    sidebar_dep(),
    block_browser_dep()
  )
}

# The asset paths that a dependency names but does not have, reported as
# "<dep>: <path>" so a failure says which file went missing rather than
# just that one did.
absent_assets <- function(dep) {

  root <- system.file(dep[["src"]][["file"]], package = dep[["package"]])
  named <- c(dep[["stylesheet"]], dep[["script"]])

  if (!nzchar(root)) {
    return(paste0(dep[["name"]], ": <unresolved src>"))
  }

  paste0(dep[["name"]], ": ", named)[
    !file.exists(file.path(root, named))
  ]
}

dep_names <- function(tag) {
  chr_xtr(htmltools::findDependencies(tag), "name")
}

two_block_board <- function() {
  new_dock_board(
    c(a = new_dataset_block("iris"), b = new_head_block())
  )
}

test_that("every sidebar dependency names assets that exist", {
  expect_identical(
    unlst(lapply(sidebar_deps(), absent_assets)),
    character()
  )
})

test_that("a side panel attaches the dependency it draws with", {
  expect_setequal(dep_names(sidebar_ui("panel")), "sidebar-server")
})

test_that("a sidebar body's own dependencies survive the panel wrapper", {

  body <- htmltools::attachDependencies(
    htmltools::div("body"),
    action_menu_dep()
  )

  expect_setequal(
    dep_names(sidebar_ui("panel", ui = body)),
    c("sidebar-server", "blockr-action-menu")
  )
})
