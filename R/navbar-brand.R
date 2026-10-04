# The navbar's first item: the blockr mark. It is always drawn, and it is the
# board's busy indicator (see "Navbar mark" in blockr-dock.css), so a board
# without any plugin keeps both. A plugin may hang a menu on it: an element of
# its UI with the class `blockr-navbar-brand-menu` (a Bootstrap
# `.dropdown-menu`) is taken out of the plugin's UI and placed under the mark,
# which then becomes that menu's toggle. blockr.session puts its workflows
# menu there.

brand_menu_class <- "blockr-navbar-brand-menu"

#' Split a plugin's navbar UI into its brand menu and the rest
#' @noRd
split_brand_menu <- function(ui) {

  if (is.null(ui)) {
    return(list(menu = NULL, rest = NULL))
  }

  sel <- paste0(".", brand_menu_class)
  menu <- htmltools::tagQuery(ui)$find(sel)$selectedTags()

  if (!length(menu)) {
    return(list(menu = NULL, rest = ui))
  }

  list(
    menu = menu[[1L]],
    rest = htmltools::tagQuery(ui)$find(sel)$remove()$allTags()
  )
}

#' The mark that leads the navbar, with or without a menu
#' @noRd
navbar_brand_ui <- function(menu = NULL) {

  # The status region the old ring carried, for screen readers
  status <- tags$span(class = "visually-hidden", role = "status",
                      `aria-label` = "Busy")

  if (is.null(menu)) {
    return(
      tags$span(
        class = "blockr-navbar-brand",
        tags$span(class = "blockr-navbar-mark", blockr_mark(20)),
        status
      )
    )
  }

  tags$div(
    class = "blockr-navbar-brand dropdown",
    tags$button(
      class = "blockr-navbar-mark blockr-navbar-mark-btn",
      type = "button",
      `data-bs-toggle` = "dropdown",
      `data-bs-auto-close` = "outside",
      `aria-expanded` = "false",
      `aria-label` = "Workflows",
      blockr_mark(20)
    ),
    menu,
    status
  )
}

#' The blockr mark: a 3 x 3 grid with two cells left out
#'
#' The squares are listed in the order the R is drawn in one stroke (up the
#' stem, across the top, back to the middle, out the leg), and each carries
#' its place as `--i`, which the busy animation staggers on.
#' @noRd
blockr_mark <- function(size = 20) {

  # (column, row) in stroke order
  cells <- list(c(0, 2), c(0, 1), c(0, 0), c(1, 0), c(2, 0), c(1, 1), c(2, 2))

  rects <- vapply(
    seq_along(cells),
    function(i) {
      sprintf(
        '<rect style="--i:%d" x="%d" y="%d" width="64" height="64" rx="7"/>',
        i - 1L, cells[[i]][1L] * 80L, cells[[i]][2L] * 80L
      )
    },
    character(1L)
  )

  HTML(
    sprintf(
      paste0(
        '<svg class="blockr-mark" width="%d" height="%d" viewBox="0 0 224 224"',
        ' aria-hidden="true">%s</svg>'
      ),
      size, size, paste(rects, collapse = "")
    )
  )
}
