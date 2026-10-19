# The dock's piece of the `preserve_board` plugin: Import and Export as one row
# of quiet 30px buttons, the design spec's toolbar size, with no field label.
# Import is a label around a file input, so that a click on it opens the
# browser's file chooser, as in Shiny's `fileInput()`, which also draws a text
# field and a progress bar. The input is hidden from sight only, and the
# keyboard and screen readers still reach it. Both keep the ids blockr.core's
# `preserve_board_server()` reads. Each name sits in a span of its own, so that
# the stylesheet can cut it short on a narrow bar.
ser_deser_ui <- function(id, board) {

  btn <- c("blockr-btn", "blockr-btn--quiet", "blockr-btn--s")

  div(
    class = "blockr-serdes",
    tags$label(
      class = btn,
      tags$input(
        id = NS(id, "restore"),
        type = "file",
        class = "visually-hidden"
      ),
      tags$span("Import")
    ),
    htmltools::tagQuery(
      downloadButton(NS(id, "serialize"), tags$span("Export"), icon = NULL)
    )$removeClass(c("btn", "btn-default"))$addClass(btn)$allTags()
  )
}
