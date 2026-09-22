# Demo board for the view-chapter nav. Run with
#   Rscript dev/chapters-demo.R            # navbar dropdown
#   BLOCKR_VIEW_NAV=sidebar Rscript dev/chapters-demo.R
pkgload::load_all(".", quiet = TRUE)
library(blockr.core)

blk <- function(n) {
  stats::setNames(
    lapply(seq_len(n), function(i) new_dataset_block("iris")),
    paste0("blk_", seq_len(n))
  )
}

view_of <- function(i, nm, ch = NULL) {
  dock_view(paste0("blk_", i), name = nm, chapter = ch)
}

brd <- new_dock_board(
  blocks = do.call(c, blk(9)),
  views = list(
    demog = view_of(1, "Demographics", "Setup"),
    dispo = view_of(2, "Disposition", "Setup"),
    expo  = view_of(3, "Exposure", "Setup"),
    ae    = view_of(4, "AE overview", "Safety"),
    soc   = view_of(5, "AE by SOC", "Safety"),
    labs  = view_of(6, "Lab overview", "Safety"),
    hep   = view_of(7, "Hepatic", c("Safety", "Lab overview")),
    resp  = view_of(8, "Response", "Efficacy"),
    appx  = view_of(9, "Appendix")
  ),
  active = "labs"
)

options(blockr.view_nav = Sys.getenv("BLOCKR_VIEW_NAV", "dropdown"))

port <- as.integer(Sys.getenv("DEMO_PORT", "3841"))
message("listening on ", port)
shiny::runApp(
  serve(brd), port = port, host = "0.0.0.0", launch.browser = FALSE
)
