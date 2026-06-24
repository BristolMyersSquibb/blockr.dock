# Panel navigator demo (feat/panel-navigator-dock).
#
# A dock board with several blocks, two stacks, and two views so the
# navigator shows: stack grouping, an Ungrouped bucket, per-row eye state
# reflecting the active view, and "also on <view>" tags.
#
#   Rscript dev/panel-navigator-demo.R   # serves on 3838
pkgload::load_all("../blockr.core", quiet = TRUE)
pkgload::load_all("../blockr.ui", quiet = TRUE)
pkgload::load_all(".", quiet = TRUE)

library(blockr.dplyr)

board <- new_dock_board(
  blocks = c(
    data = new_dataset_block("iris"),
    sel  = new_select_block(),
    flt  = new_filter_block(),
    summ = new_select_block(),
    data2 = new_dataset_block("mtcars")
  ),
  links = list(
    new_link("data", "sel"),
    new_link("sel", "flt")
  ),
  stacks = list(
    pipeline = new_dock_stack(c("data", "sel", "flt"), name = "Pipeline"),
    summaries = new_dock_stack(c("summ"), name = "Summaries")
  ),
  layouts = list(
    main = dock_layout("data", "sel", "flt", name = "Main"),
    review = dock_layout("summ", "data2", name = "Review")
  ),
  active = "main"
)

shiny::runApp(
  serve(board),
  port = 3838, host = "0.0.0.0", launch.browser = FALSE
)
