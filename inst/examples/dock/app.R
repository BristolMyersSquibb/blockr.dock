devtools::load_all(".")
devtools::load_all("../blockr.session")
devtools::load_all("../blockr.dag")
devtools::load_all("../blockr.ai")

library(blockr)

options(
  "g6R.mode" = "dev",
  "g6R.layout_on_data_change" = TRUE
)


# Create a dock board with DAG extension (like run_app does)
board <- new_dock_board(
  blocks = c(
    data = new_dataset_block(dataset = "mtcars"),
    filter = new_filter_block(),
    mutate = new_mutate_block(),
    summarize = new_summarize_block(),
    longer = new_pivot_longer_block(),
    wider = new_pivot_wider_block(),
    code = new_code_block()
  ),
  links = c(
    new_link("data", "filter", "data"),
    new_link("data", "mutate", "data"),
    new_link("data", "summarize", "data"),
    new_link("data", "longer", "data"),
    new_link("data", "wider", "data"),
    new_link("data", "code", "data")
  ),
  extensions = list(
    blockr.dag::new_dag_extension()
  )
)

# Serve with manage_session plugin (replaces preserve_board)
serve(board, plugins = custom_plugins(manage_session()))
