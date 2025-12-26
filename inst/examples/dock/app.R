devtools::load_all(".")

library(blockr)
library(blockr.session)

options(
  "g6R.mode" = "dev",
  "g6R.layout_on_data_change" = TRUE
)

# Unregister all blockr.core blocks except dataset_block
# (other packages like blockr.io, blockr.dplyr, blockr.ggplot provide better alternatives)
blockr.core::unregister_blocks(c(
  "subset_block",
  "merge_block",
  "rbind_block",
  "head_block",
  "scatter_block",
  "upload_block",
  "filebrowser_block",
  "csv_block",
  "static_block",
  "glue_block"
))

# Create a dock board with DAG extension (like run_app does)
board <- new_dock_board(
  extensions = list(
    blockr.dag::new_dag_extension()
  )
)

# Serve with manage_session plugin (replaces preserve_board)
serve(board, plugins = custom_plugins(manage_session()))
