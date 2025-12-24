devtools::load_all(".")
devtools::load_all("../blockr.session")
devtools::load_all("../blockr.dag")

library(blockr)

# Create a dock board with DAG extension (like run_app does)
board <- new_dock_board(
  extensions = list(
    blockr.dag::new_dag_extension()
  )
)

# Serve with manage_session plugin (replaces preserve_board)
serve(board, plugins = custom_plugins(manage_session()))
