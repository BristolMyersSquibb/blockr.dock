
# serve(
#   new_dock_board(
#     extensions = new_edit_board_extension()
#   ),
#   "my_board"
# )


pkgload::load_all("../blockr.dock")

library(blockr)


serve(
  new_dock_board(
    extensions = new_dag_extension()
  )
)
