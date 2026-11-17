library(blockr.core)
library(blockr.dock)

# A view whose grid gives its splits sizes of its own, 0.3 and 0.7.
# A default grid gives equal sizes, so this one shows whether a gesture
# writes back the sizes that dockview shows.
serve(
  new_board(
    blocks = c(a = new_dataset_block(), b = new_dataset_block()),
    plugins = list(
      new_dock_plugin(
        views = list(Main = dock_view(dock_grid("a", "b", sizes = c(0.3, 0.7))))
      )
    ),
    id = "my_board"
  )
)
