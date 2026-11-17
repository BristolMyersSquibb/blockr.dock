library(blockr.core)
library(blockr.dock)

serve(
  new_board(
    blocks = c(a = new_dataset_block(), b = new_dataset_block()),
    plugins = list(
      new_dock_plugin(
        views = list(First = dock_view(blk("a")), Second = dock_view(blk("b"))),
        active = "First"
      )
    ),
    id = "my_board"
  )
)
