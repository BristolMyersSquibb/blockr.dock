library(blockr.core)
library(blockr.dock)

# Blocks with and without inputs, for the card's section tests: a dataset
# block has controls but no inputs, the rbind block takes the three.
serve(
  new_dock_board(
    c(
      a = new_dataset_block("iris"),
      b = new_dataset_block("iris"),
      c = new_dataset_block("iris"),
      v = new_rbind_block()
    ),
    links = links(
      l1 = new_link("a", "v"),
      l2 = new_link("b", "v"),
      l3 = new_link("c", "v")
    )
  ),
  "my_board"
)
