library(blockr.core)
library(blockr.dock)

# A board with a link and a stack, for core's Edit board plugin to edit.
# The stack groups block `a` alone, so that removing block `b` removes its
# link but leaves the stack.
# The script gives no views, so the dock adds the view "Page" as it starts.
# That view shows the blocks' panels, with the panel of the Edit board plugin,
# which runs by default, on its left rail.
serve(
  new_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_head_block()
    ),
    links = links(ab = new_link(from = "a", to = "b", input = "data")),
    stacks = stacks(grp = new_stack(blocks = "a", name = "Group A")),
    id = "my_board"
  )
)
