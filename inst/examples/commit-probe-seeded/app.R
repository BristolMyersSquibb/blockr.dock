library(blockr.core)
library(blockr.dock)

# A board authored with a NON-default layout (explicit sizes): the round-trip
# stability sentinel compares the stored authored grid against the projection of
# the live layout, and a plain default would elide both sides to NULL and pass
# vacuously. The budget option also turns on the probe outputs.
options(blockr.dock_commit_budget = 64)

serve(
  new_dock_board(
    blocks = c(a = new_dataset_block(), b = new_dataset_block()),
    layouts = list(Main = dock_layout("a", "b", sizes = c(0.3, 0.7)))
  ),
  "my_board"
)
