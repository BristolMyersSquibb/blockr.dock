library(blockr.core)
library(blockr.dock)

# A dev-mode commit budget turns on the hidden commit probe (a live readout of
# the monotonic board-commit count) and aborts only a genuine spin; the generous
# bound never trips a legitimate gesture. The commit-sentinel e2e reads the
# probe to assert a gesture commits and quiescence adds nothing.
options(blockr.dock_commit_budget = 64)

serve(
  new_dock_board(
    extensions = new_edit_board_extension()
  ),
  "my_board"
)
