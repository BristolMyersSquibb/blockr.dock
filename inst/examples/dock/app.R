library(blockr.core)
library(blockr.dock)
library(blockr.dag)
library(blockr.session)

serve(
  new_dock_board(
    extensions = list(
      blockr.dag::new_dag_extension(),
      new_session_extension()
    )
  )
)
