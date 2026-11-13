library(blockr.dock)

blockr.core::serve(
  blockr.core::new_board(),
  plugins = data.frame(package = "blockr.dock", id = "dock_plugin")
)
