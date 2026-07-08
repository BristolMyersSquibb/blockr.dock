# Package index

## Dock Board

A `board` class that includes dock layout information and board
extension object.

- [`new_dock_board()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/dock.md)
  [`is_dock_board()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/dock.md)
  [`as_dock_board()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/dock.md)
  [`dock_extensions()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/dock.md)
  [`` `dock_extensions<-`() ``](https://bristolmyerssquibb.github.io/blockr.dock/reference/dock.md)
  [`dock_ext_ids()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/dock.md)
  [`dock_board_options()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/dock.md)
  : Dock board
- [`new_dock_stack()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/stack.md)
  [`is_dock_stack()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/stack.md)
  [`stack_color()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/stack.md)
  [`suggest_new_colors()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/stack.md)
  [`` `stack_color<-`() ``](https://bristolmyerssquibb.github.io/blockr.dock/reference/stack.md)
  [`as_dock_stack()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/stack.md)
  : Colored stacks

## Dock layout

Tools for defining panel arrangements.

- [`dock_grid()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/layout.md)
  [`panels()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/layout.md)
  [`group()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/layout.md)
  [`default_layout()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/layout.md)
  : Dock grid authoring

## Dock grid

A view’s geometry in canonical form.

- [`as_dock_grid()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/dock-grid.md)
  [`is_dock_grid()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/dock-grid.md)
  [`validate_dock_grid()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/dock-grid.md)
  : Canonical view grid

## Dock layout boundary

dockView’s native layout representation, the wire form at the client
boundary.

- [`new_dock_layout()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/dock-layout.md)
  [`is_dock_layout()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/dock-layout.md)
  [`validate_dock_layout()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/dock-layout.md)
  [`as_dock_layout()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/dock-layout.md)
  : Dock layout: dockView's native representation

## Panel IDs

The canonical panel IDs a grid or layout references.

- [`panel_obj_ids()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/panel-ids.md)
  [`layout_panel_ids()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/panel-ids.md)
  : Panel IDs of a grid or layout

## Dock views

Multi-view (tabbed) boards. Each view is its own panel arrangement.

- [`is_dock_grids()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/view.md)
  [`validate_dock_grids()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/view.md)
  [`board_grids()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/view.md)
  [`` `board_grids<-`() ``](https://bristolmyerssquibb.github.io/blockr.dock/reference/view.md)
  [`as_dock_view()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/view.md)
  [`dock_view()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/view.md)
  [`is_dock_view()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/view.md)
  [`validate_dock_view()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/view.md)
  [`view_members()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/view.md)
  [`is_dock_views()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/view.md)
  [`validate_dock_views()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/view.md)
  [`view_name()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/view.md)
  [`` `view_name<-`() ``](https://bristolmyerssquibb.github.io/blockr.dock/reference/view.md)
  [`view_names()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/view.md)
  [`active_view()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/view.md)
  [`` `active_view<-`() ``](https://bristolmyerssquibb.github.io/blockr.dock/reference/view.md)
  [`board_views()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/view.md)
  [`` `board_views<-`() ``](https://bristolmyerssquibb.github.io/blockr.dock/reference/view.md)
  : Dock views: structure and grid

## Board extensions

Board extensions allow for adding components that interact with and
modify board state.

- [`new_dock_extension()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/extension.md)
  [`is_dock_extension()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/extension.md)
  [`validate_extension()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/extension.md)
  [`extension_ui()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/extension.md)
  [`extension_server()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/extension.md)
  [`extension_id()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/extension.md)
  [`extension_name()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/extension.md)
  [`extension_description()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/extension.md)
  [`extension_ctor()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/extension.md)
  [`new_dock_extensions()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/extension.md)
  [`is_dock_extensions()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/extension.md)
  [`validate_extensions()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/extension.md)
  [`as_dock_extensions()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/extension.md)
  [`extension_block_callback()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/extension.md)
  : Dock extensions
- [`new_edit_board_extension()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/edit.md)
  : Edit board extension

## Board actions

Actions are intended as re-usable, narrowly scoped board interactions,
such as adding or removing a block.

- [`new_action()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/action.md)
  [`is_action()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/action.md)
  [`is_action_generator()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/action.md)
  [`action_id()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/action.md)
  [`board_actions()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/action.md)
  [`action_triggers()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/action.md)
  [`block_input_select()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/action.md)
  [`block_registry_selectize()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/action.md)
  [`board_select()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/action.md)
  : Board actions

## Utilities

Various utility functions that are exported for use in dependent
packages.

- [`show_panel()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/panel.md)
  : UI utilities
- [`blks_metadata()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/meta.md)
  [`blk_color()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/meta.md)
  [`blk_icon_data_uri()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/meta.md)
  [`block_status_badge()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/meta.md)
  : Get block metadata
- [`dock_id()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/ids.md)
  [`as_dock_panel_id()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/ids.md)
  [`as_obj_id()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/ids.md)
  [`as_block_panel_id()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/ids.md)
  [`as_ext_panel_id()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/ids.md)
  [`as_dock_handle_id()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/ids.md)
  [`as_block_handle_id()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/ids.md)
  [`as_ext_handle_id()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/ids.md)
  [`as_view_handle_id()`](https://bristolmyerssquibb.github.io/blockr.dock/reference/ids.md)
  : ID utilities
