# the plugin prints its views and folders in the views menu's order

    Code
      print(dock)
    Output
      <dock_plugin> blockr.dock-dock_plugin
      <dock_view> Main
      <dock_grid> horizontal
      ├─ tabs (30%)
      │  ├─ a (active)
      │  └─ b
      └─ c (70%)
      left rail
      └─ dag (plugin)
      <dock_folder> Safety
      ├─ <dock_view> Adverse events (active)
      │  <dock_grid>
      │  └─ d
      └─ <dock_folder> Labs
         └─ <dock_view> Lab
            <dock_grid>
            └─ tabs
               ├─ e (active)
               └─ f
      visible
      ├─ b (outputs)
      └─ c (none)

---

    Code
      print(new_dock_plugin(visible = list(a = "inputs")))
    Output
      <dock_plugin> blockr.dock-dock_plugin
      visible
      └─ a (inputs)

