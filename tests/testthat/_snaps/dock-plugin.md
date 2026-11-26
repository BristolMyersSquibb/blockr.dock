# the plugin prints its views and folders in the views menu's order

    Code
      print(dock)
    Output
      <dock_plugin> blockr.dock-dock_plugin (sections: i inputs, o outputs, n neither, unmarked both)
      ├─ main
      └─ <dock_folder> Safety
         ├─ ae (active)
         └─ <dock_folder> Labs
            └─ lab
      main<dock_view> Main (horizontal)
      ├─ 30%: a (active), b (o)
      └─ 70%: c (n)
      left rail: edit (plugin)
      ae<dock_view> Adverse events
      └─ d
      lab<dock_view> Lab
      └─ e (active), f

# the print marks the sections of each card that hides one

    Code
      print(dock)
    Output
      <dock_plugin> blockr.dock-dock_plugin (sections: i inputs, o outputs, n neither, unmarked both)
      └─ main (active)
      main<dock_view> Main
      └─ a, edit (plugin), b (active; o)
      left rail (collapsed): c (i)
      cards in no view: z (n)

---

    Code
      print(new_dock_plugin(visible = list(a = "inputs")))
    Output
      <dock_plugin> blockr.dock-dock_plugin (sections: i inputs, o outputs, n neither, unmarked both)
      cards in no view: a (i)

