# dock_grid has a tree-style print method

    Code
      print(dock_grid("a", "b"))
    Output
      <dock_grid> horizontal
      ├─ a (50%)
      └─ b (50%)

---

    Code
      print(dock_grid(group("data", "filt", "head"), "assistant_extension", sizes = c(
        0.6, 0.4)))
    Output
      <dock_grid> horizontal
      ├─ group (vertical, 60%)
      │  ├─ data (33%)
      │  ├─ filt (33%)
      │  └─ head (33%)
      └─ assistant_extension (40%)

---

    Code
      print(dock_grid("a", panels("b", "c", "edit", active = "c")))
    Output
      <dock_grid> horizontal
      ├─ a (50%)
      └─ tabs (50%)
         ├─ b
         ├─ c (active)
         └─ edit

---

    Code
      print(dock_grid("x", "y", "z", orientation = "vertical", sizes = c(0.5, 0.3,
        0.2)))
    Output
      <dock_grid> vertical
      ├─ x (50%)
      ├─ y (30%)
      └─ z (20%)

---

    Code
      print(dock_grid("top", group("a", group("b", "c"))))
    Output
      <dock_grid> horizontal
      ├─ top (50%)
      └─ group (vertical, 50%)
         ├─ a (50%)
         └─ group (horizontal, 50%)
            ├─ b (50%)
            └─ c (50%)

---

    Code
      print(dock_grid())
    Output
      <dock_grid> horizontal (empty)

