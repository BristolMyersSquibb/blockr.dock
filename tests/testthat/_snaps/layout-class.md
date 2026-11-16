# dock_grid has a tree-style print method

    Code
      print(dock_grid("a", "b"))
    Output
      <dock_grid> horizontal
      ├─ 50%: a
      └─ 50%: b

---

    Code
      print(dock_grid(group("data", "filt", "head"), plg("assistant"), sizes = c(0.6,
        0.4)))
    Output
      <dock_grid> horizontal
      ├─ 60%: group (vertical)
      │  ├─ 33%: data
      │  ├─ 33%: filt
      │  └─ 33%: head
      └─ 40%: assistant (plugin)

---

    Code
      print(dock_grid("a", panels("b", "c", plg("edit"), active = "c")))
    Output
      <dock_grid> horizontal
      ├─ 50%: a
      └─ 50%: b, c (active), edit (plugin)

---

    Code
      print(dock_grid("x", "y", "z", orientation = "vertical", sizes = c(0.5, 0.3,
        0.2)))
    Output
      <dock_grid> vertical
      ├─ 50%: x
      ├─ 30%: y
      └─ 20%: z

---

    Code
      print(dock_grid("top", group("a", group("b", "c"))))
    Output
      <dock_grid> horizontal
      ├─ 50%: top
      └─ 50%: group (vertical)
         ├─ 50%: a
         └─ 50%: group (horizontal)
            ├─ 50%: b
            └─ 50%: c

---

    Code
      print(dock_grid("a", rail(plg("dag"), "b", active = "b", collapsed = TRUE),
      rail("c", position = "right")))
    Output
      <dock_grid>
      └─ a
      left rail (collapsed): dag (plugin), b (active)
      right rail: c

---

    Code
      print(dock_grid())
    Output
      <dock_grid> (empty)

# a grid prints no orientation or share where a split has one child

    Code
      print(dock_grid(group("a", "b")))
    Output
      <dock_grid>
      └─ group (vertical)
         ├─ 50%: a
         └─ 50%: b

---

    Code
      print(dock_grid("a", group(panels("b", "c"))))
    Output
      <dock_grid> horizontal
      ├─ 50%: a
      └─ 50%: group
         └─ b (active), c

