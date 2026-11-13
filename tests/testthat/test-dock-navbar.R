dock_parts <- c(
  "logo", "name", "storage", "undo_redo", "spacer", "views", "read_only",
  "options"
)

# An item that no constructor would make, as a script could build by hand
hand_item <- function(...) {
  structure(list(...), class = "navbar_item")
}

test_that("each function makes a navbar item of its kind", {

  link <- navbar_link("issues", "Report an issue", "https://example.org")

  expect_true(is_navbar_item(link))
  expect_false(is_navbar_items(link))
  expect_identical(
    unclass(link),
    list(
      kind = "link",
      id = "issues",
      label = "Report an issue",
      href = "https://example.org"
    )
  )

  # A menu holds its links and menus as a collection, which drops a NULL as
  # c() does
  menu <- navbar_menu("help", "Help", link, NULL, navbar_menu("more", "More"))

  expect_identical(menu[["kind"]], "menu")
  expect_identical(menu[["label"]], "Help")
  expect_true(is_navbar_items(menu[["items"]]))
  expect_identical(names(menu[["items"]]), c("issues", "more"))
  expect_length(menu[["items"]][["more"]][["items"]], 0L)

  expect_identical(
    unclass(navbar_storage("avatar")),
    list(kind = "storage", id = "avatar")
  )
  expect_identical(
    unclass(navbar_item("deck", "blockr.topline-deck_button")),
    list(kind = "module", id = "deck", module = "blockr.topline-deck_button")
  )
})

test_that("the item constructors refuse what the dock cannot draw", {

  expect_error(navbar_link("", "A", "u"), class = "navbar_item_id_invalid")
  expect_error(
    navbar_link(c("a", "b"), "A", "u"),
    class = "navbar_item_id_invalid"
  )
  expect_error(
    navbar_storage(NA_character_),
    class = "navbar_item_id_invalid"
  )

  expect_error(
    navbar_link("issues", "", "u"),
    "Expecting the label of the navbar item \"issues\" to be a non-empty",
    fixed = TRUE,
    class = "navbar_item_label_invalid"
  )
  expect_error(navbar_menu("help", NULL), class = "navbar_item_label_invalid")
  expect_error(
    navbar_link("issues", "Issues", NA_character_),
    "Expecting the `href` of the navbar link \"issues\"",
    fixed = TRUE,
    class = "navbar_item_href_invalid"
  )
  expect_error(
    navbar_item("deck", ""),
    "Expecting the module of the navbar item \"deck\"",
    fixed = TRUE,
    class = "navbar_item_module_invalid"
  )
  expect_error(navbar_item("deck", 1), class = "navbar_item_module_invalid")

  # A menu holds links and menus alone
  expect_error(
    navbar_menu("help", "Help", navbar_storage("avatar")),
    paste0(
      "The navbar menu \"help\" holds \"avatar\", which is neither a link ",
      "nor a menu."
    ),
    fixed = TRUE,
    class = "navbar_item_menu_invalid"
  )
  expect_error(
    navbar_menu("help", "Help", default_navbar()),
    "which are neither links nor menus.",
    fixed = TRUE,
    class = "navbar_item_menu_invalid"
  )
  expect_error(
    navbar_menu("help", "Help", "issues"),
    class = "navbar_items_coercion_invalid"
  )

  # Ids are unique among the items of a menu, but may repeat those of the
  # items around it
  expect_error(
    navbar_menu(
      "help",
      "Help",
      navbar_link("issues", "Issues", "u"),
      navbar_link("issues", "Bugs", "u")
    ),
    "More than one navbar item has the id \"issues\".",
    fixed = TRUE,
    class = "navbar_items_ids_invalid"
  )

  items <- c(
    navbar_link("issues", "Issues", "u"),
    navbar_menu("help", "Help", navbar_link("issues", "Issues", "u"))
  )

  expect_identical(names(items), c("issues", "help"))
})

test_that("navbar items combine with c() and subset with [ by id", {

  a <- navbar_storage("a")
  b <- navbar_storage("b")
  d <- navbar_storage("d")

  items <- c(a, b)

  expect_true(is_navbar_items(items))
  expect_false(is_navbar_item(items))
  expect_identical(names(items), c("a", "b"))

  # Items, collections, lists of items and NULL combine in any order
  expect_identical(names(c(items, d)), c("a", "b", "d"))
  expect_identical(names(c(d, items)), c("d", "a", "b"))
  expect_identical(names(c(items, NULL, d)), c("a", "b", "d"))
  expect_identical(names(c(items, list(d))), c("a", "b", "d"))

  # Reorder and drop by id or position, and extract one
  expect_identical(names(items[c("b", "a")]), c("b", "a"))
  expect_identical(names(c(items, d)[-2]), c("a", "d"))
  expect_identical(names(items[character()]), character())
  expect_identical(items[], items)
  expect_identical(items[["b"]], b)
  expect_identical(items[[1L]], a)

  expect_error(
    items[c("a", "z")],
    "Unknown navbar item \"z\".",
    fixed = TRUE,
    class = "navbar_items_subset_invalid"
  )
  expect_error(items[["z"]], class = "navbar_items_subset_invalid")
  expect_error(items[3], class = "navbar_items_contents_invalid")
  expect_error(
    c(items, a),
    "More than one navbar item has the id \"a\".",
    fixed = TRUE,
    class = "navbar_items_ids_invalid"
  )
  expect_error(items[c("a", "a")], class = "navbar_items_ids_invalid")
  expect_error(c(items, "a"), class = "navbar_items_coercion_invalid")
})

test_that("default_navbar() gives the dock's own parts in order", {

  items <- default_navbar()

  expect_true(is_navbar_items(items))
  expect_identical(names(items), dock_parts)
  expect_identical(unique(chr_xtr(items, "kind")), "part")
})

test_that("custom_navbar() drops what drop names and appends items", {

  help <- navbar_menu(
    "help",
    "Help",
    navbar_link("issues", "Report an issue", "https://example.org")
  )

  expect_identical(custom_navbar(), default_navbar())
  expect_identical(names(custom_navbar(help)), c(dock_parts, "help"))

  expect_identical(
    names(custom_navbar(c(help, navbar_storage("avatar")), drop = "name")),
    c(setdiff(dock_parts, "name"), "help", "avatar")
  )
  expect_identical(
    names(custom_navbar(drop = c("undo_redo", "name"))),
    setdiff(dock_parts, c("name", "undo_redo"))
  )

  # An item with a default's id takes its place at the end
  expect_identical(
    names(custom_navbar(navbar_link("views", "Views", "https://example.org"))),
    c(setdiff(dock_parts, "views"), "views")
  )

  expect_error(
    custom_navbar(drop = "nmae"),
    "In `drop`, \"nmae\" names no default navbar item.",
    fixed = TRUE,
    class = "navbar_items_subset_invalid"
  )
  expect_error(
    custom_navbar(drop = c("nmae", "lgo")),
    "In `drop`, \"nmae\" and \"lgo\" name no default navbar items.",
    fixed = TRUE,
    class = "navbar_items_subset_invalid"
  )
  expect_error(custom_navbar(drop = 2), class = "navbar_items_subset_invalid")
  expect_error(custom_navbar("help"), class = "navbar_items_coercion_invalid")
})

test_that("the dock's frontend_args() returns the items of the bar", {

  dock <- new_dock_plugin()

  # The default bar where a script passes none
  expect_identical(
    frontend_args(dock),
    list(navbar = lapply(dock_parts, function(id) list(kind = "part", id = id)))
  )

  help <- navbar_menu(
    "help",
    "Help",
    navbar_link("issues", "Report an issue", "https://example.org"),
    navbar_menu("more", "More")
  )

  args <- frontend_args(dock, navbar = custom_navbar(help, drop = "name"))

  expect_named(args, "navbar")
  expect_length(args[["navbar"]], length(dock_parts))
  expect_identical(
    args[["navbar"]][[length(dock_parts)]],
    list(
      kind = "menu",
      id = "help",
      label = "Help",
      items = list(
        list(
          kind = "link",
          id = "issues",
          label = "Report an issue",
          href = "https://example.org"
        ),
        list(kind = "menu", id = "more", label = "More", items = list())
      )
    )
  )

  # The bar takes an item, or a list of items, as a collection
  expect_identical(
    frontend_args(dock, navbar = navbar_storage("avatar")),
    list(navbar = list(list(kind = "storage", id = "avatar")))
  )
  expect_identical(
    frontend_args(dock, navbar = list()),
    list(navbar = list())
  )

  # R checks a module's id alone, and leaves it to the client to find the
  # module
  expect_identical(
    frontend_args(dock, navbar = navbar_item("deck", "blockr.topline-deck"))[[
      "navbar"
    ]],
    list(list(kind = "module", id = "deck", module = "blockr.topline-deck"))
  )
})

test_that("the dock's frontend_args() refuses what it cannot draw, naming it", {

  dock <- new_dock_plugin()

  expect_error(
    frontend_args(dock, navbar = list(hand_item(kind = "part", id = "clock"))),
    "The dock has no part \"clock\" to draw on the navbar.",
    fixed = TRUE,
    class = "navbar_item_part_invalid"
  )
  expect_error(
    frontend_args(dock, navbar = list(hand_item(kind = "widget", id = "deck"))),
    "The navbar item \"deck\" has an unknown kind.",
    fixed = TRUE,
    class = "navbar_item_kind_invalid"
  )
  expect_error(
    frontend_args(
      dock,
      navbar = list(hand_item(kind = "link", id = "issues", label = "Issues"))
    ),
    paste0(
      "The navbar item \"issues\" is a link, so it holds exactly the fields ",
      "`kind`, `id`, `label`, and `href`."
    ),
    fixed = TRUE,
    class = "navbar_item_structure_invalid"
  )
  expect_error(
    frontend_args(
      dock,
      navbar = list(
        hand_item(kind = "menu", id = "help", label = "Help", items = list())
      )
    ),
    "Expecting the items of the navbar menu \"help\"",
    fixed = TRUE,
    class = "navbar_item_menu_invalid"
  )
  expect_error(
    frontend_args(
      dock,
      navbar = list(navbar_storage("avatar"), navbar_storage("avatar"))
    ),
    "More than one navbar item has the id \"avatar\".",
    fixed = TRUE,
    class = "navbar_items_ids_invalid"
  )

  # A script for blockr.dock 0.2.0 passed a function
  expect_error(
    frontend_args(dock, navbar = function(x, plugins) default_navbar()),
    class = "navbar_items_coercion_invalid"
  )
})

test_that("serve() hands navbar to the dock's frontend_args()", {

  board <- new_board()

  expect_s3_class(serve(board), "shiny.appobj")
  expect_s3_class(
    serve(board, navbar = custom_navbar(drop = "name")),
    "shiny.appobj"
  )

  # The dock's method refuses as serve() is called
  expect_error(
    serve(board, navbar = list(hand_item(kind = "part", id = "clock"))),
    "The dock has no part \"clock\" to draw on the navbar.",
    fixed = TRUE,
    class = "navbar_item_part_invalid"
  )

  # It refuses every other argument, without evaluating it
  expect_error(
    serve(board, title = "Trials", theme = no_such_function()),
    paste0(
      "The app's frontend, blockr.dock-dock_plugin, takes no argument of ",
      "`serve()` but `navbar`, and was given `title` and `theme`."
    ),
    fixed = TRUE,
    class = "frontend_args_invalid"
  )
  expect_error(
    serve(board, "my_board"),
    "and was given an unnamed argument.",
    fixed = TRUE,
    class = "frontend_args_invalid"
  )
})
