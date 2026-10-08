# Navbar items

The navbar of a dock board is drawn from a `navbar_items` collection,
the dock's own controls included. An item is created by `navbar_item()`,
items are combined with [`c()`](https://rdrr.io/r/base/c.html), `[`
reorders or drops them by id and `[[` extracts one. An app hands the
navbar to
[`blockr.core::serve()`](https://bristolmyerssquibb.github.io/blockr.core/reference/serve.html)
as `navbar`, a function of the board and its plugins that returns the
items, as it does `plugins` and `options`.

## Usage

``` r
navbar_item(id, ui, server = NULL, fill = FALSE, shrink = FALSE)

is_navbar_item(x)

is_navbar_items(x)

plugin_navbar_item(plugin)

default_navbar_items(x, plugins)

blockr_app_navbar(x, plugins)

custom_navbar(x)
```

## Arguments

- id:

  Item id, unique within the navbar

- ui:

  A function called as `ui(id, board)` with `id` as
  `NS(board_id, item_id)` and the board, returning the item's UI, or
  `NULL` to leave the item out of the bar

- server:

  `NULL` or a function called as `server(id, board)` with the item id
  and the board's reactive values, once per session, from the board
  server, so that a
  [`shiny::moduleServer()`](https://rdrr.io/pkg/shiny/man/moduleServer.html)
  it starts shares the UI's namespace

- fill:

  Whether the item takes up the free space of the bar

- shrink:

  Whether the item gives way first on a narrow bar. The items with
  neither `fill` nor `shrink` keep their width.

- x:

  Object. For `default_navbar_items()` and `blockr_app_navbar()`, the
  board, and for `custom_navbar()`, the items to append: a
  `navbar_item`, a `navbar_items` collection or a list of items.

- plugin:

  A board plugin, or `NULL`

- plugins:

  Board plugins

## Value

The constructor `navbar_item()` returns a `navbar_item` object, and so
does `plugin_navbar_item()`, unless its plugin is `NULL`, which returns
`NULL`. The functions `default_navbar_items()` and `blockr_app_navbar()`
return a `navbar_items` collection, as do
[`c()`](https://rdrr.io/r/base/c.html) and `[` on items, and
`custom_navbar()` returns a function of the board and its plugins that
returns one. The checks `is_navbar_item()` and `is_navbar_items()`
return a boolean.

## Details

The default items, returned by `default_navbar_items()`, are, in order:
the piece the `preserve_board` plugin draws (id `"preserve_board"`), a
spacer (`"spacer"`), the busy indicator (`"busy"`), the view menu
(`"views"`), the read-only indicator (`"read_only"`), drawn only on a
locked board, and the board options (`"options"`). The generic
`blockr_app_navbar()` gives the default by board class, and
`custom_navbar()` returns a navbar function that appends a fixed set of
items to it, dropping any default item that shares an id with one of
them. A plugin's piece is placed by `plugin_navbar_item()`, which the
defaults use for the `preserve_board` plugin.

An item whose `ui` draws a single
[`shiny::uiOutput()`](https://rdrr.io/pkg/shiny/man/htmlOutput.html) or
[`shiny::textOutput()`](https://rdrr.io/pkg/shiny/man/textOutput.html)
takes no room in the bar while that output is empty, as an avatar would
with nobody signed in.

## Examples

``` r
help <- navbar_item(
  "help",
  function(id, board) {
    shiny::tags$a(
      href = "https://bristolmyerssquibb.github.io/blockr.dock/",
      "Help"
    )
  }
)

brd <- new_dock_board()
items <- c(default_navbar_items(brd, blockr.core::board_plugins(brd)), help)
names(items)
#> [1] "preserve_board" "spacer"         "busy"           "views"         
#> [5] "read_only"      "options"        "help"          
names(items[c("help", "options")])
#> [1] "help"    "options"

if (interactive()) {
  blockr.core::serve(brd, navbar = custom_navbar(help))
}
```
