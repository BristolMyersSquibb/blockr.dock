# blockr.dock

A docking layout manager provided by
[dockViewR](https://github.com/DivadNojnarg/dockViewR) can be used as
front-end to a [blockr](https://blockr.site/) board using this package.

## Installation

You can install the development version of blockr.dock from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("BristolMyersSquibb/blockr.dock")
```

## Simple dock

To start up a board for visualizing `Sepal.Length` against `Sepal.Width`
for the `iris` dataset:

``` r
library(blockr.dock)
library(blockr.core)

serve(
  new_dock_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_scatter_block(x = "Sepal.Length", y = "Sepal.Width")
    ),
    links = list(from = "a", to = "b", input = "data"),
    extensions = list(edit = new_edit_board_extension()),
    layout = list("edit", list("a", "b"))
  )
)
```

![Simple dock](reference/figures/dock.png)

Simple dock

## Locked dock

A locked dock prevents users from adding or removing blocks and
extensions. Drag-and-drop and panel resizing are also disabled.

``` r
library(blockr.dock)
library(blockr.core)

options(blockr.dock_is_locked = TRUE)

serve(
  new_dock_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_head_block(n = 20L),
      c = new_scatter_block(x = "Sepal.Length", y = "Sepal.Width"),
      d = new_subset_block()
    ),
    links = c(
      new_link("a", "b", input = "data"),
      new_link("b", "c", input = "data"),
      new_link("b", "d", input = "data")
    ),
    layout = list(list("a", "b"), list("c", "d"))
  )
)
```

![Locked dock](reference/figures/locked-dock.png)

Locked dock

## Multi-view dock

Define multiple views (global tabs), each with its own DockView layout.
Blocks and extensions are shared across views via the board’s DAG; view
membership is a layout concern only.

``` r
library(blockr.core)
library(blockr.dock)

board <- new_dock_board(
  extensions = blockr.dag::new_dag_extension(),
  blocks = c(
    dataset_1 = new_dataset_block(),
    head_1 = new_head_block()
  ),
  links = new_link("dataset_1", "head_1"),
  layout = list(
    Analysis = list("dataset_1", "head_1", "dag_extension"),
    Overview = dock_view("dag_extension", active = TRUE),
    Empty = list()
  )
)

serve(board, "my_board")
```

![Multi-view dock](reference/figures/views.png)

Multi-view dock
