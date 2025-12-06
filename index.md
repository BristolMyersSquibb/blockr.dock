# blockr.dock

A docking layout manager provided by dockViewR can be used as front-end
to a blockr board using this package.

## Installation

You can install the development version of blockr.dock from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("BristolMyersSquibb/blockr.dock")
```

## Example

To start up a board for visualizing `Sepal.Length` against `Sepal.Width`
for the `iris` dataset, we can run

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

This is a read-only view of this board, as no blocks can be added or
removed and block connectivity cannot be changed. We can however change
parameters for the input data and visualization.
