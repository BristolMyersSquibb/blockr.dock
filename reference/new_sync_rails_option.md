# Rails in step across views

A board option that opens and closes the rails of every view together.
Collapsing or expanding the rail on one edge of a view does the same to
the rail on that edge of every other view, and a view visited for the
first time opens with its rails the way the views already visited show
theirs. Only a rail holding panels takes part: an empty one is not
shown. On by default; switched off, each view keeps its rails as they
were left there.

## Usage

``` r
new_sync_rails_option(
  value = blockr_option("sync_rails", TRUE),
  category = "Board options",
  ...
)
```

## Arguments

- value:

  Logical, whether the rails are kept in step. Defaults to the
  `sync_rails` blockr option, else `TRUE`.

- category:

  Options sidebar category.

- ...:

  Passed to
  [`blockr.core::new_board_option()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_board_options.html).

## Value

A `board_option` object.

## Examples

``` r
new_sync_rails_option(FALSE)
#> sync_rails: FALSE
```
