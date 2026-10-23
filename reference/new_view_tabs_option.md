# Views as tabs

A board option that shows the board's views as a line of tabs under the
navbar, one tab per view, with the view menu reduced to its chevron at
the end of the line. Off by default, it sits with the light/dark switch
and "Compact" under "Theme options", and "Show views as tabs" at the
foot of the view menu sets it as well. A board saved with the option on
shows the tabs from its first paint.

## Usage

``` r
new_view_tabs_option(
  value = blockr_option("view_tabs", FALSE),
  category = "Theme options",
  label = "Views as tabs",
  ...
)
```

## Arguments

- value:

  Logical, whether the views show as tabs. Defaults to the `view_tabs`
  blockr option, else `FALSE`.

- category:

  Options sidebar category.

- label:

  The option's name, which its switch shows.

- ...:

  Passed to
  [`blockr.core::new_board_option()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_board_options.html).

## Value

A `board_option` object.

## Examples

``` r
new_view_tabs_option(TRUE)
#> view_tabs: TRUE
```
