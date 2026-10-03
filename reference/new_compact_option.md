# Compact block headers

A board option that switches every block header to its compact form: an
eyebrow line over the block's content, with the block's mark as a small
tinted square and the name in small capitals. Off by default, which
keeps the regular header (a 32px mark and a 16px title that may wrap to
two lines). It sits with the light/dark switch under "Theme options".

## Usage

``` r
new_compact_option(
  value = blockr_option("compact", FALSE),
  category = "Theme options",
  ...
)
```

## Arguments

- value:

  Logical, whether headers start compact. Defaults to the `compact`
  blockr option, else `FALSE`.

- category:

  Options sidebar category.

- ...:

  Passed to
  [`blockr.core::new_board_option()`](https://bristolmyerssquibb.github.io/blockr.core/reference/new_board_options.html).

## Value

A `board_option` object.

## Examples

``` r
new_compact_option(TRUE)
#> compact: TRUE
```
