# Edit board extension

A simplistic example of an extension which can be used for manipulating
the board via a table-based UI. Mainly relevant for testing purposes.

## Usage

``` r
new_edit_board_extension(...)
```

## Arguments

- ...:

  Forwarded to [`new_dock_extension()`](extension.md)

## Value

A board extension object that additionally inherits from
`edit_board_extension`.

## Examples

``` r
ext <- new_edit_board_extension()
is_dock_extension(ext)
#> [1] TRUE
```
