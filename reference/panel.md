# UI utilities

Exported utilities for manipulating dock panels (i.e. displaying
panels).

## Usage

``` r
show_panel(id, board, dock, type = c("block", "extension"))
```

## Arguments

- id:

  Object ID

- board:

  Board object

- dock:

  Object available as `dock` in extensions

- type:

  Either "block" or "extensions", depending on what kind of panel should
  be shown

## Value

`NULL`, invisibly
