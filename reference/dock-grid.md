# Canonical view grid

A `dock_grid` is a view's geometry in our compact, dockView-independent
form – nested splits and tab groups with sizes normalised to 0-1 ratios
and no volatile ids, so two casts of the same layout compare
[`identical()`](https://rdrr.io/r/base/identical.html). It is authored
with [dock_grid()](https://rdrr.io/r/graphics/layout.html) and produced
by `as_dock_grid()`, which casts another `dock_grid` (identity) or a
[dock_layout](https://bristolmyerssquibb.github.io/blockr.dock/reference/dock-layout.md)
(dockView's client echo) into it and is idempotent. `is_dock_grid()` is
the class check; `validate_dock_grid()` returns its input and errors on
a malformed or non-canonical grid.

## Usage

``` r
as_dock_grid(x, ...)

is_dock_grid(x)

validate_dock_grid(x)
```

## Arguments

- x:

  Object to cast (a `dock_grid` or a
  [dock_layout](https://bristolmyerssquibb.github.io/blockr.dock/reference/dock-layout.md)),
  validate, or test.

- ...:

  Passed on to methods.

## Value

`as_dock_grid()` and `validate_dock_grid()` a `dock_grid`;
`is_dock_grid()` a boolean.
