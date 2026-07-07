# Dock extensions

Functionality of a `dock_board` can be extended by supplying one or more
`dock_extension` objects, which essentially provide UI shown in a dock
panel that allows for manipulating the board state. A set of dock
extensions can be combined into a `dock_extensions` object.

## Usage

``` r
new_dock_extension(
  server,
  ui,
  name,
  class,
  description = NULL,
  ctor = sys.parent(),
  pkg = NULL,
  options = new_board_options(),
  external_ctrl = FALSE,
  ...
)

is_dock_extension(x)

validate_extension(x, ...)

extension_ui(x, id, ...)

extension_server(x, ...)

extension_id(x)

extension_name(x)

extension_description(x)

extension_ctor(x)

new_dock_extensions(x = list())

is_dock_extensions(x)

validate_extensions(x)

as_dock_extensions(x, ...)

# S3 method for class 'dock_extensions'
as_dock_extensions(x, ...)

# S3 method for class 'dock_extension'
as_dock_extensions(x, ...)

# S3 method for class 'list'
as_dock_extensions(x, ...)

extension_block_callback(x, ...)
```

## Arguments

- server:

  A function returning
  [`shiny::moduleServer()`](https://rdrr.io/pkg/shiny/man/moduleServer.html).
  Beyond `id`, it is called with the board handles `board`, `update`,
  `view_data` and `actions`, plus `extensions` – an environment exposing
  every extension's server result keyed by ID (each carrying its
  `state`), so one extension can read another's state via
  `extensions[[id]]`. `view_data` is a reactive holding the live
  all-views layout (the committed board is `board`); it is `NULL` until
  every view has reported its layout once, so `req()` it.

- ui:

  A function with a single argument (`ns`) returning a `shiny.tag`

- name:

  Name for extension

- class:

  Extension subclass

- description:

  Optional free-text description of the extension, surfaced as
  consumer-neutral metadata (e.g. to the AI assistant)

- ctor:

  Constructor function name

- pkg:

  Package to look up `ctor`

- options:

  Board options supplied by an extension

- external_ctrl:

  Set up external control (experimental). `FALSE` (the default) opts
  out; `TRUE` exposes every constructor input as externally
  controllable; a character vector names a subset of them.

- ...:

  Further attributes

- x:

  Extension object

- id:

  Namespace ID

## Value

The constructors `new_dock_extension()` and `new_dock_extension()`, as
do the coercion function `as_dock_extension()` and
`as_dock_extension()`, return objects that inherit from `dock_extension`
and `dock_extensions` respectively. This inheritance structure can be
checked using `is_dock_extension()` and `is_dock_extensions()`, which
both return a boolean. A `dock_extension` can be validated using
`validate_extension()` and a `dock_extensions` object using
`validate_extensions()`, which return the input object invisibly and
throw errors as side-effects. Several getter functions return extension
attributes, including `extension_ui()` (a function),
`extension_server()` (a function), `extension_id()` (a string),
`extension_name()` (a string), `extension_description()` (a string or
`NULL`) and `extension_ctor()` (an object that inherits from
`blockr_ctor`).

## Examples

``` r
ext <- new_edit_board_extension()
is_dock_extension(ext)
#> [1] TRUE
```
