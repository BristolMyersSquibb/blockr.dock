# Board actions

Logic including a modal-based UI for board actions such as "append
block" or "edit stack" can be specified using `action` objects, which
essentially are classed functions that can either be called to return a
shiny module `as_module = TRUE` or a function `as_module = FALSE` which
injects code (passed as `expr`) into a shiny server context.

## Usage

``` r
new_action(func)

is_action(x)

is_action_module(x)

is_action_function(x)

add_block_action(trigger, as_module = TRUE)

append_block_action(trigger, as_module = TRUE)

remove_block_action(trigger, as_module = TRUE)

add_link_action(trigger, as_module = TRUE)

remove_link_action(trigger, as_module = TRUE)

add_stack_action(trigger, as_module = TRUE)

edit_stack_action(trigger, as_module = TRUE)

remove_stack_action(trigger, as_module = TRUE)

block_input_select(
  block = NULL,
  block_id = NULL,
  links = NULL,
  mode = c("create", "update", "inputs"),
  ...
)

block_registry_selectize(id, blocks = list_blocks())

board_select(id, blocks, selected = NULL, ...)
```

## Arguments

- func:

  A function which will be evaluated (with modified formals) in a shiny
  server context

- x:

  Object

- trigger:

  A string, function or
  [`shiny::reactive()`](https://rdrr.io/pkg/shiny/man/reactive.html)

- as_module:

  Logical flag controlling the return type

- block:

  Block object

- block_id:

  Block ID

- links:

  Links object

- mode:

  Switch for determining the return object

- ...:

  Forwarded to other methods

- id:

  Input ID

- blocks:

  Character vector of block registry IDs

- selected:

  Character vector of pre-selected block (registry) IDs

## Value

The constructor `new_action` returns a classed function that inherits
from `action`. Inheritance can be checked with functions `is_action()`,
`is_action_module()` and `is_action_function()`, which all return scalar
logicals.

For utilities `block_input_select()`, `block_registry_selectize()` and
`board_select`, see the respective sections.

## Details

An action is a function that can be called with arguments `trigger` and
`as_module` to return another function. The action trigger may either be
a string (referring to an `input`), a function (that will be called with
a single argument `input`) or a
[`shiny::reactive()`](https://rdrr.io/pkg/shiny/man/reactive.html)
object. The flag `as_module` controls the behavior of the returned
function: if `TRUE`, it is a function (inheriting from `action_module`)
with arguments `board`, `update`, `...` and `domain`, which, when
called, again returns a function with arguments `input`, `output` and
`session`, suitable as argument to
[`shiny::moduleServer()`](https://rdrr.io/pkg/shiny/man/moduleServer.html).
If `FALSE` is passed instead, a function (inheriting from
`action_function`) with arguments `board`, `update`, `...` and `domain`
is returned.

The expression `expr`, passed when instantiating an `action` object will
be evaluated in a context, where the following bindings exist: `board`,
`update`, `domain`, `input`, `output` and `session`. In the case of
`as_module = FALSE`, `domain` is an alias for `session`.

## `block_input_select()`

Determine input options for a block by removing inputs that are already
used and also takes into account some edge-cases, such as variadic
blocks. If `mode` is set as "inputs", this will return a character
vector, for "create", the return value of a
[`shiny::selectizeInput()`](https://rdrr.io/pkg/shiny/man/selectInput.html)
call and for "update", the return value of a
[`shiny::updateSelectizeInput()`](https://rdrr.io/pkg/shiny/man/updateSelectInput.html)
call.

## `block_registry_selectize()`

This creates UI for a block registry selector via
[`shiny::selectizeInput()`](https://rdrr.io/pkg/shiny/man/selectInput.html)
and returns an object that inherits from `shiny.tag`.

## `board_select()`

Block selection UI, enumerating all blocks in a board is available as
`board_select()`. An object that inherits from `shiny.tag` is returned,
which contains the result from a
[`shiny::selectizeInput()`](https://rdrr.io/pkg/shiny/man/selectInput.html)
call.
