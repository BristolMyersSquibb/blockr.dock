# Board actions

Logic including a modal-based UI for board actions such as "append
block" or "edit stack" can be specified using `action` objects, which
essentially are classed shiny server functions.

## Usage

``` r
new_action(func, id)

is_action(x)

is_action_generator(x)

action_id(x)

board_actions(x, ...)

action_triggers(x)

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

  A function which will be used to create a
  [`shiny::moduleServer()`](https://rdrr.io/pkg/shiny/man/moduleServer.html).

- id:

  Input ID

- x:

  Object

- ...:

  Forwarded to other methods

- block:

  Block object

- block_id:

  Block ID

- links:

  Links object

- mode:

  Switch for determining the return object

- blocks:

  Character vector of block registry IDs

- selected:

  Character vector of pre-selected block (registry) IDs

## Value

The constructor `new_action` returns a classed function that inherits
from `action`. Inheritance can be checked with functions `is_action()`,
`is_action_generator()` checks whether an objects is a function that
returns an `action` object. String-value action IDs can be retrieved
with `action_id()` and the set of actions associated with a board can be
enumerated via `board_actions()`. Finally, `action_triggers()` returns a
named list of objects suitable for use as action triggers.

For utilities `block_input_select()`, `block_registry_selectize()` and
`board_select`, see the respective sections.

## Details

An action is a function that can be called with arguments `input`,
`output` and `session`, behaving as one would expect from a shiny server
module function. Actions are typically created by action generator
functions, they each have a unique ID and a
[`shiny::reactiveVal()`](https://rdrr.io/pkg/shiny/man/reactiveVal.html)-based
trigger object (inheriting from `action_trigger`). Action trigger
objects implement their own counter-based invalidation mechanism (on top
of how reactive values behave).

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
