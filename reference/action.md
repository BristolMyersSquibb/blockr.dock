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

board_block_select(
  id,
  board,
  blk_ids = board_block_ids(board),
  selected = NULL,
  max_items = 1L,
  label = NULL,
  options = list()
)

sidebar_owned_by(action, board_id, session = get_session())
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

- board:

  Board object

- blk_ids:

  Character vector of block IDs to offer for selection

- selected:

  Character vector of pre-selected block IDs

- max_items:

  Maximum number of blocks that can be selected at once, or `NULL` for
  no limit

- label:

  Input label

- options:

  Passed to
  [`shiny::selectizeInput()`](https://rdrr.io/pkg/shiny/man/selectInput.html)
  as `options`, merged over the defaults that make up the picker

- action:

  Action ID

- board_id:

  ID of the board module the action is registered with

- session:

  Shiny session

## Value

The constructor `new_action` returns a classed function that inherits
from `action`. Inheritance can be checked with functions `is_action()`,
`is_action_generator()` checks whether an objects is a function that
returns an `action` object. String-value action IDs can be retrieved
with `action_id()` and the set of actions associated with a board can be
enumerated via `board_actions()`. Finally, `action_triggers()` returns a
named list of objects suitable for use as action triggers. For an action
that currently holds a sidebar panel, `sidebar_owned_by()` returns a
list with components `panel`, `open` and `pinned`, and `NULL` otherwise.

For utilities `block_input_select()` and `board_block_select()`, see the
respective sections.

## Details

An action is a function that can be called with arguments `input`,
`output` and `session`, behaving as one would expect from a shiny server
module function. Actions are typically created by action generator
functions, they each have a unique ID and a
[`shiny::reactiveVal()`](https://rdrr.io/pkg/shiny/man/reactiveVal.html)-based
trigger object (inheriting from `action_trigger`). Action trigger
objects implement their own counter-based invalidation mechanism (on top
of how reactive values behave).

An action that fills a sidebar panel records itself on it as it writes,
so the panel reports the writing action as its owner alongside whether
it is open and pinned. `sidebar_owned_by()` reads that back for a given
action: a consumer re-firing an action on a new selection can ask
whether the form it previously opened is still the one on screen,
without knowing which panel that action fills. The read is a snapshot
and creates no reactive dependency, as called for in an
[`shiny::observeEvent()`](https://rdrr.io/pkg/shiny/man/observeEvent.html)
handler.

## `block_input_select()`

Determine input options for a block by removing inputs that are already
used and also takes into account some edge-cases, such as variadic
blocks. If `mode` is set as "inputs", this will return a character
vector, for "create", the return value of a
[`shiny::selectizeInput()`](https://rdrr.io/pkg/shiny/man/selectInput.html)
call and for "update", the return value of a
[`shiny::updateSelectizeInput()`](https://rdrr.io/pkg/shiny/man/updateSelectInput.html)
call.

## `board_block_select()`

Block selection UI over the blocks of a board is available as
`board_block_select()`, which returns an object inheriting from
`shiny.tag.list`: the result of a
[`shiny::selectizeInput()`](https://rdrr.io/pkg/shiny/man/selectInput.html)
call together with the styling its option rendering requires. This is
the picker the board itself uses wherever a block is chosen, listing
each block by icon, name, ID and defining package, and searchable over
all of those.
