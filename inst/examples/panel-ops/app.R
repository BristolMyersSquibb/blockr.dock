library(shiny)
library(blockr.core)
library(blockr.dock)

# A board whose extension exists to emit `views$mod` payloads, one placement
# hint per button. No UI gesture emits a server-side `move` or `resize`, and the
# add-panel modal only ever emits an `add` anchored `within` the group clicked,
# so a browser test has no way to drive the rest of the hint grammar by clicking
# a dock. This is the emitter that gives it one.
#
# The `size` hint rides `resize`, which is the verb that consumes it -- on `add`
# it is recorded for a later size-on-create pass and moves nothing, so a button
# carrying it there would assert nothing about placement.
#
# Blocks `c` and `d` are on the board but out of the view, which is what leaves
# them addressable by `add` (a member is rejected).
ops_ui <- function(id, board) {
  div(
    actionButton(NS(id, "rail_move_b"), "Move b to the right rail"),
    actionButton(NS(id, "rail_add_c"), "Add c to the right rail"),
    actionButton(NS(id, "grid_move_b"), "Move b below a"),
    actionButton(NS(id, "grid_move_c"), "Move c onto a"),
    actionButton(NS(id, "grid_add_d"), "Add d right of a"),
    actionButton(NS(id, "grid_resize_a"), "Resize a to 0.3")
  )
}

ops_srv <- function(id, board, update, ...) {
  moduleServer(
    id,
    function(input, output, session) {

      emit <- function(...) {
        update(list(views = list(mod = list(main = list(...)))))
      }

      observeEvent(
        input$rail_move_b,
        emit(move = list(blk("b", rail = "right")))
      )
      observeEvent(
        input$rail_add_c,
        emit(add = list(blk("c", rail = "right")))
      )
      observeEvent(
        input$grid_move_b,
        emit(move = list(blk("b", near = "a", side = "below")))
      )
      observeEvent(
        input$grid_move_c,
        emit(move = list(blk("c", near = "a", side = "within")))
      )
      observeEvent(
        input$grid_add_d,
        emit(add = list(blk("d", near = "a", side = "right")))
      )
      observeEvent(
        input$grid_resize_a,
        emit(resize = list(blk("a", size = 0.3)))
      )

      # What the board itself ended up holding. Neither `move` nor `resize`
      # writes the board directly -- the settled client echo does, through the
      # grid mirror -- so reading the committed board here is what separates a
      # panel that merely landed from one that persisted. Exports are gated by
      # Shiny's test mode, so a running app registers nothing.
      grid <- reactive(board_grids(board$board)[["main"]])

      exportTestValues(
        stored_rails = lapply(grid()[["rails"]], `[[`, "panels"),
        stored_grid = grid()
      )

      list(state = list())
    }
  )
}

new_panel_ops_extension <- function(...) {
  new_dock_extension(
    ops_srv,
    ops_ui,
    name = "Panel ops",
    class = "panel_ops_extension",
    ...
  )
}

serve(
  new_dock_board(
    blocks = c(
      a = new_dataset_block("iris"),
      b = new_head_block(),
      c = new_head_block(n = 3L),
      d = new_head_block(n = 5L)
    ),
    links = links(
      ab = new_link(from = "a", to = "b", input = "data"),
      ac = new_link(from = "a", to = "c", input = "data"),
      ad = new_link(from = "a", to = "d", input = "data")
    ),
    extensions = list(ops = new_panel_ops_extension()),
    views = list(main = list(ext("ops"), blk("a"), blk("b"))),
    grids = list(
      main = dock_grid(
        ext("ops"),
        panels(blk("a"), blk("b")),
        sizes = c(0.35, 0.65)
      )
    )
  ),
  "my_board"
)
