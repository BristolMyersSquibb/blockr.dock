library(shiny)
library(blockr.core)
library(blockr.dock)

# A static htmlwidget, built in a block's UI rather than through a render
# function, of a kind that only knows the sizes htmlwidgets hands it: it writes
# the last one to `data-size`. A real widget of this kind (a leaflet map, say)
# draws at that size.
probe_binding <- "
if (!window.probeWidgetRegistered) {
  window.probeWidgetRegistered = true;
  HTMLWidgets.widget({
    name: 'probe',
    type: 'output',
    factory: function (el, width, height) {
      var record = function (w, h) {
        el.setAttribute('data-size', Math.round(w) + 'x' + Math.round(h));
      };
      record(width, height);
      return { renderValue: function () {}, resize: record };
    }
  });
}
"

probe_widget <- function(id) {
  tagList(
    htmltools::htmlDependency(
      "htmlwidgets",
      as.character(utils::packageVersion("htmlwidgets")),
      src = system.file("www", package = "htmlwidgets"),
      script = "htmlwidgets.js"
    ),
    tags$script(HTML(probe_binding)),
    div(
      id = id,
      class = "probe html-widget",
      style = "width: 100%; height: 150px;"
    ),
    tags$script(
      type = "application/json",
      `data-for` = id,
      HTML("{\"x\":[],\"evals\":[],\"jsHooks\":[]}")
    )
  )
}

new_probe_block <- function(...) {
  new_data_block(
    function(id) {
      moduleServer(
        id,
        function(input, output, session) {
          list(expr = reactive(quote(datasets::iris)), state = list())
        }
      )
    },
    function(id) probe_widget(NS(id, "probe")),
    class = "probe_block",
    ...
  )
}

# Block `o` is built with its inputs open, off screen, and then moved into
# its panel; block `s` paints with its inputs closed.
shut <- new_probe_block()
attr(shut, "visible") <- "outputs"

serve(
  new_dock_board(
    blocks = c(o = new_probe_block(), s = shut),
    views = list(main = dock_view(c("o", "s"), name = "Main")),
    grids = list(
      main = dock_grid("block_panel-o", "block_panel-s", sizes = c(0.5, 0.5))
    ),
    active = "main"
  ),
  "my_board"
)
