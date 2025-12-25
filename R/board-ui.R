#' @export
board_ui.dock_board <- function(id, x, plugins = board_plugins(x),
                                options = board_options(x), ...) {

  opt_ui_or_null <- function(plg, plgs, x) {
    if (plg %in% names(plgs)) board_ui(id, plgs[[plg]], x)
  }

  stopifnot(is_string(id))

  ns <- NS(id)

  tagList(
    show_hide_block_dep(),
    blockr_dock_dep(),
    off_canvas(
      id = NS(id, "blocks_offcanvas"),
      title = "Offcanvas blocks",
      block_ui(id, x, plugins[["edit_block"]])
    ),
    options_ui(
      id,
      options,
      div(
        id = "preserve_board",
        class = "mb-1",
        opt_ui_or_null("preserve_board", plugins, x)
      ),
      div(
        id = "generate_code",
        opt_ui_or_null("generate_code", plugins, x)
      )
    ),
    # Main board view (shown by default)
    tags$div(
      id = ns("board_view"),
      class = "blockr-board-view",
      style = "position: relative;",
      dockViewR::dock_view_output(
        NS(id, dock_id()),
        width = "100%",
        height = "calc(100vh - 48px)"
      ),
      # Slide-out block panel (hidden by default)
      block_panel_ui(ns)
    ),
    # Workflow overview page (hidden by default using position/visibility)
    tags$div(
      id = ns("workflows_view"),
      class = "blockr-workflows-view blockr-view-hidden",
      workflow_overview_ui(ns)
    ),
    off_canvas(
      id = NS(id, "exts_offcanvas"),
      position = "bottom",
      title = "Offcanvas extensions",
      lapply(
        dock_extensions(x),
        extension_ui,
        id = id,
        board = x
      )
    ),
    # JavaScript for view switching
    tags$script(HTML(sprintf("
      Shiny.addCustomMessageHandler('blockr-switch-view', function(view) {
        var boardView = document.getElementById('%s');
        var workflowsView = document.getElementById('%s');
        if (view === 'workflows') {
          if (boardView) boardView.classList.add('blockr-view-hidden');
          if (workflowsView) workflowsView.classList.remove('blockr-view-hidden');
          // Trigger resize for reactable
          setTimeout(function() {
            window.dispatchEvent(new Event('resize'));
          }, 50);
        } else {
          if (boardView) boardView.classList.remove('blockr-view-hidden');
          if (workflowsView) workflowsView.classList.add('blockr-view-hidden');
        }
      });
    ", ns("board_view"), ns("workflows_view"))))
  )
}

options_ui <- function(id, x, ...) {

  stopifnot(is_board_options(x))

  opts <- split(x, chr_ply(x, attr, "category"))

  offcanvas_id <- NS(id, "options_offcanvas")

  tagList(
    # Edge tab button removed - navbar gear icon triggers this offcanvas instead
    off_canvas(
      id = offcanvas_id,
      position = "end",
      title = "Board options",
      ...,
      hr(),
      do.call(
        accordion,
        c(
          list(
            id = NS(id, "board_options"),
            multiple = TRUE,
            open = FALSE,
            class = "accordion-flush"
          ),
          map(
            do.call,
            rep(list(accordion_panel), length(opts)),
            map(
              list,
              title = names(opts),
              lapply(opts, lapply, board_option_ui, id)
            )
          )
        )
      )
    )
  )
}

blockr_dock_dep <- function() {
  htmltools::htmlDependency(
    "blockr-fab",
    pkg_version(),
    src = pkg_file("assets", "css"),
    stylesheet = "blockr-dock.css"
  )
}
