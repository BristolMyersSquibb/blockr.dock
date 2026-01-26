#' @export
board_ui.dock_board <- function(id, x, plugins = board_plugins(x),
                                options = board_options(x), ...) {

  opt_ui_or_null <- function(plg, plgs, x) {
    if (plg %in% names(plgs)) board_ui(id, plgs[[plg]], x)
  }

  stopifnot(is_string(id))

  ns <- NS(id)

  tagList(
    shinyjs::useShinyjs(),
    show_hide_block_dep(),
    blockr_dock_dep(),
    off_canvas(
      id = NS(id, "blocks_offcanvas"),
      title = "Offcanvas blocks",
      block_ui(id, x, plugins[["edit_block"]])
    ),
    # Sidebars
    sidebars_ui(ns, x, generate_code_ui = opt_ui_or_null("generate_code", plugins, x)),
    div(
      class = "blockr-navbar",
      div(
        class = "blockr-navbar-left",
        opt_ui_or_null("preserve_board", plugins, x)
      ),
      div(
        class = "blockr-navbar-right",
        tags$button(
          class = "blockr-navbar-icon-btn",
          onclick = sprintf(
            "Shiny.setInputValue('%s', Date.now(), {priority: 'event'})",
            ns("open_settings_sidebar")
          ),
          bsicons::bs_icon("gear")
        )
      )
    ),
    dockViewR::dock_view_output(
      NS(id, dock_id()),
      width = "100%",
      height = "calc(100vh - 48px)"
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
