#' @export
board_ui.dock_board <- function(id, x, plugins = board_plugins(x),
                                options = board_options(x), ...) {

  ns <- NS(id)

  opt_ui_or_null <- function(plg, plgs, x) {
    if (plg %in% names(plgs)) board_ui(id, plgs[[plg]], x)
  }

  stopifnot(is_string(id))

  tagList(
    show_hide_block_dep(),
    blockr_dock_dep(),
    # Navbar with slots for providers
    navbar_ui(ns("navbar"), x),
    # Settings offcanvas (opened by navbar settings button)
    options_offcanvas_ui(
      ns("navbar"),
      x,
      options_ui = tagList(
        div(
          id = "preserve_board",
          class = "mb-1",
          opt_ui_or_null("preserve_board", plugins, x)
        ),
        div(
          id = "generate_code",
          opt_ui_or_null("generate_code", plugins, x)
        ),
        board_options_ui(id, options)
      ),
      extensions_ui = lapply(
        dock_extensions(x),
        extension_ui,
        id = id,
        board = x
      )
    ),
    # Block editing offcanvas
    off_canvas(
      id = ns("blocks_offcanvas"),
      title = "Offcanvas blocks",
      block_ui(id, x, plugins[["edit_block"]])
    ),
    # Dockview container
    dockViewR::dock_view_output(
      ns(dock_id()),
      width = "100%",
      height = "calc(100vh - 48px)"
    )
  )
}

board_options_ui <- function(id, x) {
  stopifnot(is_board_options(x))

  opts <- split(x, chr_ply(x, attr, "category"))

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
}

blockr_dock_dep <- function() {
  htmltools::htmlDependency(
    "blockr-fab",
    pkg_version(),
    src = pkg_file("assets", "css"),
    stylesheet = "blockr-dock.css"
  )
}
