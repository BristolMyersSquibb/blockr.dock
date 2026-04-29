#' @export
board_ui.dock_board <- function(id, x, plugins = board_plugins(x),
                                options = board_options(x), ...) {

  opt_ui_or_null <- function(plg, plgs, x) {
    if (plg %in% names(plgs)) board_ui(id, plgs[[plg]], x)
  }

  stopifnot(is_string(id))

  offcanvas_id <- NS(id, "options_offcanvas")
  views <- board_views(x)

  # View nav in the navbar -- always present, since boards always carry a
  # `dock_layouts` (single-page boards have one auto-named "Page" view).
  v_nav <- view_nav_ui(id, views)

  # One dock output per view, stacked inside the view container; visibility
  # is toggled by CSS based on the active view.
  dock_outputs <- dock_outputs_ui(id, views)

  tagList(
    show_hide_block_dep(),
    blockr_dock_dep(),
    off_canvas(
      id = NS(id, "blocks_offcanvas"),
      title = "Offcanvas blocks",
      block_ui(id, x, plugins[["edit_block"]])
    ),
    div(
      class = "blockr-navbar",
      div(
        class = "blockr-navbar-left",
        opt_ui_or_null("preserve_board", plugins, x)
      ),
      div(
        class = "blockr-navbar-right",
        v_nav,
        tags$button(
          class = "blockr-navbar-icon-btn",
          `data-bs-toggle` = "offcanvas",
          `data-bs-target` = paste0("#", offcanvas_id),
          bsicons::bs_icon("gear")
        )
      )
    ),
    options_ui(
      id,
      options,
      div(
        id = "generate_code",
        opt_ui_or_null("generate_code", plugins, x)
      )
    ),
    dock_outputs,
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

# View container -- one dock output per view is inserted into this element
# at server time by `init_view_docks()`. Each dock output sits inside a
# nested moduleServer namespace so `manage_dock(id, ...)` can render to
# `session$output[[dock_id()]]` and dockViewR inputs resolve correctly.
dock_outputs_ui <- function(id, views) {
  div(
    id = NS(id, "view_container"),
    class = "blockr-view-container",
    style = "position: relative; height: calc(100vh - 48px);"
  )
}

options_ui <- function(id, x, ...) {

  stopifnot(is_board_options(x))

  opts <- split(x, chr_ply(x, attr, "category"))

  off_canvas(
    id = NS(id, "options_offcanvas"),
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
}

blockr_dock_dep <- function() {
  htmltools::htmlDependency(
    "blockr-fab",
    pkg_version(),
    src = pkg_file("assets", "css"),
    stylesheet = "blockr-dock.css"
  )
}
