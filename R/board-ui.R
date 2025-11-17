#' @export
board_ui.dock_board <- function(id, x, plugins = board_plugins(x),
                                options = board_options(x), ...) {

  opt_ui_or_null <- function(plg, plgs, x) {
    if (plg %in% names(plgs)) board_ui(id, plgs[[plg]], x)
  }

  stopifnot(is_string(id))

  tagList(
    show_hide_block_dep(),
    off_canvas(
      id = NS(id, "blocks_offcanvas"),
      title = "Offcanvas blocks",
      block_ui(id, x, edit_ui = plugins[["edit_block"]])
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
    dockViewR::dock_view_output(
      NS(id, dock_id()),
      width = "100%",
      height = "100vh"
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

options_ui <- function(id, x, ...) {

  stopifnot(is_board_options(x))

  opts <- split(x, chr_ply(x, attr, "category"))

  offcanvas_id <- NS(id, "options_offcanvas")

  tagList(
    blockr_fab_dep(),
    tags$button(
      class = "blockr-fab",
      icon("gear"),
      `data-bs-toggle` = "offcanvas",
      `data-bs-target` = paste0("#", offcanvas_id),
      `aria-controls` = offcanvas_id
    ),
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

blockr_fab_dep <- function() {
  htmltools::htmlDependency(
    "blockr-fab",
    pkg_version(),
    src = pkg_file("assets", "css"),
    stylesheet = "blockr-fab.css"
  )
}
