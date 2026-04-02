#' @export
board_ui.dock_board <- function(id, x, plugins = board_plugins(x),
                                options = board_options(x), ...) {

  opt_ui_or_null <- function(plg, plgs, x) {
    if (plg %in% names(plgs)) board_ui(id, plgs[[plg]], x)
  }

  stopifnot(is_string(id))

  offcanvas_id <- NS(id, "options_offcanvas")
  workspaces <- board_workspaces(x)

  # Workspace nav in the navbar (only when workspaces are defined)
  ws_nav <- NULL
  if (!is.null(workspaces)) {
    ws_nav <- workspace_nav_ui(id, workspaces)
  }

  # Dock output(s): one per workspace, or a single one without workspaces
  dock_outputs <- dock_outputs_ui(id, workspaces)

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
        ws_nav,
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

# Generate dockview output(s). When workspaces are defined, one dock per
# workspace stacked via CSS; otherwise a single dock.
# Each dock output is placed inside a moduleServer namespace so that
# manage_dock(id, ...) can render to session$output[[dock_id()]] and
# dockViewR inputs resolve correctly.
dock_outputs_ui <- function(id, workspaces) {

  dock_height <- "calc(100vh - 48px)"

  if (is.null(workspaces)) {
    return(
      dockViewR::dock_view_output(
        NS(NS(id, "dock_main"), dock_id()),
        width = "100%",
        height = dock_height
      )
    )
  }

  div(
    id = NS(id, "ws_container"),
    class = "blockr-ws-container",
    style = paste0("position: relative; height: ", dock_height, ";")
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
