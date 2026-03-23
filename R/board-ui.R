#' @export
board_ui.dock_board <- function(id, x, plugins = board_plugins(x),
                                options = board_options(x), ...) {

  opt_ui_or_null <- function(plg, plgs, x) {
    if (plg %in% names(plgs)) board_ui(id, plgs[[plg]], x)
  }

  stopifnot(is_string(id))

  offcanvas_id <- NS(id, "options_offcanvas")

  if (has_workspaces(x)) {
    dock_area <- workspace_dock_ui(id, x)
  } else {
    dock_area <- dockViewR::dock_view_output(
      NS(id, dock_id()),
      width = "100%",
      height = "calc(100vh - 48px)"
    )
  }

  tagList(
    tags$script(HTML(sprintf(
      "$(document).on('shiny:connected', function() {
        Shiny.setInputValue('%s', window.innerWidth);
      });",
      NS(id, "screen_width")
    ))),
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
        class = "blockr-navbar-center",
        if (has_workspaces(x)) workspace_dropdown_ui(id, x)
      ),
      div(
        class = "blockr-navbar-right",
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
    dock_area,
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

workspace_dock_ui <- function(id, x) {
  ws_names <- names(dock_workspaces(x))
  dock_height <- "calc(100vh - 48px)"

  div(
    class = "blockr-workspace-container",
    style = sprintf("position: relative; height: %s;", dock_height),
    lapply(seq_along(ws_names), function(i) {
      ws <- ws_names[i]
      is_active <- i == 1L
      div(
        class = paste("blockr-workspace", if (is_active) "active"),
        id = paste0("workspace-", ws),
        dockViewR::dock_view_output(
          NS(id, dock_id(workspace = ws)),
          width = "100%",
          height = "100%"
        )
      )
    })
  )
}

workspace_dropdown_ui <- function(id, x) {
  ws_names <- names(dock_workspaces(x))
  if (length(ws_names) <= 1L) return(NULL)

  ns_id <- NS(id, "")

  div(
    class = "blockr-workspace-dropdown",
    `data-ns` = ns_id,
    tags$button(
      class = "btn btn-default dropdown-toggle",
      `data-bs-toggle` = "dropdown",
      tags$span(class = "active-ws-label", ws_names[[1]]),
      tags$span(class = "caret")
    ),
    tags$ul(
      class = "dropdown-menu workspace-dropdown-menu",
      lapply(ws_names, function(ws) {
        tags$li(
          tags$a(
            href = "#",
            class = paste("workspace-item", if (ws == ws_names[[1]]) "active"),
            `data-workspace` = ws,
            ws
          )
        )
      })
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
