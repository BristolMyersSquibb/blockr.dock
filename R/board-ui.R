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
        if (has_workspaces(x)) workspace_tabs_ui(id, x)
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
  workspaces <- dock_workspaces(x)
  leaf_names <- ws_leaf_names(workspaces)
  first_leaf <- leaf_names[1L]
  dock_height <- "calc(100vh - 48px)"

  div(
    class = "blockr-workspace-container",
    id = NS(id, "workspace-container"),
    style = sprintf("position: relative; height: %s;", dock_height),
    lapply(seq_along(leaf_names), function(i) {
      ws <- leaf_names[i]
      is_active <- ws == first_leaf
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

workspace_tabs_ui <- function(id, x) {
  workspaces <- dock_workspaces(x)
  first_leaf <- ws_leaf_names(workspaces)[1L]
  first_top <- names(workspaces)[1L]

  tags$ul(
    class = "nav nav-tabs blockr-workspace-tabs",
    `data-ns` = NS(id, ""),
    lapply(names(workspaces), function(nm) {
      ws <- workspaces[[nm]]

      if (is_ws_parent(ws)) {
        # Parent tab with dropdown for children
        children <- ws[["children"]]
        first_child <- names(children)[1L]
        is_active <- nm == first_top

        tags$li(
          class = "nav-item dropdown",
          tags$a(
            class = paste("nav-link dropdown-toggle workspace-tab-parent",
                          if (is_active) "active"),
            href = "#",
            `data-bs-toggle` = "dropdown",
            role = "button",
            `aria-expanded` = "false",
            `data-parent` = nm,
            tags$span(class = "ws-parent-label", nm),
            tags$span(
              class = "ws-active-child",
              if (is_active) paste0(" / ", first_child)
            )
          ),
          tags$ul(
            class = "dropdown-menu workspace-child-menu",
            lapply(names(children), function(cnm) {
              is_first <- cnm == first_child && is_active
              tags$li(
                tags$a(
                  class = paste("dropdown-item workspace-child-item",
                                if (is_first) "active"),
                  href = "#",
                  `data-workspace` = cnm,
                  `data-parent` = nm,
                  cnm
                )
              )
            })
          )
        )
      } else {
        # Leaf tab — no dropdown
        is_active <- nm == first_leaf

        tags$li(
          class = "nav-item",
          tags$a(
            class = paste("nav-link workspace-tab",
                          if (is_active) "active"),
            href = "#",
            `data-workspace` = nm,
            nm
          )
        )
      }
    })
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
