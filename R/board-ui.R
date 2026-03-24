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
  leaves <- ws_leaves(workspaces)
  leaf_names <- names(leaves)
  enabled <- Filter(function(nm) !isTRUE(leaves[[nm]][["disabled"]]), leaf_names)
  first_leaf <- enabled[1L]
  dock_height <- "calc(100vh - 48px)"

  div(
    class = "blockr-workspace-container",
    id = NS(id, "workspace-container"),
    style = sprintf("position: relative; height: %s;", dock_height),
    lapply(leaf_names, function(ws) {
      if (isTRUE(leaves[[ws]][["disabled"]])) return(NULL)
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
  leaves <- ws_leaves(workspaces)
  enabled <- Filter(function(nm) !isTRUE(leaves[[nm]][["disabled"]]), names(leaves))
  first_leaf <- enabled[1L]

  # First top-level entry that has at least one enabled leaf
  first_top <- Find(function(nm) {
    ws <- workspaces[[nm]]
    if (is_ws_parent(ws)) {
      any(!vapply(ws[["children"]], function(c) isTRUE(c[["disabled"]]), logical(1L)))
    } else {
      !isTRUE(ws[["disabled"]])
    }
  }, names(workspaces))

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

        # Parent is disabled if ALL children are disabled
        all_disabled <- all(
          vapply(children, function(c) isTRUE(c[["disabled"]]), logical(1L))
        )

        parent_classes <- paste(
          "nav-link dropdown-toggle workspace-tab-parent",
          if (is_active) "active",
          if (all_disabled) "disabled"
        )

        parent_link <- tags$a(
          class = parent_classes,
          href = "#",
          `data-bs-toggle` = if (!all_disabled) "dropdown",
          role = "button",
          `aria-expanded` = "false",
          `data-parent` = nm,
          tags$span(class = "ws-parent-label", nm),
          tags$span(
            class = "ws-active-child",
            if (is_active) paste0(" / ", first_child)
          )
        )
        if (all_disabled) {
          parent_link <- tagAppendAttributes(
            parent_link, `aria-disabled` = "true"
          )
        }

        tags$li(
          class = "nav-item dropdown",
          parent_link,
          tags$ul(
            class = "dropdown-menu workspace-child-menu",
            lapply(names(children), function(cnm) {
              is_first <- cnm == first_child && is_active
              child_disabled <- isTRUE(children[[cnm]][["disabled"]])

              child_classes <- paste(
                "dropdown-item workspace-child-item",
                if (is_first) "active",
                if (child_disabled) "disabled"
              )

              child_link <- tags$a(
                class = child_classes,
                href = "#",
                `data-workspace` = cnm,
                `data-parent` = nm,
                tags$span(class = "ws-tab-label", cnm),
                if (!child_disabled) tags$span(
                  class = "ws-tab-actions",
                  workspace_edit_icon(),
                  workspace_delete_icon(id, cnm)
                )
              )
              if (child_disabled) {
                child_link <- tagAppendAttributes(
                  child_link, `aria-disabled` = "true"
                )
              }

              tags$li(child_link)
            })
          )
        )
      } else {
        # Leaf tab — no dropdown
        is_active <- nm == first_leaf
        is_disabled <- isTRUE(ws[["disabled"]])

        tab_classes <- paste(
          "nav-link workspace-tab",
          if (is_active) "active",
          if (is_disabled) "disabled"
        )

        tab_link <- tags$a(
          class = tab_classes,
          href = "#",
          `data-workspace` = nm,
          tags$span(class = "ws-tab-label", nm),
          if (!is_disabled) tags$span(
            class = "ws-tab-actions",
            workspace_edit_icon(),
            workspace_delete_icon(id, nm)
          )
        )
        if (is_disabled) {
          tab_link <- tagAppendAttributes(
            tab_link, `aria-disabled` = "true"
          )
        }

        tags$li(class = "nav-item", tab_link)
      }
    }),
    # New workspace button
    tags$li(
      class = "nav-item",
      tags$a(
        href = "#",
        class = "nav-link workspace-new-btn",
        bsicons::bs_icon("plus")
      )
    )
  )
}

workspace_edit_icon <- function() {
  tags$span(
    class = "ws-edit-icon",
    bsicons::bs_icon("pencil")
  )
}

workspace_delete_icon <- function(ns, ws_name) {
  if (is.character(ns)) {
    ns_prefix <- NS(ns, "")
  } else {
    ns_prefix <- ns("")
  }
  tags$span(
    class = "ws-delete-icon",
    tabindex = "0",
    `data-workspace` = ws_name,
    `data-ns` = ns_prefix,
    bsicons::bs_icon("x")
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
