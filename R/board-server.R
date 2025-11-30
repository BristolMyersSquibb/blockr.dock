board_server_callback <- function(board, update, ..., session = get_session()) {
  dock <- manage_dock(board, update, session)

  exts <- isolate(
    dock_extensions(board$board)
  )

  intercom <- set_names(
    replicate(length(exts), reactiveVal()),
    exts
  )

  ext_state <- lapply(
    as.list(exts),
    extension_server,
    list(board = board, update = update, dock = dock),
    intercom,
    list(...)
  )

  c(
    list(dock = dock),
    ext_state
  )
}

manage_dock <- function(board, update, session = get_session()) {
  dock <- set_dock_view_output(session = session)

  input <- session$input

  if (get_log_level() >= debug_log_level) {
    observeEvent(
      input[[dock_input("active-group")]],
      {
        ag <- input[[dock_input("active-group")]] # nolint: object_usage_linter.
        log_debug("active group is now {ag}")
      }
    )
  }

  observeEvent(
    req(input[[dock_input("initialized")]]),
    {
      layout <- dock_layout(board$board)

      restore_dock(layout, dock)

      for (id in as_dock_panel_id(layout)) {
        if (is_block_panel_id(id)) {
          show_block_panel(id, add_panel = FALSE, proxy = dock)
        } else if (is_ext_panel_id(id)) {
          show_ext_panel(id, add_panel = FALSE, proxy = dock)
        } else {
          blockr_abort(
            "Unknown panel type {class(id)}.",
            class = "dock_panel_invalid"
          )
        }
      }
    },
    once = TRUE
  )

  observeEvent(
    input[[dock_input("panel-to-remove")]],
    {
      id <- as_dock_panel_id(
        input[[dock_input("panel-to-remove")]]
      )

      if (is_block_panel_id(id)) {
        hide_block_panel(id, rm_panel = TRUE, proxy = dock)
      } else if (is_ext_panel_id(id)) {
        hide_ext_panel(id, rm_panel = TRUE, proxy = dock)
      } else {
        blockr_abort(
          "Unknown panel type {class(id)}.",
          class = "dock_panel_invalid"
        )
      }
    }
  )

  observeEvent(
    input[[dock_input("panel-to-add")]],
    suggest_panels_to_add(dock, board, session)
  )

  empty_layout <- reactive(
    {
      req(input[[dock_input("initialized")]])
      n_pan <- coal(
        input[[dock_input("n-panels")]],
        length(determine_active_views(dock_layout(board$board)))
      )
      req(n_pan == 0)
    }
  )

  observeEvent(
    empty_layout(),
    suggest_panels_to_add(dock, board, session)
  )

  observeEvent(
    input$confirm_add,
    {
      req(input$add_dock_panel)

      pos <- list(
        referenceGroup = input[[dock_input("panel-to-add")]],
        direction = "within"
      )

      for (id in input$add_dock_panel) {
        if (grepl("^blk-", id)) {
          show_block_panel(
            board_blocks(board$board)[sub("^blk-", "", id)],
            add_panel = pos,
            proxy = dock
          )
        } else if (grepl("^ext-", id)) {
          exts <- as.list(dock_extensions(board$board))

          show_ext_panel(
            exts[[sub("^ext-", "", id)]],
            add_panel = pos,
            proxy = dock
          )
        } else {
          blockr_abort(
            "Unknown panel specification {id}.",
            class = "dock_panel_invalid"
          )
        }
      }

      removeModal()
    }
  )

  observeEvent(
    input$cancel_add,
    removeModal()
  )

  prev_active_group <- reactiveVal()
  active_group_trail <- reactiveVal()

  observeEvent(
    input[[dock_input("active-group")]],
    {
      cur_ag <- input[[dock_input("active-group")]]
      pre_ag <- active_group_trail()
      if (!identical(pre_ag, cur_ag)) {
        log_trace("setting previous active group to {pre_ag}")
        prev_active_group(pre_ag)
      }
      active_group_trail(cur_ag)
    }
  )

  # Update panel
  # Panel name update
  # When a block is modified we have to update
  # the node data
  observeEvent(update()$blocks$mod, {
    blks <- update()$blocks$mod
    # Iterate over modified blocks and update panel titles
    for (id in names(blks)) {
      blk <- blks[[id]]
      new_name <- block_name(blk)
      blk_panel_id <- as_block_panel_id(id)

      old_title <- dockViewR::get_panels(dock)[[blk_panel_id]]$title
      if (new_name == old_title) {
        next
      }
      dockViewR::set_panel_title(
        dock,
        blk_panel_id,
        new_name
      )
    }
  })

  list(
    layout = reactive(dockViewR::get_dock(dock)),
    proxy = dock,
    prev_active_group = prev_active_group
  )
}


suggest_panels_to_add <- function(dock, board, session) {
  ns <- session$ns

  panels <- dock_panel_ids(dock)

  if (length(panels) == 0L) {
    panels <- list()
  } else if (length(panels) == 1L) {
    panels <- list(panels)
  }

  stopifnot(is.list(panels), all(lgl_ply(panels, is_dock_panel_id)))

  options_data <- list()

  # Get available blocks
  blk_opts <- setdiff(
    board_block_ids(board$board),
    as_obj_id(panels[lgl_ply(panels, is_block_panel_id)])
  )

  if (length(blk_opts)) {
    blks <- board_blocks(board$board)[blk_opts]
    meta <- blks_metadata(blks)

    for (i in seq_along(blk_opts)) {
      id <- blk_opts[i]
      options_data[[length(options_data) + 1L]] <- list(
        value = paste0("blk-", id),
        label = block_name(blks[[id]]),
        description = paste0("ID: ", id),
        category = "block",
        package = meta$package[i],
        icon = meta$icon[i],
        color = meta$color[i],
        searchtext = paste(block_name(blks[[id]]), id, meta$package[i])
      )
    }
  }

  # Get available extensions
  ext_opts <- setdiff(
    dock_ext_ids(board$board),
    as_obj_id(panels[lgl_ply(panels, is_ext_panel_id)])
  )

  if (length(ext_opts)) {
    all_exts <- as.list(dock_extensions(board$board))

    for (ext_id in ext_opts) {
      ext <- all_exts[[ext_id]]
      ext_name <- extension_name(ext)
      ext_pkg <- ctor_pkg(extension_ctor(ext))

      options_data[[length(options_data) + 1L]] <- list(
        value = paste0("ext-", ext_id),
        label = ext_name,
        description = paste0("ID: ", ext_id),
        category = "extension",
        package = coal(ext_pkg, "local"),
        icon = extension_default_icon(),
        color = "#999999",
        searchtext = paste(ext_name, ext_id, ext_pkg)
      )
    }
  }

  if (length(options_data)) {
    showModal(
      modalDialog(
        title = "Add panel",
        size = "l",
        easyClose = TRUE,
        footer = NULL,
        tagList(
          css_panel_modal(),
          css_panel_selectize(),
          selectizeInput(
            ns("add_dock_panel"),
            label = "Select panel to add",
            choices = NULL,
            multiple = TRUE,
            options = list(
              options = options_data,
              valueField = "value",
              labelField = "label",
              searchField = c("label", "description", "searchtext"),
              placeholder = "Type to search...",
              openOnFocus = FALSE,
              plugins = list("remove_button"),
              render = js_panel_selectize_render()
            )
          ),
          div(
            style = "display: flex; justify-content: flex-end; margin-top: 20px;",
            actionButton(
              ns("confirm_add"),
              label = "Add Panel",
              class = "btn-primary"
            )
          ),
          tags$script(
            HTML(
              sprintf(
                "$('#shiny-modal').on(
                  'shown.bs.modal',
                  function() {
                    $('#%s')[0].selectize.focus();
                  }
                );",
                ns("add_dock_panel")
              )
            )
          )
        )
      )
    )
  } else {
    notify("No further panels can be added. Remove some panels first.")
  }
}

# Panel modal helper functions ------------------------------------------------

extension_default_icon <- function() {
  as.character(bsicons::bs_icon("gear"))
}

css_panel_modal <- function() {
  tags$style(
    HTML(
      "#shiny-modal .modal-header {
        padding: 12px 20px;
        border-bottom: 1px solid #e2e8f0;
      }
      #shiny-modal .modal-title {
        font-size: 1.125rem;
        font-weight: 600;
        margin: 0;
      }
      #shiny-modal .modal-body {
        padding: 20px;
      }
      #shiny-modal .modal-body .form-group {
        width: 100%;
        margin-bottom: 16px;
      }
      #shiny-modal .modal-body .selectize-input,
      #shiny-modal .modal-body input[type='text'] {
        width: 100%;
      }
      #shiny-modal .modal-body .shiny-input-container {
        width: 100%;
      }
      #shiny-modal .modal-body .control-label {
        font-size: 0.875rem;
        color: #6c757d;
        margin-bottom: 4px;
        font-weight: normal;
      }
      #shiny-modal .modal-footer {
        padding: 12px 20px;
        border-top: 1px solid #e2e8f0;
        gap: 8px;
      }
      #shiny-modal .modal-footer .btn {
        font-size: 0.875rem;
        padding: 0.375rem 0.75rem;
      }"
    )
  )
}

css_panel_selectize <- function() {
  tags$style(
    HTML(
      ".selectize-dropdown-content {
        max-height: 450px !important;
        padding: 8px 0;
      }
      .panel-option {
        padding: 16px 24px;
        display: flex;
        align-items: flex-start;
        gap: 16px;
        margin: 4px 8px;
        border-radius: 6px;
        transition: background-color 0.15s ease;
      }
      .panel-icon-wrapper {
        flex-shrink: 0;
        width: 40px;
        height: 40px;
        border-radius: 8px;
        display: flex;
        align-items: center;
        justify-content: center;
        font-size: 18px;
      }
      .panel-content {
        flex: 1;
        min-width: 0;
      }
      .panel-header {
        display: flex;
        align-items: flex-start;
        justify-content: space-between;
        gap: 10px;
        margin-bottom: 4px;
      }
      .panel-name {
        font-weight: 600;
        font-size: 15px;
        color: #212529;
        flex: 1;
      }
      .panel-desc {
        font-size: 13px;
        color: #6c757d;
        line-height: 1.4;
      }
      .badge-two-tone {
        display: inline-block;
        padding: 0.125rem 0.375rem;
        font-size: 0.625rem;
        border-radius: 0.25rem;
        background-color: rgba(148, 163, 184, 0.1);
        color: rgba(100, 116, 139, 0.9);
        border: 1px solid rgba(100, 116, 139, 0.1);
        white-space: nowrap;
        flex-shrink: 0;
      }
      .selectize-dropdown .panel-option {
        border-bottom: 1px solid #f0f0f0;
      }
      .selectize-dropdown .panel-option:last-child {
        border-bottom: none;
      }
      .selectize-dropdown .panel-option:hover,
      .selectize-dropdown .panel-option.active {
        background-color: #e9ecef;
      }
      .selectize-input .remove {
        text-decoration: none !important;
        color: #6c757d !important;
        font-weight: normal !important;
        border: none !important;
        margin-left: 8px !important;
        padding: 2px 6px !important;
        border-radius: 3px !important;
        transition: background-color 0.2s ease, color 0.2s ease;
      }
      .selectize-input .remove:hover {
        background-color: rgba(108, 117, 125, 0.1) !important;
        color: #495057 !important;
      }"
    )
  )
}

js_panel_selectize_render <- function() {
  icon_style <- blockr_option("icon_style", "light")

  I(
    sprintf(
      "(function() {
      var iconStyle = '%s';

      var hexToRgba = function(hex, alpha) {
        var r = parseInt(hex.slice(1, 3), 16);
        var g = parseInt(hex.slice(3, 5), 16);
        var b = parseInt(hex.slice(5, 7), 16);
        return 'rgba(' + r + ', ' + g + ', ' + b + ', ' + alpha + ')';
      };

      var getIconColors = function(color) {
        if (iconStyle === 'light') {
          return {
            iconFill: color,
            bgColor: hexToRgba(color, 0.3)
          };
        } else {
          return {
            iconFill: 'white',
            bgColor: color
          };
        }
      };

      var styleIcon = function(iconSvg, color, size) {
        var colors = getIconColors(color);
        var cleanSvg = iconSvg;
        var styleStart = cleanSvg.indexOf('style=\"');
        if (styleStart !== -1) {
          var styleEnd = cleanSvg.indexOf('\"', styleStart + 7);
          if (styleEnd !== -1) {
            cleanSvg = cleanSvg.substring(0, styleStart) +
                       cleanSvg.substring(styleEnd + 1);
          }
        }
        return {
          svg: cleanSvg.replace(
            '<svg ',
            '<svg style=\"width: ' + size + 'px; height: ' + size +
            'px; fill: ' + colors.iconFill + ';\" '
          ),
          bgColor: colors.bgColor
        };
      };

      return {
        item: function(item, escape) {
          var name = escape(item.label);
          var pkg = escape(item.package || '');
          var color = item.color || '#6c757d';
          var iconSvg = item.icon || '';

          var styledIcon = styleIcon(iconSvg, color, 14);
          var styledSvg = styledIcon.svg;
          var bgColor = styledIcon.bgColor;

          var containerStyle =
            'display: inline-flex; align-items: center; gap: 8px; ' +
            'padding: 4px 8px; background-color: #f8f9fa; ' +
            'border-radius: 6px; border: 1px solid #e9ecef;';
          var iconWrapperStyle =
            'background-color: ' + bgColor + '; width: 24px; ' +
            'height: 24px; border-radius: 4px; display: flex; ' +
            'align-items: center; justify-content: center; flex-shrink: 0;';
          var pkgBadgeHtml = pkg ?
            '<div class=\"badge-two-tone\" style=\"margin-left: 4px;\">' +
            pkg + '</div>' : '';
          return '<div style=\"' + containerStyle + '\">' +
                 '<div style=\"' + iconWrapperStyle + '\">' +
                 styledSvg + '</div>' +
                 '<div style=\"font-weight: 500; font-size: 14px;\">' +
                 name + '</div>' + pkgBadgeHtml + '</div>';
        },
        option: function(item, escape) {
          var name = escape(item.label);
          var desc = escape(item.description || '');
          var pkg = escape(item.package || '');
          var color = item.color || '#6c757d';
          var iconSvg = item.icon || '';

          var styledIcon = styleIcon(iconSvg, color, 20);
          var styledSvg = styledIcon.svg;
          var bgColor = styledIcon.bgColor;

          var iconWrapperStyle = 'background-color: ' + bgColor + ';';
          var iconWrapper = '<div class=\"panel-icon-wrapper\" ' +
                            'style=\"' + iconWrapperStyle + '\">' +
                            styledSvg + '</div>';

          var pkgBadge = pkg ?
                         '<div class=\"badge-two-tone\">' + pkg +
                         '</div>' : '';

          var descHtml = desc ?
            '<div class=\"panel-desc\">' + desc + '</div>' : '';

          return '<div class=\"panel-option\">' + iconWrapper +
                   '<div class=\"panel-content\">' +
                     '<div class=\"panel-header\">' +
                       '<div class=\"panel-name\">' + name +
                       '</div>' + pkgBadge + '</div>' +
                     descHtml + '</div>' + '</div>';
        }
      };
    })()",
      icon_style
    )
  )
}
