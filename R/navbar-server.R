#' Navbar Server
#'
#' Server logic for the top navbar, handling:
#' - Save workflow functionality
#' - Recent workflows list
#' - Load workflow
#' - Editable workflow title
#'
#' @param id Namespace ID (not used, kept for compatibility)
#' @param board Reactive values object containing board state
#' @param dock Dock object from manage_dock() for serialization
#' @param ... Extra arguments
#' @param session Shiny session (already namespaced)
#'
#' @keywords internal
navbar_server <- function(id, board, dock = NULL, ..., session = get_session()) {

  # Note: We don't use moduleServer here because we're already inside
  # the board's namespace. The session passed in is already correctly

  # namespaced, so input$save_btn etc. will work directly.

  input <- session$input
  output <- session$output

  message("=== NAVBAR SERVER INITIALIZED ===")
  message("Session NS: ", session$ns("test"))

  backend <- blockr_option("session_mgmt_backend", pins::board_local())
  message("Backend initialized: ", class(backend))

  # Track when workflows were last refreshed
  refresh_trigger <- reactiveVal(0)

  # Result reactiveVal for restore_board (needed for dock_board serialization)
  restore_result <- reactiveVal()

  # Observer to trigger session reload when a board is restored
  # This mirrors what blockr.core's preserve_board plugin does
  observeEvent(
    restore_result(),
    {
      message("=== NAVBAR RESTORE RESULT CHANGED ===")
      message("Calling update_serve_obj and session$reload()")

      # Update the serve object with the new board (like preserve_board does)
      blockr.core:::update_serve_obj(restore_result(), "reload")

      # Reload the session to pick up the restored board
      session$reload()
    }
  )

  # === SAVE FUNCTIONALITY ===
  observeEvent(
    input$save_btn,
    {
      message("=== NAVBAR SAVE BUTTON CLICKED ===")
      message("input$save_btn value: ", input$save_btn)
      message("Backend class: ", class(backend))
      message("Board class: ", class(board))
      message("Board names: ", paste(names(board), collapse = ", "))

      # Check if blockr.session is available
      if (!requireNamespace("blockr.session", quietly = TRUE)) {
        message("ERROR: blockr.session package not available!")
        showNotification(
          "blockr.session package not loaded",
          type = "error",
          session = session
        )
        return()
      }

      message("blockr.session is available")
      message("Attempting to save...")

      res <- tryCatch(
        {
          result <- blockr.session:::upload_board(backend, board, dock = dock, session = session)
          message("Upload result: ", result)
          result
        },
        error = function(e) {
          message("ERROR during save: ", e$message)
          showNotification(e$message, type = "error", session = session)
          NULL
        }
      )

      if (!is.null(res)) {
        message("Save successful: ", res)
        showNotification(
          paste("Successfully saved", res),
          type = "message",
          session = session
        )
        # Update save status to "Just now"
        session$sendCustomMessage("blockr-update-save-status", "Just now")
        # Trigger refresh of workflow list
        refresh_trigger(refresh_trigger() + 1)
      } else {
        message("Save returned NULL")
      }
    }
  )

  # === NEW WORKFLOW ===
  observeEvent(
    input$new_btn,
    {
      message("=== NAVBAR NEW BUTTON CLICKED ===")

      # Create a fresh dock board with the same extensions as the current board
      new_board <- new_dock_board(
        extensions = dock_extensions(board$board)
      )

      message("Created new board with extensions: ",
              paste(class(dock_extensions(board$board)), collapse = ", "))

      # Update the serve object and reload
      blockr.core:::update_serve_obj(new_board, "reload")
      session$reload()
    }
  )

  # === RECENT WORKFLOWS (reactive) ===
  recent_workflows <- reactive({
    # Depend on refresh trigger to update after saves
    refresh_trigger()

    # Get all saved boards
    boards <- tryCatch(
      blockr.session:::pin_list(backend),
      error = function(e) character()
    )
    boards <- boards[boards != ""]

    if (length(boards) == 0) {
      return(list())
    }

    # Get metadata for each to sort by date
    workflow_info <- lapply(boards, function(name) {
      versions <- tryCatch(
        blockr.session:::pin_versions(name, backend),
        error = function(e) character()
      )
      if (length(versions) > 0) {
        meta <- tryCatch(
          pins::pin_meta(backend, name, versions[1]),
          error = function(e) NULL
        )
        if (!is.null(meta)) {
          list(
            name = name,
            modified = meta$created,
            version = versions[1]
          )
        }
      }
    })

    # Filter nulls and sort by modified date
    workflow_info <- Filter(Negate(is.null), workflow_info)

    if (length(workflow_info) == 0) {
      return(list())
    }

    workflow_info <- workflow_info[order(
      sapply(workflow_info, function(x) as.character(x$modified)),
      decreasing = TRUE
    )]

    # Return top 4
    head(workflow_info, 4)
  })

  # Render recent workflows list
  output$recent_workflows <- renderUI({
    workflows <- recent_workflows()

    if (length(workflows) == 0) {
      return(
        tags$div(
          class = "blockr-workflow-item blockr-workflow-empty",
          "No saved workflows yet"
        )
      )
    }

    tagList(
      lapply(workflows, function(wf) {
        # Check if this is the current workflow
        current_name <- coal(
          get_board_option_or_null("board_name", session),
          board$board_id
        )
        is_active <- identical(wf$name, current_name)

        tags$div(
          class = paste(
            "blockr-workflow-item",
            if (is_active) "active" else ""
          ),
          onclick = sprintf(
            "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
            session$ns("load_workflow"),
            wf$name
          ),
          tags$div(class = "blockr-workflow-name", wf$name),
          tags$div(
            class = "blockr-workflow-meta",
            format_time_ago(wf$modified)
          )
        )
      })
    )
  })

  # === LOAD WORKFLOW ===
  observeEvent(
    input$load_workflow,
    {
      req(input$load_workflow)

      message("=== NAVBAR LOAD WORKFLOW ===")
      message("Workflow to load: ", input$load_workflow)

      versions <- tryCatch(
        blockr.session:::pin_versions(input$load_workflow, backend),
        error = function(e) {
          message("ERROR getting versions: ", e$message)
          showNotification(e$message, type = "error", session = session)
          character()
        }
      )

      message("Versions found: ", length(versions))
      if (length(versions) > 0) message("Latest version: ", versions[1])

      if (length(versions) > 0) {
        meta <- tryCatch(
          pins::pin_meta(backend, input$load_workflow, versions[1]),
          error = function(e) {
            message("ERROR getting meta: ", e$message)
            showNotification(e$message, type = "error", session = session)
            NULL
          }
        )

        message("Meta retrieved: ", !is.null(meta))
        if (!is.null(meta)) {
          message("Meta pin_hash: ", meta$pin_hash)
          message("Meta user$format: ", meta$user$format)
        }

        if (!is.null(meta)) {
          board_ser <- tryCatch(
            blockr.session:::download_board(
              backend,
              input$load_workflow,
              versions[1],
              meta$pin_hash,
              meta$user$format
            ),
            error = function(e) {
              message("ERROR downloading board: ", e$message)
              showNotification(e$message, type = "error", session = session)
              NULL
            }
          )

          message("Board downloaded: ", !is.null(board_ser))
          if (!is.null(board_ser)) {
            message("Board ser class: ", class(board_ser))
            message("Board ser names: ", paste(names(board_ser), collapse = ", "))
          }

          if (!is.null(board_ser)) {
            message("Calling restore_board...")
            message("board$board class: ", class(board$board))
            message("restore_result class: ", class(restore_result))

            res <- tryCatch(
              {
                restore_board(board$board, board_ser, restore_result, session = session)
              },
              error = function(e) {
                message("ERROR in restore_board: ", e$message)
                message("Traceback: ", paste(capture.output(traceback()), collapse = "\n"))
                showNotification(paste("Restore error:", e$message), type = "error", session = session)
                NULL
              }
            )

            message("restore_board returned: ", class(res))

            showNotification(
              paste("Loaded", input$load_workflow),
              type = "message",
              session = session
            )
          }
        }
      }
    }
  )

  # === EDITABLE TITLE ===
  observeEvent(
    input$title_edit,
    {
      req(input$title_edit)
      set_board_option_value("board_name", input$title_edit, session)
    }
  )

  # Update title display when board name changes
  observe({
    current_name <- get_board_option_or_null("board_name", session)
    message("=== NAVBAR TITLE OBSERVE ===")
    message("Current board_name from options: ", current_name)
    if (!is.null(current_name) && nzchar(current_name)) {
      message("Sending title update message: ", current_name)
      session$sendCustomMessage(
        "blockr-update-navbar-title",
        current_name
      )

      # Also check if this workflow is saved and update save status
      tryCatch(
        {
          versions <- blockr.session:::pin_versions(current_name, backend)
          if (length(versions) > 0) {
            meta <- pins::pin_meta(backend, current_name, versions[1])
            if (!is.null(meta) && !is.null(meta$created)) {
              save_status <- format_time_ago(meta$created)
              message("Workflow last saved: ", save_status)
              session$sendCustomMessage("blockr-update-save-status", save_status)
            }
          }
        },
        error = function(e) {
          message("Could not get save status: ", e$message)
        }
      )
    }
  })

  # === VERSION HISTORY ===

  # Get versions for current workflow
  current_versions <- reactive({
    refresh_trigger()

    current_name <- coal(
      get_board_option_or_null("board_name", session),
      board$board_id
    )

    if (is.null(current_name) || !nzchar(current_name)) {
      return(NULL)
    }

    tryCatch({
      versions <- blockr.session:::pin_versions(current_name, backend)
      if (length(versions) == 0) return(NULL)

      # Get metadata for each version
      version_data <- lapply(seq_along(versions), function(i) {
        v <- versions[i]
        meta <- tryCatch(
          pins::pin_meta(backend, current_name, v),
          error = function(e) NULL
        )
        if (!is.null(meta)) {
          list(
            version = v,
            created = meta$created,
            is_current = i == 1
          )
        }
      })

      Filter(Negate(is.null), version_data)
    }, error = function(e) NULL)
  })

  # History title (shows current workflow name)
  output$history_title <- renderUI({
    current_name <- coal(
      get_board_option_or_null("board_name", session),
      board$board_id
    )
    if (is.null(current_name) || !nzchar(current_name)) {
      return(tags$span("Version history"))
    }
    tags$span(paste("Version history for", current_name))
  })

  # Render version history dropdown
  output$version_history <- renderUI({
    versions <- current_versions()

    if (is.null(versions) || length(versions) == 0) {
      return(
        tags$div(
          class = "blockr-version-empty",
          "No version history available"
        )
      )
    }

    current_name <- coal(
      get_board_option_or_null("board_name", session),
      board$board_id
    )

    # Separate current and previous versions
    current <- versions[[1]]
    previous <- if (length(versions) > 1) versions[-1] else list()

    tagList(
      # Current version section
      tags$div(
        class = "blockr-version-section",
        "CURRENT"
      ),
      tags$div(
        class = "blockr-version-item blockr-version-current",
        tags$div(
          class = "blockr-version-item-content",
          tags$div(class = "blockr-version-item-title", "Current version"),
          tags$div(
            class = "blockr-version-item-meta",
            paste("Saved", format_time_ago(current$created))
          )
        ),
        tags$span(class = "blockr-version-badge", "Current")
      ),

      # Previous versions section
      if (length(previous) > 0) {
        tagList(
          tags$div(
            class = "blockr-version-section",
            "PREVIOUS VERSIONS"
          ),
          lapply(previous[1:min(3, length(previous))], function(v) {
            tags$div(
              class = "blockr-version-item",
              tags$div(
                class = "blockr-version-item-content",
                tags$div(
                  class = "blockr-version-item-title",
                  paste("Version from", format_time_ago(v$created))
                ),
                tags$div(
                  class = "blockr-version-item-meta",
                  paste("Saved", format_time_ago(v$created))
                )
              ),
              tags$button(
                class = "blockr-version-restore-btn",
                onclick = sprintf(
                  "Shiny.setInputValue('%s', {name: '%s', version: '%s'}, {priority: 'event'})",
                  session$ns("restore_version"),
                  current_name,
                  v$version
                ),
                "Restore"
              )
            )
          }),
          if (length(previous) > 3) {
            tags$a(
              href = "#",
              class = "blockr-version-link",
              onclick = sprintf(
                "Shiny.setInputValue('%s', Date.now(), {priority: 'event'}); return false;",
                session$ns("view_full_history")
              ),
              "View full history ",
              bsicons::bs_icon("arrow-right")
            )
          }
        )
      }
    )
  })

  # Restore a specific version
  observeEvent(input$restore_version, {
    req(input$restore_version)

    workflow_name <- input$restore_version$name
    version <- input$restore_version$version

    message("=== RESTORING VERSION: ", workflow_name, " @ ", version, " ===")

    meta <- tryCatch(
      pins::pin_meta(backend, workflow_name, version),
      error = function(e) {
        showNotification(e$message, type = "error", session = session)
        NULL
      }
    )

    if (!is.null(meta)) {
      board_ser <- tryCatch(
        blockr.session:::download_board(
          backend,
          workflow_name,
          version,
          meta$pin_hash,
          meta$user$format
        ),
        error = function(e) {
          showNotification(e$message, type = "error", session = session)
          NULL
        }
      )

      if (!is.null(board_ser)) {
        restore_board(board$board, board_ser, restore_result, session = session)
        showNotification(
          paste("Restored version from", format_time_ago(meta$created)),
          type = "message",
          session = session
        )
      }
    }
  })

  # === WORKFLOW OVERVIEW ===

  # All workflows for overview table
 all_workflows <- reactive({
    refresh_trigger()

    boards <- tryCatch(
      blockr.session:::pin_list(backend),
      error = function(e) character()
    )
    boards <- boards[boards != ""]

    if (length(boards) == 0) {
      return(data.frame(
        name = character(),
        created = as.POSIXct(character()),
        modified = as.POSIXct(character()),
        version = character(),
        stringsAsFactors = FALSE
      ))
    }

    # Get metadata for all workflows
    workflow_data <- lapply(boards, function(name) {
      tryCatch({
        versions <- blockr.session:::pin_versions(name, backend)
        if (length(versions) > 0) {
          meta <- pins::pin_meta(backend, name, versions[1])
          if (!is.null(meta)) {
            data.frame(
              name = name,
              created = as.POSIXct(meta$created),
              modified = as.POSIXct(meta$created),
              version = versions[1],
              stringsAsFactors = FALSE
            )
          }
        }
      }, error = function(e) NULL)
    })

    result <- do.call(rbind, Filter(Negate(is.null), workflow_data))
    if (is.null(result)) {
      return(data.frame(
        name = character(),
        created = as.POSIXct(character()),
        modified = as.POSIXct(character()),
        version = character(),
        stringsAsFactors = FALSE
      ))
    }
    result
  })

  # Filtered workflows based on search
  filtered_workflows <- reactive({
    wf <- all_workflows()
    search_term <- input$workflow_search

    if (is.null(search_term) || !nzchar(search_term)) {
      return(wf)
    }

    wf[grepl(search_term, wf$name, ignore.case = TRUE), , drop = FALSE]
  })

  # Show workflow overview page
  observeEvent(input$view_all_workflows, {
    message("=== VIEW ALL WORKFLOWS CLICKED ===")
    session$sendCustomMessage("blockr-switch-view", "workflows")
  })

  # Back to board from workflow overview
  observeEvent(input$back_to_board, {
    message("=== BACK TO BOARD CLICKED ===")
    session$sendCustomMessage("blockr-switch-view", "board")
  })

  # Render workflow table
  output$workflow_table_ui <- renderUI({
    wf <- filtered_workflows()
    render_workflow_table(
      wf,
      session$ns,
      session$ns("load_from_overview"),
      session$ns("selected_workflows")
    )
  })

  # Workflow count
  output$workflow_count <- renderText({
    wf <- filtered_workflows()
    n <- if (is.null(wf)) 0 else nrow(wf)
    paste("Showing", n, "workflow", if (n != 1) "s" else "")
  })

  # Track workflows to delete
  workflows_to_delete <- reactiveVal(character())

  # Load from overview
  observeEvent(input$load_from_overview, {
    req(input$load_from_overview)
    message("=== LOAD FROM OVERVIEW: ", input$load_from_overview, " ===")
    # Switch back to board view
    session$sendCustomMessage("blockr-switch-view", "board")

    # Reuse existing load logic
    workflow_name <- input$load_from_overview

    versions <- tryCatch(
      blockr.session:::pin_versions(workflow_name, backend),
      error = function(e) {
        showNotification(e$message, type = "error", session = session)
        character()
      }
    )

    if (length(versions) > 0) {
      meta <- tryCatch(
        pins::pin_meta(backend, workflow_name, versions[1]),
        error = function(e) {
          showNotification(e$message, type = "error", session = session)
          NULL
        }
      )

      if (!is.null(meta)) {
        board_ser <- tryCatch(
          blockr.session:::download_board(
            backend,
            workflow_name,
            versions[1],
            meta$pin_hash,
            meta$user$format
          ),
          error = function(e) {
            showNotification(e$message, type = "error", session = session)
            NULL
          }
        )

        if (!is.null(board_ser)) {
          restore_board(board$board, board_ser, restore_result, session = session)
          showNotification(
            paste("Loaded", workflow_name),
            type = "message",
            session = session
          )
        }
      }
    }
  })

  # Delete selected workflows
  observeEvent(input$delete_selected, {
    selected <- input$selected_workflows
    req(length(selected) > 0)

    workflows_to_delete(selected)
    showModal(delete_confirm_modal(session$ns, length(selected)))
  })

  # Confirm delete
  observeEvent(input$confirm_delete, {
    to_delete <- workflows_to_delete()
    req(length(to_delete) > 0)

    message("=== DELETING WORKFLOWS: ", paste(to_delete, collapse = ", "), " ===")

    for (name in to_delete) {
      tryCatch(
        pins::pin_delete(backend, name),
        error = function(e) message("Failed to delete: ", name, " - ", e$message)
      )
    }

    workflows_to_delete(character())
    removeModal()
    refresh_trigger(refresh_trigger() + 1)

    showNotification(
      paste("Deleted", length(to_delete), "workflow", if (length(to_delete) != 1) "s" else ""),
      type = "message",
      session = session
    )
    # Table will auto-refresh via reactive dependency on refresh_trigger
  })

  # === USER AVATAR ===
  output$user_avatar <- renderUI({
    # Try session$user first (Shiny Server Pro / Posit Connect)
    # Fall back to system username for local development
    username <- coal(
      session$user,
      Sys.info()[["user"]],
      Sys.getenv("USER"),
      Sys.getenv("USERNAME"),
      "User"
    )

    initials <- get_initials(username)
    tags$span(class = "blockr-navbar-avatar", initials)
  })

  NULL
}

#' Get initials from a username
#'
#' @param username Character string
#' @return Character string with 1-2 uppercase initials
#' @keywords internal
get_initials <- function(username) {
  if (is.null(username) || !nzchar(username)) {
    return("U")
  }

 # Split by common separators (space, dot, underscore, hyphen)
  parts <- strsplit(username, "[._ -]+")[[1]]
  parts <- parts[nzchar(parts)]

  if (length(parts) >= 2) {
    # Take first letter of first two parts
    initials <- paste0(
      toupper(substr(parts[1], 1, 1)),
      toupper(substr(parts[2], 1, 1))
    )
  } else if (length(parts) == 1) {
    # Take first two letters of single name
    initials <- toupper(substr(parts[1], 1, 2))
  } else {
    initials <- "U"
  }

  initials
}

#' Format timestamp as relative time
#'
#' @param timestamp POSIXct timestamp
#' @return Character string like "2 min ago", "3 hours ago", etc.
#' @keywords internal
format_time_ago <- function(timestamp) {
  if (is.null(timestamp) || is.na(timestamp)) {
    return("Unknown")
  }

  diff <- difftime(Sys.time(), timestamp, units = "mins")
  mins <- as.numeric(diff)

  if (mins < 0) {
    return("Just now")
  } else if (mins < 1) {
    return("Just now")
  } else if (mins < 60) {
    return(paste0(round(mins), " min ago"))
  } else if (mins < 1440) {
    hours <- round(mins / 60)
    return(paste0(hours, " hour", if (hours != 1) "s" else "", " ago"))
  } else if (mins < 10080) {
    days <- round(mins / 1440)
    return(paste0(days, " day", if (days != 1) "s" else "", " ago"))
  } else {
    return(format(timestamp, "%b %d"))
  }
}

#' Coalesce - return first non-NULL value
#' @keywords internal
coal <- function(...) {
  for (x in list(...)) {
    if (!is.null(x)) return(x)
  }
  NULL
}
