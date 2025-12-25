#' Workflow Overview Page
#'
#' Full page showing all saved workflows in a reactable table.
#'
#' @param ns Namespace function
#' @keywords internal
workflow_overview_ui <- function(ns) {
  tags$div(
    class = "blockr-workflow-page",

    # Header with back button, search, and actions
    tags$div(
      class = "blockr-workflow-page-header",
      tags$div(
        class = "blockr-workflow-page-header-left",
        tags$button(
          class = "blockr-workflow-back-btn",
          onclick = sprintf(
            "Shiny.setInputValue('%s', Date.now(), {priority: 'event'})",
            ns("back_to_board")
          ),
          bsicons::bs_icon("arrow-left"),
          " Back"
        ),
        tags$h1(class = "blockr-workflow-page-title", "Workflows")
      ),
      tags$div(
        class = "blockr-workflow-page-header-right",
        # Delete button (shown when items selected)
        tags$div(
          id = ns("bulk_actions"),
          class = "blockr-workflow-bulk-actions",
          style = "display: none;",
          tags$button(
            id = ns("delete_selected"),
            class = "blockr-wf-delete-btn",
            onclick = sprintf(
              "Shiny.setInputValue('%s', Date.now(), {priority: 'event'})",
              ns("delete_selected")
            ),
            bsicons::bs_icon("trash"),
            " Delete selected"
          )
        )
      )
    ),

    # Search bar
    tags$div(
      class = "blockr-workflow-page-search",
      tags$div(
        class = "blockr-workflow-search-wrapper",
        tags$span(class = "blockr-workflow-search-icon", bsicons::bs_icon("search")),
        tags$input(
          id = ns("workflow_search"),
          type = "text",
          class = "form-control blockr-workflow-search shiny-bound-input",
          placeholder = "Search workflows...",
          oninput = sprintf(
            "Shiny.setInputValue('%s', this.value, {priority: 'event'})",
            ns("workflow_search")
          )
        )
      )
    ),

    # Table
    tags$div(
      class = "blockr-workflow-page-table",
      uiOutput(ns("workflow_table_ui"))
    ),

    # Footer
    tags$div(
      class = "blockr-workflow-page-footer",
      textOutput(ns("workflow_count"), inline = TRUE)
    ),

    # JavaScript for table sorting and selection
    tags$script(HTML(sprintf("
      // Sort table by column
      function blockrSortTable(tableId, column) {
        var table = document.getElementById(tableId);
        if (!table) return;

        var th = table.querySelector('th[data-sort=\"' + column + '\"]');
        var tbody = table.querySelector('tbody');
        var rows = Array.from(tbody.querySelectorAll('tr'));

        // Determine sort direction
        var isDesc = th.classList.contains('active-sort') && th.classList.contains('desc');
        var isAsc = th.classList.contains('active-sort') && th.classList.contains('asc');
        var newDir = isDesc ? 'asc' : 'desc';

        // Update header classes
        table.querySelectorAll('th.sortable').forEach(function(h) {
          h.classList.remove('active-sort', 'asc', 'desc');
        });
        th.classList.add('active-sort', newDir);

        // Sort rows
        rows.sort(function(a, b) {
          var aVal = a.getAttribute('data-' + column) || '';
          var bVal = b.getAttribute('data-' + column) || '';

          // Try date comparison for created/modified
          if (column === 'created' || column === 'modified') {
            aVal = new Date(aVal).getTime() || 0;
            bVal = new Date(bVal).getTime() || 0;
          } else {
            aVal = aVal.toLowerCase();
            bVal = bVal.toLowerCase();
          }

          if (aVal < bVal) return newDir === 'asc' ? -1 : 1;
          if (aVal > bVal) return newDir === 'asc' ? 1 : -1;
          return 0;
        });

        // Re-append sorted rows
        rows.forEach(function(row) {
          tbody.appendChild(row);
        });
      }

      // Select all workflows
      function blockrSelectAllWorkflows(tableId, checked, inputId) {
        var table = document.getElementById(tableId);
        if (!table) return;

        var checkboxes = table.querySelectorAll('.blockr-wf-row-select');
        checkboxes.forEach(function(cb) {
          cb.checked = checked;
        });

        blockrUpdateWorkflowSelection(tableId, inputId);
      }

      // Update selection and notify Shiny
      function blockrUpdateWorkflowSelection(tableId, inputId) {
        var table = document.getElementById(tableId);
        if (!table) return;

        var selected = [];
        table.querySelectorAll('.blockr-wf-row-select:checked').forEach(function(cb) {
          selected.push(cb.value);
        });

        // Update Shiny input
        Shiny.setInputValue(inputId, selected, {priority: 'event'});

        // Show/hide bulk actions
        var bulkActions = document.getElementById('%s');
        if (bulkActions) {
          bulkActions.style.display = selected.length > 0 ? 'flex' : 'none';
        }

        // Update select-all checkbox state
        var allCheckboxes = table.querySelectorAll('.blockr-wf-row-select');
        var selectAll = table.querySelector('.blockr-wf-select-all');
        if (selectAll && allCheckboxes.length > 0) {
          selectAll.checked = selected.length === allCheckboxes.length;
          selectAll.indeterminate = selected.length > 0 && selected.length < allCheckboxes.length;
        }
      }
    ", ns("bulk_actions"))))
  )
}

#' Render workflow table
#'
#' @param workflows Data frame of workflows
#' @param ns Namespace function
#' @param load_input_id Input ID for load action
#' @param select_input_id Input ID for selection changes
#' @keywords internal
render_workflow_table <- function(workflows, ns, load_input_id, select_input_id) {
  if (is.null(workflows) || nrow(workflows) == 0) {
    return(
      tags$div(
        class = "blockr-wf-empty",
        tags$p("No workflows found")
      )
    )
  }

  table_id <- ns("workflow_table")

  tags$table(
    id = table_id,
    class = "blockr-wf-table",
    # Header
    tags$thead(
      tags$tr(
        tags$th(
          class = "blockr-wf-th-checkbox",
          tags$input(
            type = "checkbox",
            class = "blockr-wf-select-all",
            onchange = sprintf(
              "blockrSelectAllWorkflows('%s', this.checked, '%s')",
              table_id, select_input_id
            )
          )
        ),
        tags$th(
          class = "blockr-wf-th sortable",
          `data-sort` = "name",
          onclick = sprintf("blockrSortTable('%s', 'name')", table_id),
          "Title ", tags$span(class = "sort-icon")
        ),
        tags$th(
          class = "blockr-wf-th sortable",
          `data-sort` = "created",
          onclick = sprintf("blockrSortTable('%s', 'created')", table_id),
          "Created ", tags$span(class = "sort-icon")
        ),
        tags$th(
          class = "blockr-wf-th sortable active-sort desc",
          `data-sort` = "modified",
          onclick = sprintf("blockrSortTable('%s', 'modified')", table_id),
          "Modified ", tags$span(class = "sort-icon")
        ),
        tags$th(class = "blockr-wf-th", "Actions")
      )
    ),
    # Body
    tags$tbody(
      lapply(seq_len(nrow(workflows)), function(i) {
        wf <- workflows[i, ]
        created_str <- if (inherits(wf$created, "POSIXct")) {
          format(wf$created, "%b %d, %Y")
        } else {
          as.character(wf$created)
        }
        modified_str <- if (inherits(wf$modified, "POSIXct")) {
          format_time_ago(wf$modified)
        } else {
          as.character(wf$modified)
        }

        tags$tr(
          `data-name` = wf$name,
          `data-created` = as.character(wf$created),
          `data-modified` = as.character(wf$modified),
          tags$td(
            class = "blockr-wf-td-checkbox",
            tags$input(
              type = "checkbox",
              class = "blockr-wf-row-select",
              value = wf$name,
              onchange = sprintf(
                "blockrUpdateWorkflowSelection('%s', '%s')",
                table_id, select_input_id
              )
            )
          ),
          tags$td(class = "blockr-wf-td", tags$span(class = "blockr-wf-title", wf$name)),
          tags$td(class = "blockr-wf-td", created_str),
          tags$td(class = "blockr-wf-td", modified_str),
          tags$td(
            class = "blockr-wf-td blockr-wf-actions",
            tags$button(
              class = "blockr-wf-load-btn",
              onclick = sprintf(
                "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                load_input_id, wf$name
              ),
              "Load"
            ),
            tags$button(
              class = "blockr-wf-action-btn",
              title = "Open in new tab (coming soon)",
              bsicons::bs_icon("box-arrow-up-right")
            ),
            tags$button(
              class = "blockr-wf-action-btn",
              title = "Run workflow (coming soon)",
              bsicons::bs_icon("play")
            )
          )
        )
      })
    )
  )
}

#' Delete confirmation modal
#'
#' @param ns Namespace function
#' @param count Number of workflows to delete
#' @keywords internal
delete_confirm_modal <- function(ns, count) {
  modalDialog(
    title = "Delete workflows?",
    tags$p(
      sprintf(
        "Are you sure you want to delete %d workflow%s? This action cannot be undone.",
        count,
        if (count != 1) "s" else ""
      )
    ),
    footer = tagList(
      modalButton("Cancel"),
      tags$button(
        id = ns("confirm_delete"),
        class = "btn btn-danger",
        onclick = sprintf(
          "Shiny.setInputValue('%s', Date.now(), {priority: 'event'})",
          ns("confirm_delete")
        ),
        "Delete"
      )
    ),
    easyClose = TRUE
  )
}
