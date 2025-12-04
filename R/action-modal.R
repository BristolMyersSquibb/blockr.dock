block_modal <- function(ns, board, mode = c("append", "add")) {

  mode <- match.arg(mode)

  board_block_ids <- board_block_ids(board)
  board_link_ids <- board_link_ids(board)

  title <- if (mode == "append") "Append new block" else "Add new block"
  button_label <- if (mode == "append") "Append Block" else "Add Block"

  # Use different IDs for each mode to avoid conflicts
  selection_id <- paste0(mode, "_block_selection")
  name_id <- paste0(mode, "_block_name")
  block_id_field <- paste0(mode, "_block_id")
  confirm_id <- paste0(mode, "_block_confirm")

  # Always visible fields
  visible_fields <- list(
    block_registry_selectize(ns(selection_id))
  )

  # Add block name field (visible)
  visible_fields[[length(visible_fields) + 1]] <- textInput(
    ns(name_id),
    label = "User defined block title (can be changed after creation)",
    placeholder = "Select block first"
  )

  # Advanced options (collapsible)
  advanced_fields <- list()

  # Add block input field only for append mode (in advanced options)
  if (mode == "append") {
    advanced_fields[[length(advanced_fields) + 1]] <- block_input_select(
      inputId = ns("append_block_input"),
      label = "Block input"
    )
  }

  # Add Block ID field
  advanced_fields[[length(advanced_fields) + 1]] <- textInput(
    ns(block_id_field),
    label = "Block ID",
    value = rand_names(board_block_ids)
  )

  # Add link ID field only for append mode (in advanced options)
  if (mode == "append") {
    advanced_fields[[length(advanced_fields) + 1]] <- textInput(
      ns("append_link_id"),
      label = "Link ID",
      value = rand_names(board_link_ids)
    )
  }

  # Collapsible advanced options section
  advanced_section <- div(
    id = ns("block-advanced-options"),
    tagList(advanced_fields)
  )

  modalDialog(
    title = title,
    size = "l",
    easyClose = TRUE,
    footer = NULL,
    tagList(
      css_modal_advanced(ns("block-advanced-options")),
      visible_fields,
      toggle_button(ns("block-advanced-options"), ns("block-advanced-toggle")),
      advanced_section,
      confirm_button(
        inputId = ns(confirm_id),
        label = button_label
      ),
      auto_focus_script(ns(selection_id))
    )
  )
}

link_modal <- function(ns, board, block_id) {

  board_blocks <- board_blocks(board)

  stopifnot(is_string(block_id), block_id %in% names(board_blocks))

  board_blocks <- board_blocks[names(board_blocks) != block_id]

  selection_id <- "create_link"

  avail <- map(
    block_input_select,
    board_blocks,
    names(board_blocks),
    MoreArgs = list(links = board_links(board), mode = "inputs")
  )

  if (sum(lengths(avail)) == 0L) {
    notify(
      "No inputs are currently available.",
      type = "warning"
    )
    return()
  }

  visible_fields <- list(
    board_select(
      id = ns(selection_id),
      blocks = board_blocks[lgl_ply(avail, has_length)],
      label = "Select target block",
      choices = NULL
    )
  )

  toggle <- toggle_button(
    ns("link-advanced-options"),
    ns("link-advanced-toggle")
  )

  advanced_fields <- list(
    block_input_select(
      inputId = ns("add_link_input"),
      label = "Block input"
    ),
    textInput(
      ns("add_link_id"),
      label = "Link ID",
      value = rand_names(board_link_ids(board))
    )
  )

  advanced_section <- div(
    id = ns("link-advanced-options"),
    tagList(advanced_fields)
  )

  modalDialog(
    title = "Create new link",
    size = "m",
    easyClose = TRUE,
    footer = NULL,
    tagList(
      css_modal_advanced(ns("link-advanced-options")),
      visible_fields,
      toggle,
      advanced_section,
      confirm_button(
        inputId = ns("add_link_confirm"),
        label = "Add Link"
      ),
      auto_focus_script(ns(selection_id))
    )
  )
}

stack_modal <- function(ns, board, mode = c("create", "edit"), stack = NULL,
                        stack_id = NULL) {

  mode <- match.arg(mode)

  board_blocks <- board_blocks(board)
  board_stack_ids <- board_stack_ids(board)
  board_stacks <- board_stacks(board)

  avail <- available_stack_blocks(board)

  if (mode == "edit") {
    sel <- stack_blocks(stack)
    avail <- c(avail, sel)
  } else {
    sel <- NULL
  }

  board_blocks <- board_blocks[avail]

  # Mode-specific values
  title <- if (mode == "create") "Create new stack" else "Edit stack"
  button_label <- if (mode == "create") "Create Stack" else "Update Stack"

  selection_id <- if (mode == "create") {
    "stack_block_selection"
  } else {
    "edit_stack_blocks"
  }
  name_id <- if (mode == "create") "stack_name" else "edit_stack_name"
  stack_id_field <- "stack_id"
  confirm_id <- if (mode == "create") "stack_confirm" else "edit_stack_confirm"

  # Always visible fields
  visible_fields <- list(
    board_select(
      id = ns(selection_id),
      blocks = board_blocks,
      label = "Select blocks to add to stack (optional)",
      choices = NULL,
      selected = sel,
      multiple = TRUE
    )
  )

  # Add stack name field (visible)
  visible_fields[[length(visible_fields) + 1]] <- textInput(
    ns(name_id),
    label = if (mode == "create") {
      "Stack name (can be changed after creation)"
    } else {
      "Stack name"
    },
    placeholder = if (mode == "create") "Enter stack name" else NULL,
    value = if (mode == "edit") stack_name(stack) else NULL
  )

  # For edit mode, add color picker to visible fields
  if (mode == "edit") {
    visible_fields[[length(visible_fields) + 1]] <- shinyWidgets::colorPickr(
      inputId = ns("edit_stack_color"),
      label = "Stack color",
      selected = stack_color(stack),
      theme = "nano",
      position = "right-end",
      useAsButton = TRUE
    )
  }

  # Advanced options (only for create mode)
  toggle <- NULL
  advanced_section <- NULL

  if (mode == "create") {
    # Advanced options toggle button
    toggle <- toggle_button(
      ns("stack-advanced-options"),
      ns("stack-advanced-toggle")
    )

    # Advanced options (collapsible)
    advanced_fields <- list()

    # Add color picker field
    advanced_fields[[length(advanced_fields) + 1]] <- shinyWidgets::colorPickr(
      inputId = ns("stack_color"),
      label = "Stack color",
      selected = suggest_new_colors(
        stack_color(board_stacks)
      ),
      theme = "nano",
      position = "right-end",
      useAsButton = TRUE
    )

    # Add Stack ID field
    advanced_fields[[length(advanced_fields) + 1]] <- textInput(
      ns(stack_id_field),
      label = "Stack ID",
      value = rand_names(board_stack_ids)
    )

    # Collapsible advanced options section
    advanced_section <- div(
      id = ns("stack-advanced-options"),
      tagList(advanced_fields)
    )
  }

  modalDialog(
    title = title,
    size = "l",
    easyClose = TRUE,
    footer = NULL,
    tagList(
      css_modal_advanced(ns("stack-advanced-options")),
      visible_fields,
      toggle,
      advanced_section,
      confirm_button(
        inputId = ns(confirm_id),
        label = button_label
      ),
      auto_focus_script(ns(selection_id))
    )
  )
}
