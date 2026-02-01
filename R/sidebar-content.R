# =============================================================================
# Sidebar Content Renderers
# =============================================================================
#
# Content renderers for each sidebar type. These generate the card-based UI
# that appears inside sidebars.

#' Render Block Sidebar Content
#'
#' Generates the card list for block selection sidebars (add/append/prepend).
#'
#' @param ns Namespace function
#' @param board The board object
#' @param mode One of "add", "append", "prepend"
#' @param search_term Optional search filter
#'
#' @return Shiny tags
#'
#' @keywords internal
render_block_sidebar_content <- function(ns, board, mode, search_term = NULL) {

  # Use block_metadata which is available via blockr.core import
  blocks <- list_blocks()
  meta <- block_metadata(blocks)
  blocks_df <- data.frame(
    ctor = meta[, "id"],
    name = meta[, "name"],
    description = meta[, "description"],
    category = meta[, "category"],
    package = meta[, "package"],
    icon = meta[, "icon"],
    color = blk_color(meta[, "category"]),
    stringsAsFactors = FALSE
  )

  # Filter by search term
  blocks_df <- filter_by_search(
    blocks_df, search_term, c("name", "description", "category", "package")
  )

  if (nrow(blocks_df) == 0) {
    return(empty_sidebar_content("No blocks found"))
  }

  # Group by category
  grouped <- group_by_category(blocks_df)

  # Build card list
  cards <- lapply(names(grouped), function(cat) {
    cat_df <- grouped[[cat]]

    cat_cards <- lapply(seq_len(nrow(cat_df)), function(i) {
      row <- cat_df[i, ]

      # Get block inputs for append mode
      blk_inputs <- if (mode == "append") {
        blk <- create_block(row$ctor)
        block_inputs(blk)
      } else {
        character()
      }

      # Build accordion content based on mode
      accordion_fields <- tagList(
        # Block name (all modes)
        textInput(
          ns(paste0("block_name_", row$ctor)),
          label = "Name (optional)",
          value = "",
          placeholder = row$name
        ),
        # Block ID (all modes)
        textInput(
          ns(paste0("block_id_", row$ctor)),
          label = "Block ID (optional)",
          value = "",
          placeholder = "Auto-generated"
        ),
        # Input selection (append mode only, if multiple inputs)
        if (mode == "append" && length(blk_inputs) > 1) {
          selectInput(
            ns(paste0("block_input_", row$ctor)),
            label = "Connect to input",
            choices = blk_inputs,
            selected = blk_inputs[1]
          )
        },
        # Link ID (append and prepend modes)
        if (mode %in% c("append", "prepend")) {
          textInput(
            ns(paste0("link_id_", row$ctor)),
            label = "Link ID (optional)",
            value = "",
            placeholder = "Auto-generated"
          )
        }
      )

      button_label <- switch(mode,
        add = "Add Block",
        append = "Append Block",
        prepend = "Prepend Block"
      )

      accordion_content <- tags$div(
        accordion_fields,
        tags$button(
          type = "button",
          class = "btn btn-primary btn-sm blockr-accordion-confirm",
          `data-action` = "card-confirm",
          `data-input-id` = ns(paste0(mode, "_block_card_confirm")),
          `data-ctor` = row$ctor,
          `data-mode` = mode,
          button_label
        )
      )

      render_block_card(
        ns = ns,
        id = row$ctor,
        title = row$name,
        description = row$description,
        icon = row$icon,
        category = row$category,
        color = row$color,
        badge = row$package,
        accordion_content = accordion_content,
        on_click_input = paste0(mode, "_block_card_click"),
        on_click_data = list(ctor = row$ctor, mode = mode)
      )
    })

    tagList(
      render_category_header(cat),
      cat_cards
    )
  })

  tagList(cards)
}


#' Render Link Sidebar Content
#'
#' Generates the card list for link creation sidebars.
#'
#' @param ns Namespace function
#' @param board The board object
#' @param source_block The source block ID (link goes FROM this block)
#' @param search_term Optional search filter
#'
#' @return Shiny tags
#'
#' @keywords internal
render_link_sidebar_content <- function(ns, board, source_block, # nolint: line_length_linter.
                                        search_term = NULL) {

  board_blocks <- board_blocks(board)

  # Exclude source block
  board_blocks <- board_blocks[names(board_blocks) != source_block]

  if (length(board_blocks) == 0) {
    return(empty_sidebar_content("No target blocks available"))
  }

  # Get available inputs for each block
  avail <- map(
    block_input_select,
    board_blocks,
    names(board_blocks),
    MoreArgs = list(links = board_links(board), mode = "inputs")
  )

  # Filter to blocks with available inputs
  has_inputs <- lgl_ply(avail, has_length)
  board_blocks <- board_blocks[has_inputs]
  avail <- avail[has_inputs]


  if (length(board_blocks) == 0) {
    return(empty_sidebar_content("No inputs are currently available"))
  }

  meta <- blks_metadata(board_blocks)

  # Build block list
  blocks_df <- data.frame(
    id = names(board_blocks),
    name = chr_ply(board_blocks, block_name),
    description = paste0("ID: ", names(board_blocks)),
    icon = meta$icon,
    category = meta$category,
    package = meta$package,
    color = meta$color,
    inputs = I(avail),
    stringsAsFactors = FALSE
  )

  # Filter by search term
  blocks_df <- filter_by_search(blocks_df, search_term, c("name", "id", "package"))

  if (nrow(blocks_df) == 0) {
    return(empty_sidebar_content("No matching blocks found"))
  }

  # Build cards with input selection accordion
  cards <- lapply(seq_len(nrow(blocks_df)), function(i) {
    row <- blocks_df[i, ]
    inputs <- row$inputs[[1]]

    # Accordion content: input selection + link ID
    accordion_content <- tags$div(
      # Only show input selector if multiple inputs
      if (length(inputs) > 1) {
        selectInput(
          ns(paste0("link_input_", row$id)),
          label = "Connect to input:",
          choices = inputs,
          selected = inputs[1]
        )
      },
      textInput(
        ns(paste0("link_id_", row$id)),
        label = "Link ID (optional)",
        value = "",
        placeholder = "Auto-generated"
      ),
      tags$button(
        type = "button",
        class = "btn btn-primary btn-sm blockr-accordion-confirm",
        `data-action` = "card-confirm",
        `data-input-id` = ns("link_card_confirm"),
        `data-target` = row$id,
        `data-input` = inputs[1],
        "Create Link"
      )
    )

    render_block_card(
      ns = ns,
      id = row$id,
      title = row$name,
      description = row$description,
      icon = row$icon,
      category = coal(row$category, "default"),
      color = row$color,
      badge = row$package,
      accordion_content = accordion_content,
      # Click on card opens accordion
      on_click_input = "link_card_click",
      on_click_data = list(target = row$id, input = inputs[1])
    )
  })

  tagList(cards)
}


#' Render Stack Sidebar Content
#'
#' Generates content for stack creation/edit sidebars.
#' Shows search and block cards only - options are in the footer.
#'
#' @param ns Namespace function
#' @param board The board object
#' @param mode One of "create", "edit"
#' @param stack For edit mode, the stack object
#' @param stack_id For edit mode, the stack ID
#' @param search_term Optional search filter
#' @param current_selection Current selection from input (preserves selection
#'   on re-render)
#'
#' @return Shiny tags
#'
#' @keywords internal
render_stack_sidebar_content <- function(ns, board, mode, stack = NULL,
                                         stack_id = NULL, search_term = NULL,
                                         current_selection = NULL) {

  board_blks <- board_blocks(board)

  avail <- available_stack_blocks(board)

  # Get initial selection from stack (edit mode)
  if (mode == "edit" && !is.null(stack)) {
    initial_blocks <- stack_blocks(stack)
    avail <- unique(c(avail, initial_blocks))
  } else {
    initial_blocks <- character()
  }

  # Use current selection if provided, otherwise fall back to initial
  selected <- if (!is.null(current_selection) && length(current_selection)) {
    # Handle comma-separated string or vector
    if (length(current_selection) == 1 && grepl(",", current_selection)) {
      strsplit(current_selection, ",")[[1]]
    } else {
      current_selection
    }
  } else {
    initial_blocks
  }
  selected <- selected[nzchar(selected)]

  blocks <- board_blks[avail]

  # Filter by search (always show selected)
  if (!is.null(search_term) && nzchar(search_term) && length(blocks) > 0) {
    query <- tolower(search_term)
    matches <- vapply(seq_along(blocks), function(i) {
      blk_id <- names(blocks)[i]
      blk_id %in% selected ||
        grepl(query, tolower(block_name(blocks[[i]])), fixed = TRUE) ||
        grepl(query, tolower(blk_id), fixed = TRUE)
    }, logical(1))
    blocks <- blocks[matches]
  }


  input_id <- if (mode == "create") {
    "stack_block_selection"
  } else {
    "edit_stack_blocks"
  }

  if (length(blocks) > 0) {
    stack_block_cards(ns, blocks, selected, input_id)
  } else {
    msg <- if (!is.null(search_term) && nzchar(search_term)) {
      "No matching blocks"
    } else {
      "No blocks available"
    }
    empty_sidebar_content(msg)
  }
}


#' Render Stack Sidebar Footer
#'
#' Generates the footer for stack sidebars with collapsible options.
#'
#' @param ns Namespace function
#' @param board The board object
#' @param mode One of "create", "edit"
#' @param stack For edit mode, the stack object
#' @param stack_id For edit mode, the stack ID
#'
#' @return Shiny tags
#'
#' @keywords internal
render_stack_sidebar_footer <- function(ns, board, mode, stack = NULL,
                                        stack_id = NULL) {

  if (mode == "edit" && !is.null(stack)) {
    name_value <- stack_name(stack)
    color_value <- stack_color(stack)
    confirm_id <- "edit_stack_confirm"
    name_id <- "edit_stack_name"
    color_id <- "edit_stack_color"
    button_label <- "Update Stack"
    show_stack_id <- FALSE
  } else {
    name_value <- ""
    color_value <- suggest_new_colors(stack_color(board_stacks(board)))
    confirm_id <- "stack_confirm"
    name_id <- "stack_name"
    color_id <- "stack_color"
    button_label <- "Create Stack"
    show_stack_id <- TRUE
  }

  tagList(
    tags$div(
      class = "blockr-stack-footer-options",
      tags$div(
        class = "blockr-stack-footer-main",
        textInput(ns(name_id), label = NULL, value = name_value,
                  placeholder = "Stack name")
      ),
      tags$button(
        class = "blockr-stack-footer-chevron",
        type = "button",
        `data-action` = "toggle-class",
        `data-target` = ".blockr-stack-footer-options",
        `data-toggle-class` = "is-open",
        bsicons::bs_icon("chevron-down")
      )
    ),
    tags$div(
      class = "blockr-stack-footer-dropdown",
      shinyWidgets::colorPickr(
        inputId = ns(color_id),
        label = "Stack color",
        selected = color_value,
        theme = "nano",
        useAsButton = TRUE
      ),
      if (show_stack_id) {
        textInput(
          ns("stack_id"), label = "Stack ID", placeholder = "Auto-generated"
        )
      }
    ),
    tags$button(
      id = ns(confirm_id),
      class = "btn btn-primary blockr-sidebar-confirm-btn",
      type = "button",
      `data-action` = "confirm",
      `data-input-id` = ns(confirm_id),
      button_label
    )
  )
}


#' Stack Block Cards (multi-select)
#'
#' Renders clickable block cards for stack block selection.
#'
#' @param ns Namespace function
#' @param blocks List of block objects
#' @param selected Character vector of selected block IDs
#' @param input_id Input ID for the selection
#'
#' @return Shiny tags
#'
#' @keywords internal
stack_block_cards <- function(ns, blocks, selected = character(), input_id) {
  if (length(blocks) == 0) {
    return(empty_sidebar_content("No blocks available"))
  }

  meta <- blks_metadata(blocks)

  # Hidden input to store selection
  hidden_input <- tags$input(
    type = "hidden",
    id = ns(input_id),
    class = "blockr-stack-selection-input shiny-input-text",
    value = paste(selected, collapse = ",")
  )

  cards <- lapply(seq_along(blocks), function(i) {
    blk_id <- names(blocks)[i]
    blk_name <- block_name(blocks[[i]])
    is_selected <- blk_id %in% selected

    tags$div(
      class = paste(
        "blockr-block-card blockr-stack-card", if (is_selected) "is-selected"
      ),
      `data-action` = "stack-card-toggle",
      `data-input-id` = ns(input_id),
      `data-block-id` = blk_id,

      # Icon
      tags$div(
        class = "blockr-block-card-icon",
        blk_icon_data_uri(meta$icon[i], meta$color[i], 40, "inline")
      ),

      # Content
      tags$div(
        class = "blockr-block-card-content",
        tags$p(class = "blockr-block-card-title", blk_name),
        tags$p(class = "blockr-block-card-description", paste("ID:", blk_id))
      ),

      # Checkmark
      tags$div(class = "blockr-stack-card-check", bsicons::bs_icon("check-lg"))
    )
  })

  tagList(hidden_input, cards)
}


#' Render Dock Sidebar Content (Add Panel)
#'
#' Generates the card list for adding existing blocks/extensions as dock panels.
#'
#' @param ns Namespace function
#' @param board The board object
#' @param visible_block_ids Block IDs already visible
#' @param visible_ext_ids Extension IDs already visible
#' @param search_term Optional search filter
#'
#' @return Shiny tags
#'
#' @keywords internal
render_dock_sidebar_content <- function(ns, board, visible_block_ids,
                                        visible_ext_ids, search_term = NULL) {

  # Get available blocks (not already visible)
  blk_opts <- setdiff(board_block_ids(board), visible_block_ids)
  ext_opts <- setdiff(dock_ext_ids(board), visible_ext_ids)

  cards <- list()

  # Block cards
  if (length(blk_opts) > 0) {
    blks <- board_blocks(board)[blk_opts]
    meta <- blks_metadata(blks)

    blocks_df <- data.frame(
      id = blk_opts,
      name = chr_ply(blks, block_name),
      description = paste0("ID: ", blk_opts),
      icon = meta$icon,
      category = meta$category,
      package = meta$package,
      color = meta$color,
      type = "block",
      stringsAsFactors = FALSE
    )

    # Filter by search
    blocks_df <- filter_by_search(blocks_df, search_term, c("name", "id", "package"))

    if (nrow(blocks_df) > 0) {
      cards <- c(cards, list(render_category_header("Blocks")))

      block_cards <- lapply(seq_len(nrow(blocks_df)), function(i) {
        row <- blocks_df[i, ]
        render_block_card(
          ns = ns,
          id = paste0("blk-", row$id),
          title = row$name,
          description = row$description,
          icon = row$icon,
          category = coal(row$category, "default"),
          color = row$color,
          badge = row$package,
          on_click_input = "dock_panel_click",
          on_click_data = list(id = paste0("blk-", row$id), type = "block")
        )
      })

      cards <- c(cards, block_cards)
    }
  }

  # Extension cards
  if (length(ext_opts) > 0) {
    all_exts <- as.list(dock_extensions(board))

    ext_data <- lapply(ext_opts, function(ext_id) {
      ext <- all_exts[[ext_id]]
      list(
        id = ext_id,
        name = extension_name(ext),
        description = paste0("ID: ", ext_id),
        package = coal(ctor_pkg(extension_ctor(ext)), "local")
      )
    })

    # Filter by search
    if (!is.null(search_term) && nzchar(search_term)) {
      search_lower <- tolower(search_term)
      ext_data <- Filter(function(x) {
        grepl(search_lower, tolower(x$name), fixed = TRUE) ||
          grepl(search_lower, tolower(x$id), fixed = TRUE) ||
          grepl(search_lower, tolower(x$package), fixed = TRUE)
      }, ext_data)
    }

    if (length(ext_data) > 0) {
      cards <- c(cards, list(render_category_header("Extensions")))

      ext_cards <- lapply(ext_data, function(ext) {
        render_block_card(
          ns = ns,
          id = paste0("ext-", ext$id),
          title = ext$name,
          description = ext$description,
          icon = extension_default_icon(),
          category = "extension",
          color = blk_color("utility"),
          badge = ext$package,
          on_click_input = "dock_panel_click",
          on_click_data = list(id = paste0("ext-", ext$id), type = "extension")
        )
      })

      cards <- c(cards, ext_cards)
    }
  }

  if (length(cards) == 0) {
    return(empty_sidebar_content("All panels are already visible"))
  }

  tagList(cards)
}


# =============================================================================
# Settings Sidebar Content
# =============================================================================

render_settings_sidebar_content <- function(ns, board) {
  # Combine board options with options from blocks (same as toolbar_ui.board)
  options <- combine_board_options(
    board_options(board),
    lapply(board_blocks(board), board_options),
    lapply(available_blocks(), board_options)
  )

  # Group by category
  opts <- split(options, chr_ply(options, attr, "category"))

  tagList(
    lapply(names(opts), function(cat_name) {
      render_settings_category(ns, cat_name, opts[[cat_name]])
    })
  )
}

render_settings_category <- function(ns, category_name, options) {
  # Generate unique ID for accessibility
  cat_id <- paste0("settings_cat_", gsub("[^a-zA-Z0-9]", "_", category_name))

  tags$div(
    class = "blockr-settings-category",
    tags$button(
      type = "button",
      class = "blockr-settings-category-header",
      `data-action` = "toggle-class",
      `data-toggle-class` = "is-open",
      `aria-expanded` = "false",
      `aria-controls` = ns(cat_id),
      tags$span(class = "blockr-settings-category-title", category_name),
      tags$span(
        class = "blockr-settings-category-chevron",
        `aria-hidden` = "true",
        bsicons::bs_icon("chevron-down")
      )
    ),
    tags$div(
      id = ns(cat_id),
      class = "blockr-settings-category-content",
      lapply(options, board_option_ui, id = ns(""))
    )
  )
}


# =============================================================================
# Helper Functions
# =============================================================================


#' Filter Data Frame by Search Term
#'
#' Filters rows where any of the specified fields contain the search term
#' (case-insensitive).
#'
#' @param df Data frame to filter
#' @param search_term Search term (returns df unchanged if NULL or empty)
#' @param fields Character vector of column names to search in
#'
#' @return Filtered data frame
#'
#' @keywords internal
filter_by_search <- function(df, search_term, fields) {
  if (is.null(search_term) || !nzchar(search_term)) {
    return(df)
  }

  search_lower <- tolower(search_term)
  matches <- Reduce(`|`, lapply(fields, function(field) {
    grepl(search_lower, tolower(df[[field]]), fixed = TRUE)
  }))

  df[matches, ]
}


#' Empty Sidebar Content
#'
#' Renders a consistent empty state message for sidebars.
#'
#' @param message The message to display
#'
#' @return Shiny tags
#'
#' @keywords internal
empty_sidebar_content <- function(message) {
  tags$div(
    class = "blockr-sidebar-empty",
    tags$p(message)
  )
}


#' Default Block Icon
#'
#' @return HTML string for default icon
#'
#' @keywords internal
default_block_icon <- function() {
  as.character(bsicons::bs_icon("box"))
}
