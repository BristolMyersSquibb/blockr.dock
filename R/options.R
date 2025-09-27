new_blocks_position_option <- function(
  reference_panel = blockr_option("reference_panel", NULL),
  direction = blockr_option("direction", "within"),
  ...
) {
  new_board_option(
    id = "blocks_position",
    default = list(reference_panel = reference_panel, direction = direction),
    ui = function(id) {
      tagList(
        h4("Blocks position"),
        selectInput(
          NS(id, "reference_panel"),
          "Reference panel",
          choices = list()
        ),
        selectInput(
          NS(id, "direction"),
          "Direction",
          choices = c("within", "above", "below", "left", "right"),
          selected = direction
        )
      )
    },
    server = function(..., session) {
      list(
        observeEvent(
          req(length(reference_panel_candidates(session)) > 0),
          {
            layout_panels <- reference_panel_candidates(session)

            updateSelectInput(
              session,
              "reference_panel",
              choices = layout_panels,
              selected = coal(reference_panel, last(layout_panels))
            )
          },
          once = TRUE
        ),
        observeEvent(
          get_board_option_or_null("blocks_position", session),
          {
            opt <- get_board_option_value("blocks_position", session)

            updateSelectInput(
              session,
              "reference_panel",
              choices = reference_panel_candidates(session),
              selected = opt$reference_panel
            )

            updateSelectInput(
              session,
              "direction",
              selected = opt$direction
            )
          }
        )
      )
    },
    update_trigger = c("reference_panel", "direction"),
    ...
  )
}

reference_panel_candidates <- function(session) {
  dockViewR::get_panels_ids("layout", session)
}

#' @export
validate_board_option.blocks_position_option <- function(x) {

  val <- board_option_value(NextMethod())

  if (!is.list(val)) {
    blockr_abort(
      "Expecting `blocks_position_option` to be list.",
      class = "board_options_blocks_position_invalid"
    )
  }

  comps <- c("reference_panel", "direction")

  if (!setequal(names(val), comps)) {
    blockr_abort(
      "Expecting `blocks_position_option` to contain component{?s} {comps}.",
      class = "board_options_blocks_position_invalid"
    )
  }

  ref <- val$reference_panel

  if (!(is.null(ref) || is_string(ref))) {
    blockr_abort(
      "Expecting the `reference_panel` entry of `blocks_position_option` to ",
      "either be `NULL` or string-valued.",
      class = "board_options_blocks_position_invalid"
    )
  }

  opts <- c("within", "above", "below", "left", "right")
  dir <- val$direction

  if (!(is_string(dir) && dir %in% opts)) {
    abort(
      "Expecting the `direction` entry of `blocks_position_option` to be one ",
      "of {opts}.",
      class = "board_options_blocks_position_invalid"
    )
  }

  invisible(x)
}
