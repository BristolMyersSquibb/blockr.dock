# Core's board options that ship a bslib control (a switch, the dark-mode
# toggle) are drawn in the options sidebar with the dock's own checkboxes
# instead, under the same input id, so core reads their values as before and
# its servers, which set them through bslib's `toggle_switch()`, still reach
# them. The rest keep the control their option brings.
dock_option_ui <- function(x, id) {

  input_id <- NS(id, board_option_id(x))
  value <- board_option_value(x)

  if (inherits(x, "dark_mode_option")) {
    return(dark_mode_ui(input_id, value))
  }

  if (inherits(x, "filter_rows_option")) {
    return(dock_checkbox(input_id, "Enable preview search", isTRUE(value)))
  }

  if (inherits(x, "thematic_option")) {
    if (!requireNamespace("thematic", quietly = TRUE)) {
      return(NULL)
    }
    return(dock_checkbox(input_id, "Enable thematic", isTRUE(value)))
  }

  board_option_ui(x, id)
}

# Core's dark_mode option takes "light" or "dark", which a checkbox cannot
# report. So the checkbox has no id and no binding, and dark-mode.js reports
# the word under the option's input id and writes the page's `data-bs-theme`.
# Unset, it follows the system setting and reports what it found, as bslib's
# toggle did.
dark_mode_ui <- function(input_id, value) {

  box <- dock_checkbox(NULL, "Dark mode", identical(value, "dark"))

  query <- htmltools::tagQuery(box)$find("input")
  query$addClass("blockr-dark-mode")
  query$addAttrs(`data-input` = input_id, `data-mode` = coal(value, "auto"))
  query$allTags()
}

dark_mode_dep <- function() {
  htmltools::htmlDependency(
    "blockr-dark-mode",
    pkg_version(),
    src = pkg_file("assets", "js"),
    script = "dark-mode.js"
  )
}
