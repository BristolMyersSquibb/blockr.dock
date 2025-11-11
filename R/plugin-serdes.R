ser_deser_ui <- function(id, board) {

  import_btn <- htmltools::tagQuery(
    fileInput(
      NS(id, "restore"),
      buttonLabel = "Import",
      label = "",
      placeholder = "Select a board file"
    )
  )$find(".btn")$addClass("btn-sm")$reset()$find(".input-group")$addClass(
    "input-group-sm"
  )$allTags()

  div(
    class = "d-flex justify-content-center align-items-center gap-1",
    htmltools::tagAppendAttributes(
      import_btn,
      style = "margin-bottom: 0.5rem"
    ),
    downloadButton(
      NS(id, "serialize"),
      "Export",
      class = "btn-sm",
      icon = icon("file-export"),
    )
  )
}

