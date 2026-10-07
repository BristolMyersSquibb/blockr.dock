test_that("export button carries no icon (#72)", {

  ui <- ser_deser_ui("ser", new_board())

  root <- xml2::read_html(as.character(htmltools::tagList(ui)))

  link <- xml2::xml_find_all(
    root,
    paste0("//a[", has_class("shiny-download-link"), "]")
  )

  expect_length(link, 1L)

  # The stylesheet used to hide this button's icon, so anything rendered
  # inside the link is markup nobody sees.
  expect_length(xml2::xml_children(link), 0L)
  expect_identical(trimws(xml2::xml_text(link)), "Export")
})
