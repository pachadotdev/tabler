##' @title Helper function for combo layout
##' @description Build a page structure that matches the official `layout-combo.html` preview.
build_combo_layout <- function(navbar, sidebar, body, footer, theme = "light", color = NULL) {
  # Minimal recreation of the official combo layout structure used in preview
  # Includes an aside (sidebar), a header (navbar), and page-wrapper with
  # page-header, page-body and footer. Keep containers empty where the
  # official preview leaves them blank.

  aside_tag <- if (!is.null(sidebar) && inherits(sidebar, "shiny.tag") && sidebar$name == "aside") sidebar else shiny::tags$aside(class = "navbar navbar-vertical navbar-expand-lg", `data-bs-theme` = if (!is.null(theme) && theme == "dark") "dark" else NULL, shiny::tags$div(class = "container-fluid"))

  header_tag <- if (!is.null(navbar) && inherits(navbar, "shiny.tag") && navbar$name == "header") navbar else shiny::tags$header(class = "navbar navbar-expand-md d-none d-lg-flex d-print-none", shiny::tags$div(class = "container-xl"))

  shiny::tags$div(
    class = "page",
    aside_tag,
    header_tag,
    shiny::tags$div(
      class = "page-wrapper",
      shiny::tags$div(class = "page-header d-print-none", `aria-label` = "Page header", shiny::tags$div(class = "container-xl")),
      shiny::tags$div(class = "page-body", shiny::tags$div(class = "container-xl", body)),
      if (!is.null(footer)) footer else shiny::tags$footer(class = "footer footer-transparent d-print-none", shiny::tags$div(class = "container-xl"))
    )
  )
}
