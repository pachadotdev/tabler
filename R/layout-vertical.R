##' @title Helper function for vertical layout
##' @description Build a page structure that matches the official `layout-fluid-vertical.html` preview.
build_vertical_layout <- function(navbar, sidebar, body, footer, theme = "light", color = NULL) {
  # Recreate the fluid-vertical preview structure: aside sidebar with container-fluid,
  # page-wrapper with page-header (container-xl), page-body (container-xl), and footer.
  aside_tag <- if (!is.null(sidebar) && inherits(sidebar, "shiny.tag") && sidebar$name == "aside") sidebar else shiny::tags$aside(class = "navbar navbar-vertical navbar-expand-lg", `data-bs-theme` = if (!is.null(theme) && theme == "dark") "dark" else NULL, shiny::tags$div(class = "container-fluid"))

  header_tag <- if (!is.null(navbar) && inherits(navbar, "shiny.tag") && navbar$name == "header") navbar else NULL

  shiny::tags$div(
    class = "page",
    aside_tag,
    if (!is.null(header_tag)) header_tag,
    shiny::tags$div(
      class = "page-wrapper",
      shiny::tags$div(class = "page-header d-print-none", `aria-label` = "Page header", shiny::tags$div(class = "container-xl")),
      shiny::tags$div(class = "page-body", shiny::tags$div(class = "container-xl", body)),
      if (!is.null(footer)) footer else shiny::tags$footer(class = "footer footer-transparent d-print-none", shiny::tags$div(class = "container-xl"))
    )
  )
}

# Compatibility wrapper expected by core.R and tests. Delegate to build_vertical_layout.
build_vertical_simple_layout <- function(navbar, sidebar, body, footer, side = "left", layout = "vertical", theme = "light", color = NULL) {
  # side and layout args are accepted for compatibility but currently ignored
  build_vertical_layout(navbar, sidebar, body, footer, theme = theme, color = color)
}
