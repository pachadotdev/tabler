#' @title Fluid vertical layout helper
#' @description Build a page structure that matches the official `layout-fluid-vertical.html` preview.
#' @inheritParams layout-boxed
#' @rdname layout-fluid-vertical
build_fluid_vertical_layout <- function(navbar, sidebar, body, footer, theme = "light", color = NULL) {
  # Minimal recreation of the official layout-fluid-vertical preview. This
  # places an aside (sidebar) with a container-fluid and a page-wrapper with
  # page-header (aria-label) and page-body containing a container-xl.
  aside_tag <- if (!is.null(sidebar) && inherits(sidebar, "shiny.tag") && sidebar$name == "aside") sidebar else
    shiny::tags$aside(class = "navbar navbar-vertical navbar-expand-lg", `data-bs-theme` = if (!is.null(theme) && theme == "dark") "dark" else NULL,
      shiny::tags$div(class = "container-fluid",
        # Navbar toggler placeholder
        shiny::tags$button(aria.label = "Toggle navigation"),
        # Navbar brand placeholder
        shiny::tags$div(class = "navbar-brand navbar-brand-autodark"),
        shiny::tags$div(class = "navbar-nav flex-row d-lg-none"),
        shiny::tags$div(class = "collapse navbar-collapse", id = "sidebar-menu")
      )
    )

  header_tag <- if (!is.null(navbar) && inherits(navbar, "shiny.tag") && navbar$name == "header") navbar else
    shiny::tags$header(class = "navbar navbar-expand-md d-none d-lg-flex d-print-none",
      shiny::tags$div(class = "container-xl",
        shiny::tags$button(aria.label = "Toggle navigation"),
        shiny::tags$div(class = "navbar-nav flex-row order-md-last"),
        shiny::tags$div(class = "collapse navbar-collapse", id = "navbar-menu")
      )
    )

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
