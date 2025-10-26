## Minimal fluid layout builder: reproduces containers from official layout-fluid.html
## This is a diet implementation used by tests and preview generation.

build_fluid_layout <- function(navbar = NULL, sidebar = NULL, body = NULL, footer = NULL, theme = "light", color = NULL) {
  # Top navbar (expand-md)
  header_top <- shiny::tags$header(class = "navbar navbar-expand-md d-print-none",
    shiny::tags$div(class = "container-xl",
      # navbar toggler
      shiny::tags$button(class = "navbar-toggler", type = "button", `data-bs-toggle` = "collapse", `data-bs-target` = "#navbar-menu", `aria-controls` = "navbar-menu", `aria-expanded` = "false", `aria-label` = "Toggle navigation",
        shiny::tags$span(class = "navbar-toggler-icon")
      ),
      # brand placeholder
      shiny::tags$div(class = "navbar-brand navbar-brand-autodark d-none-navbar-horizontal pe-0 pe-md-3",
        shiny::tags$a(href = "/", `aria-label` = "Tabler")
      ),
      # right-side nav container
      shiny::tags$div(class = "navbar-nav flex-row order-md-last",
        shiny::tags$div(class = "nav-item d-none d-md-flex me-3"),
        shiny::tags$div(class = "d-none d-md-flex"),
        shiny::tags$div(class = "nav-item dropdown")
      )
    )
  )

  # Secondary header containing collapse wrapper #navbar-menu
  header_collapse <- shiny::tags$header(class = "navbar-expand-md",
    shiny::tags$div(class = "collapse navbar-collapse", id = "navbar-menu",
      shiny::tags$div(class = "navbar",
        shiny::tags$div(class = "container-xl")
      )
    )
  )

  # Page wrapper with header, body and footer
  page_wrapper <- shiny::tags$div(class = "page-wrapper",
    # page header
    shiny::tags$div(class = "page-header d-print-none", `aria-label` = "Page header",
      shiny::tags$div(class = "container-xl",
        shiny::tags$div(class = "row g-2 align-items-center")
      )
    ),
    # page body
    shiny::tags$div(class = "page-body",
      shiny::tags$div(class = "container-xl",
        shiny::tags$div(class = "row row-deck row-cards", body)
      )
    ),
    # footer
    shiny::tags$footer(class = "footer footer-transparent d-print-none",
      shiny::tags$div(class = "container-xl",
        shiny::tags$div(class = "row text-center align-items-center flex-row-reverse")
      )
    )
  )

  # Assemble the page root
  shiny::tags$div(class = "page", header_top, header_collapse, page_wrapper)
}
