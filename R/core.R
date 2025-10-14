#' Create a Tabler Dashboard Page
#'
#' Main function to create a complete dashboard page with Tabler theme
#'
#' @param title App title
#' @param navbar Dashboard navbar (optional)
#' @param body Dashboard body content
#' @param footer Dashboard footer (optional)
#'
#' @export
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(tabler)
#'
#'   ui <- tabler_page(
#'     title = "My Dashboard",
#'     body = tabler_body(
#'       h1("Welcome to Tabler!")
#'     )
#'   )
#'
#'   server <- function(input, output, session) {}
#'
#'   shinyApp(ui, server)
#' }
tabler_page <- function(title = NULL, navbar = NULL, body = NULL, footer = NULL) {
  # Build the HTML structure
  shiny::tagList(
    shiny::tags$head(
      shiny::tags$meta(charset = "utf-8"),
      shiny::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      if (!is.null(title)) shiny::tags$title(title)
    ),
    shiny::tags$body(
      class = "antialiased",
      shiny::tags$div(
        class = "page",
        # Navbar
        if (!is.null(navbar)) navbar,

        # Main content wrapper
        shiny::tags$div(
          class = "page-wrapper",

          # Page body
          shiny::tags$div(
            class = "page-body",
            body
          ),

          # Footer
          if (!is.null(footer)) footer
        )
      )
    )
  ) |>
    add_deps()
}

#' Create a Dashboard Body
#'
#' Container for dashboard content
#'
#' @param ... Content to include in the body
#' @param class Additional CSS classes
#'
#' @export
tabler_body <- function(..., class = NULL) {
  shiny::tags$div(
    class = paste("container-xl", class),
    ...
  )
}

#' Create a Dashboard Navbar
#'
#' Top navigation bar for the dashboard
#'
#' @param title Brand title/text
#' @param brand_image URL or path to brand image
#' @param ... Additional navbar items
#'
#' @export
tabler_navbar <- function(title = NULL, brand_image = NULL, ...) {
  shiny::tags$header(
    class = "navbar navbar-expand-md navbar-light d-print-none",
    shiny::tags$div(
      class = "container-xl",

      # Brand
      shiny::tags$h1(
        class = "navbar-brand navbar-brand-autodark d-none-navbar-horizontal pe-0 pe-md-3",
        if (!is.null(brand_image)) {
          shiny::tags$a(
            href = "#",
            shiny::tags$img(
              src = brand_image,
              alt = title %||% "Dashboard",
              class = "navbar-brand-image"
            )
          )
        } else if (!is.null(title)) {
          shiny::tags$a(href = "#", title)
        }
      ),

      # Navbar items
      shiny::tags$div(
        class = "navbar-nav flex-row order-md-last",
        ...
      )
    )
  )
}

#' Create a Dashboard Footer
#'
#' Footer for the dashboard
#'
#' @param left Left-aligned content
#' @param right Right-aligned content
#'
#' @export
tabler_footer <- function(left = NULL, right = NULL) {
  shiny::tags$footer(
    class = "footer footer-transparent d-print-none",
    shiny::tags$div(
      class = "container-xl",
      shiny::tags$div(
        class = "row text-center align-items-center flex-row-reverse",
        if (!is.null(right)) {
          shiny::tags$div(
            class = "col-lg-auto ms-lg-auto",
            right
          )
        },
        if (!is.null(left)) {
          shiny::tags$div(
            class = "col-12 col-lg-auto mt-3 mt-lg-0",
            left
          )
        }
      )
    )
  )
}

# Helper operator for null coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x
