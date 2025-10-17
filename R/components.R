#' Create a Tabler Card
#'
#' Build a modern card component with optional header, body, and footer
#'
#' @param ... Card body content
#' @param title Card title (optional)
#' @param footer Card footer content (optional)
#' @param status Card color status: "primary", "secondary", "success", "warning", "danger", etc.
#' @param class Additional CSS classes
#'
#' @export
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(tabler)
#'
#'   ui <- tablerPage(
#'     title = "Card Demo",
#'     body = tablerBody(
#'       tablerCard(
#'         title = "Sample Card",
#'         "This is card content",
#'         footer = "Card footer",
#'         status = "primary"
#'       )
#'     )
#'   )
#'
#'   server <- function(input, output, session) {}
#'   shinyApp(ui, server)
#' }
tablerCard <- function(..., title = NULL, footer = NULL, status = NULL, class = NULL) {
  # Build status class
  status_class <- if (!is.null(status)) paste0("card-status-", status) else NULL

  # Card header
  header <- if (!is.null(title)) {
    shiny::tags$div(
      class = "card-header",
      shiny::tags$h3(class = "card-title", title)
    )
  }

  # Card footer
  footer_tag <- if (!is.null(footer)) {
    shiny::tags$div(
      class = "card-footer",
      footer
    )
  }

  shiny::tags$div(
    class = paste("card", status_class, class),
    header,
    shiny::tags$div(
      class = "card-body",
      ...
    ),
    footer_tag
  )
}

#' Create a Value Box
#'
#' Display a key metric or value prominently
#'
#' @param value Main value to display
#' @param title Title/label for the value
#' @param icon Icon name (optional)
#' @param color Color theme
#' @param width Column width (1-12)
#'
#' @export
tablerValueBox <- function(value, title, icon = NULL, color = "primary", width = 3) {
  icon_tag <- if (!is.null(icon)) {
    tablerIcon(icon)
  }

  shiny::tags$div(
    class = paste0("col-", width),
    shiny::tags$div(
      class = "card card-sm",
      shiny::tags$div(
        class = "card-body",
        shiny::tags$div(
          class = "row align-items-center",
          shiny::tags$div(
            class = "col-auto",
            if (!is.null(icon_tag)) {
              shiny::tags$span(
                class = paste0("bg-", color, " text-white avatar"),
                icon_tag
              )
            }
          ),
          shiny::tags$div(
            class = "col",
            shiny::tags$div(
              class = "font-weight-medium",
              value
            ),
            shiny::tags$div(
              class = "text-secondary",
              title
            )
          )
        )
      )
    )
  )
}

#' Create an Icon
#'
#' Display an icon from Tabler Icons or other icon libraries
#'
#' @param name Icon name
#' @param library Icon library: "tabler", "bootstrap", "feather"
#' @param class Additional CSS classes
#'
#' @export
tablerIcon <- function(name, library = "tabler", class = NULL) {
  icon_class <- switch(library,
    "tabler" = "ti ti-",
    "bootstrap" = "bi bi-",
    "feather" = "fe fe-",
    "ti ti-" # default to tabler
  )

  shiny::tags$i(
    class = paste(paste0(icon_class, name), class)
  )
}

#' Create an Alert/Notification
#'
#' Display important messages to users
#'
#' @param ... Alert content
#' @param type Alert type: "info", "success", "warning", "danger"
#' @param dismissible Whether alert can be dismissed
#' @param title Alert title (optional)
#'
#' @export
tablerAlert <- function(..., type = "info", dismissible = FALSE, title = NULL) {
  dismiss_button <- if (dismissible) {
    shiny::tags$button(
      type = "button",
      class = "btn-close",
      `data-bs-dismiss` = "alert",
      `aria-label` = "Close"
    )
  }

  title_tag <- if (!is.null(title)) {
    shiny::tags$h4(class = "alert-title", title)
  }

  shiny::tags$div(
    class = paste("alert alert-", type),
    role = "alert",
    title_tag,
    ...,
    dismiss_button
  )
}

#' Create a Button
#'
#' Create interactive buttons with Tabler styling
#'
#' @param label Button text
#' @param onclick JavaScript to execute on click
#' @param color Button color theme
#' @param size Button size: "sm", "md", "lg"
#' @param outline Use outline style
#' @param icon Icon to include
#' @param ... Additional HTML attributes
#'
#' @export
tablerButton <- function(label, onclick = NULL, color = "primary",
                         size = "md", outline = FALSE, icon = NULL, ...) {
  size_class <- if (size != "md") paste0("btn-", size) else NULL
  color_class <- paste0("btn-", if (outline) "outline-", color)

  icon_tag <- if (!is.null(icon)) {
    list(tablerIcon(icon), " ")
  }

  shiny::tags$button(
    type = "button",
    class = paste("btn", color_class, size_class),
    onclick = onclick,
    ...,
    icon_tag,
    label
  )
}
