#' @title  Create a Tabler Card
#' @description Build a modern card component with optional header, body, and footer
#' @param ... Card body content
#' @param title Card title (optional)
#' @param footer Card footer content (optional)
#' @param status Card color status: "primary", "secondary", "success", "warning", "danger", etc.
#' @param class Additional CSS classes
#' @rdname shiny-components
#' @return A Shiny tag representing the card
#' @export
tabler_card <- function(..., title = NULL, footer = NULL, status = NULL, class = NULL) {
  # Build status class
  status_class <- if (!is.null(status)) paste0("card-status-", status) else NULL

  # Capture positional args to support shorthand: tabler_card("Title", "body text")
  dots <- list(...)
  inferred_title <- FALSE
  title_was_provided <- !missing(title)

  body_content <- NULL
  # If caller didn't supply title by name but passed a first character arg,
  # treat it as the title and optionally a second arg as the body text.
  if (!title_was_provided && is.null(title) && length(dots) >= 1 && is.character(dots[[1]])) {
    inferred_title <- TRUE
    title <- dots[[1]]
    if (length(dots) >= 2) {
      body_content <- dots[[2]]
    }
    # any remaining dots beyond first two are ignored for the shorthand
  } else {
    # Use remaining dots as body content
    body_content <- if (length(dots) > 0) dots else NULL
  }

  # Card header (only when title explicitly provided by named arg)
  header <- NULL
  if (title_was_provided && !is.null(title)) {
    header <- shiny::tags$div(
      class = "card-header",
      shiny::tags$h3(class = "card-title", title)
    )
    # body will be the dots
    body_tag <- shiny::tags$div(class = "card-body", body_content)
  } else if (inferred_title) {
    # Render boxed-style card body with title and muted paragraph as in layout-boxed.html
    # If body_content is a simple character, wrap in <p class='text-muted'>
    body_par <- NULL
    if (!is.null(body_content)) {
      if (is.character(body_content)) {
        body_par <- shiny::tags$p(class = "text-muted", body_content)
      } else {
        body_par <- body_content
      }
    }

    body_tag <- shiny::tags$div(
      class = "card-body",
      shiny::tags$div(
        class = "row gy-3",
        shiny::tags$div(
          class = "col-12 col-sm d-flex flex-column",
          shiny::tags$h3(class = "h2", title),
          body_par
        )
      )
    )
  } else {
    # No title at all; render body normally
    body_tag <- shiny::tags$div(class = "card-body", body_content)
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
    body_tag,
    footer_tag
  )
}

#' @title Create a Value Box
#' @description Display a key metric or value prominently
#' @param value Main value to display
#' @param title Title/label for the value
#' @param icon Icon name (optional)
#' @param color Color theme
#' @param width Column width (1-12)
#' @rdname shiny-components
#' @return A Shiny tag representing the value box
#' @export
tabler_value_box <- function(value, title, icon = NULL, color = "primary", width = 3) {
  icon_tag <- if (!is.null(icon)) {
    tabler_icon(icon)
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

#' @title Create an Icon
#' @description Display an icon from Tabler Icons or other icon libraries
#' @param name Icon name
#' @param library Icon library: "tabler", "bootstrap", "feather"
#' @param class Additional CSS classes
#' @rdname shiny-components
#' @return A Shiny tag representing the icon
#' @export
tabler_icon <- function(name, library = "tabler", class = NULL) {
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

#' @title Create an Alert/Notification
#' @description Display important messages to users
#' @param ... Alert content
#' @param type Alert type: "info", "success", "warning", "danger"
#' @param dismissible Whether alert can be dismissed
#' @param title Alert title (optional)
#' @rdname shiny-components
#' @return A Shiny tag representing the alert
#' @export
tabler_alert <- function(..., type = "info", dismissible = FALSE, title = NULL) {
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

#' @title Create a Button
#' @description Create interactive buttons with Tabler styling
#' @param label Button text
#' @param onclick JavaScript to execute on click
#' @param color Button color theme
#' @param size Button size: "sm", "md", "lg"
#' @param outline Use outline style
#' @param icon Icon to include
#' @param ... Additional HTML attributes
#' @rdname shiny-components
#' @return A Shiny tag representing the button
#' @export
tabler_button <- function(label, onclick = NULL, color = "primary",
                         size = "md", outline = FALSE, icon = NULL, ...) {
  size_class <- if (size != "md") paste0("btn-", size) else NULL
  color_class <- paste0("btn-", if (outline) "outline-", color)

  icon_tag <- if (!is.null(icon)) {
    list(tabler_icon(icon), " ")
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
