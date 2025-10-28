#' @title  Create a Tabler Card
#' @description Build a modern card component with optional header, body, and footer
#' @param ... Card body content
#' @param title Card title (optional)
#' @param footer Card footer content (optional)
#' @param status Card color status: "primary", "secondary", "success", "warning", "danger", etc.
#' @param class Additional CSS classes
#' @rdname tabler-components
#' @return An HTML tag representing the card
#' @export
card <- function(..., title = NULL, footer = NULL, status = NULL, class = NULL) {
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

  # Card header/body rendering
  header <- NULL
  if (title_was_provided && !is.null(title)) {
    # Render boxed-style card by default: title inside card body as h3.h2,
    # body content below it, and footer rendered as muted paragraph inside
    # the same column. This matches the upstream boxed examples.
    # Prepare body paragraph(s)
    body_par <- NULL
    if (!is.null(body_content)) {
      if (is.character(body_content)) {
        # character content -> normal paragraph
        body_par <- p(body_content)
      } else if (is.list(body_content) && length(body_content) == 1 && inherits(body_content[[1]], "shiny.tag")) {
        # single shiny.tag (e.g., p(...)) -> use as-is
        body_par <- body_content[[1]]
      } else {
        # other cases: include as-is
        body_par <- body_content
      }
    }

    # do not render footer as muted paragraph inside body; render as card-footer later
    body_tag <- div(
      class = "card-body",
      div(
        class = "row gy-3",
        div(
          class = "col-12 col-sm d-flex flex-column",
          h3(class = "h2", title),
          body_par,
          NULL
        )
      )
    )
    # keep rendering a separate card-footer below so footer is shown once
  } else if (inferred_title) {
    # Render boxed-style card body for shorthand usage (title inferred from first arg)
    body_par <- NULL
    if (!is.null(body_content)) {
      if (is.character(body_content)) {
        body_par <- p(class = "text-muted", body_content)
      } else {
        body_par <- body_content
      }
    }

    body_tag <- div(
      class = "card-body",
      div(
        class = "row gy-3",
        div(
          class = "col-12 col-sm d-flex flex-column",
          h3(class = "h2", title),
          body_par
        )
      )
    )
  } else {
    # No title at all; render body normally
    body_tag <- div(class = "card-body", body_content)
  }

  # Card footer
  footer_tag <- if (!is.null(footer)) {
    div(
      class = "card-footer",
      footer
    )
  }

  div(
    class = paste("card", status_class, class),
    header,
    body_tag,
    footer_tag
  )
}

#' @title Create a page pretitle
#' @description Small helper to render the page pretitle element used by Tabler examples
#' @param text The pretitle text to display
#' @rdname tabler-components
#' @export
pre_title <- function(text) {
  div(class = "page-pretitle", text)
}

#' @title Create a page title
#' @description Small helper to render the page title element used by Tabler examples
#' @param text The title text to display
#' @rdname tabler-components
#' @export
title <- function(text) {
  h2(class = "page-title", text)
}

#' @title Create a page header
#' @description Create the full page header structure with pretitle and title
#' @param title_text The main title text
#' @param pretitle_text The pretitle text (optional)
#' @param ... Additional elements to include in the header (e.g., action buttons)
#' @rdname tabler-components
#' @export
page_header <- function(title_text, pretitle_text = NULL, ...) {
  div(
    class = "page-header d-print-none",
    `aria-label` = "Page header",
    div(
      class = "container-xl",
      div(
        class = "row g-2 align-items-center",
        div(
          class = "col",
          if (!is.null(pretitle_text)) div(class = "page-pretitle", pretitle_text),
          h2(class = "page-title", title_text)
        ),
        if (length(list(...)) > 0) {
          div(
            class = "col-auto ms-auto d-print-none",
            ...
          )
        }
      )
    )
  )
}

#' @title Create a Value Box
#' @description Display a key metric or value prominently
#' @param value Main value to display
#' @param title Title/label for the value
#' @param icon Icon name (optional)
#' @param color Color theme
#' @param width Column width (1-12)
#' @rdname tabler-components
#' @return An HTML tag representing the value box
#' @export
value_box <- function(value, title, icon = NULL, color = "primary", width = 3) {
  icon_tag <- if (!is.null(icon)) {
    icon(icon)
  }

  div(
    class = paste0("col-", width),
    div(
      class = "card card-sm",
      div(
        class = "card-body",
        div(
          class = "row align-items-center",
          div(
            class = "col-auto",
            if (!is.null(icon_tag)) {
              span(
                class = paste0("bg-", color, " text-white avatar"),
                icon_tag
              )
            }
          ),
          div(
            class = "col",
            div(
              class = "font-weight-medium",
              value
            ),
            div(
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
#' @rdname tabler-components
#' @return An HTML tag representing the icon
#' @export
icon <- function(name, library = "tabler", class = NULL) {
  icon_class <- switch(library,
    "tabler" = "ti ti-",
    "bootstrap" = "bi bi-",
    "feather" = "fe fe-",
    "ti ti-" # default to tabler
  )

  # Build class string carefully so there are no trailing spaces when `class` is NULL
  parts <- c(paste0(icon_class, name), class)
  parts <- Filter(function(x) nzchar(as.character(x)), parts)
  class_attr <- paste(parts, collapse = " ")

  i(
    class = class_attr
  )
}

#' @title Create an Alert/Notification
#' @description Display important messages to users
#' @param ... Alert content
#' @param type Alert type: "info", "success", "warning", "danger"
#' @param dismissible Whether alert can be dismissed
#' @param title Alert title (optional)
#' @rdname tabler-components
#' @return An HTML tag representing the alert
#' @export
alert <- function(..., type = "info", dismissible = FALSE, title = NULL) {
  dismiss_button <- if (dismissible) {
    button_tag(
      type = "button",
      class = "btn-close",
      `data-bs-dismiss` = "alert",
      `aria-label` = "Close"
    )
  }

  title_tag <- if (!is.null(title)) {
    h4(class = "alert-title", title)
  }

  div(
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
#' @param href URL to link to (creates an anchor tag instead of button)
#' @param onclick JavaScript to execute on click
#' @param color Button color theme
#' @param size Button size: "sm", "md", "lg"
#' @param outline Use outline style
#' @param icon Icon to include
#' @param icon_right Position icon on the right side
#' @param disabled Whether the button is disabled
#' @param block Make button full-width (block-level)
#' @param pill Use pill-shaped button style
#' @param square Use square button style
#' @param loading Show loading spinner
#' @param class Additional CSS classes
#' @param type HTML button type attribute
#' @param ... Additional HTML attributes
#' @rdname tabler-components
#' @return An HTML tag representing the button
#' @export
button <- function(label, href = NULL, onclick = NULL, color = "primary",
                   size = "md", outline = FALSE, icon = NULL,
                   icon_right = FALSE, disabled = FALSE, block = FALSE,
                   pill = FALSE, square = FALSE, loading = FALSE,
                   class = NULL, type = "button", ...) {
  # Build classes
  size_class <- if (!is.null(size) && nzchar(size) && size != "md") paste0("btn-", size) else NULL
  color_class <- paste0("btn-", if (isTRUE(outline)) "outline-" else "", color)
  extra_classes <- c(
    class,
    if (isTRUE(block)) "w-100" else NULL,
    if (isTRUE(pill)) "btn-pill" else NULL,
    if (isTRUE(square)) "btn-square" else NULL,
    if (isTRUE(loading)) "btn-loading" else NULL
  )

  parts <- c("btn", color_class, size_class, extra_classes)
  parts <- Filter(function(x) nzchar(as.character(x)), parts)
  class_attr <- paste(parts, collapse = " ")

  # Icon handling: allow passing an icon name or a full tag
  icon_tag <- NULL
  if (!is.null(icon)) {
    if (inherits(icon, "shiny.tag") || inherits(icon, "html")) {
      # Single tag element
      icon_tag <- list(icon)
    } else if (is.list(icon)) {
      # Already a list
      icon_tag <- icon
    } else {
      # Icon name - create the icon element only (don't render the raw name)
      icon_tag <- list(
        icon(icon)
      )
    }
  }

  # Build children list with icon positioned correctly
  children <- list()
  if (!is.null(icon_tag)) {
    if (isTRUE(icon_right)) {
      children <- c(list(label), list(" "), icon_tag)
    } else {
      children <- c(icon_tag, list(" "), list(label))
    }
  } else {
    children <- list(label)
  }

  # Common attributes
  attrs <- list(class = class_attr, ...)

  if (!is.null(onclick)) attrs$onclick <- onclick
  if (isTRUE(disabled)) attrs$class <- paste(attrs$class, "disabled")

  # Ensure children is a flat list (but preserve tag objects which are lists
  # with class 'shiny.tag'). This avoids nested lists that can duplicate
  # elements when icon_tag was itself a list.
  flatten_children <- function(x) {
    out <- list()
    for (el in x) {
      if (is.list(el) && !inherits(el, "shiny.tag") && !inherits(el, "html")) {
        out <- c(out, flatten_children(el))
      } else {
        out <- c(out, list(el))
      }
    }
    out
  }

  children <- flatten_children(children)

  # Render as <a> if href provided, else <button>
  if (!is.null(href)) {
    attrs$href <- href
    # role=button when using anchor
    attrs$role <- attrs$role %||% "button"
    do.call(htmltools::a, c(attrs, children))
  } else {
    attrs$type <- type
    do.call(button_tag, c(attrs, children))
  }
}
