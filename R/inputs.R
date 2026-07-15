# Input widgets — each returns an HTML tag tree and uses data-tabler-input
# attributes so the browser JS can bind them to the WebSocket session.

# Helper: wrap a control with a Tabler-styled label
.input_wrap <- function(inputId, label, control, hint = NULL) {
  div(
    class = "mb-3",
    if (!is.null(label)) {
      tags$label(class = "form-label", `for` = inputId, label)
    },
    control,
    if (!is.null(hint)) {
      div(class = "form-text text-muted", hint)
    }
  )
}

#' @title Select Input
#' @description A dropdown that lets the user pick one item from a list.
#' @param inputId The input identifier used in \code{server}.
#' @param label   Display label shown above the control.
#' @param choices Named or unnamed character vector of choices.
#' @param selected Initially-selected value (defaults to the first choice).
#' @param ... Additional HTML attributes passed to the \code{<select>} element.
#' @return An HTML tag.
#' @rdname tabler-inputs
#' @export
selectInput <- function(inputId, label, choices, selected = NULL, ...) {
  if (is.null(selected)) selected <- choices[[1L]]
  if (!is.null(names(choices))) {
    option_tags <- lapply(seq_along(choices), function(i) {
      val  <- choices[[i]]
      lbl  <- names(choices)[[i]]
      tags$option(value = val, selected = if (identical(val, selected)) TRUE else NULL, lbl)
    })
  } else {
    option_tags <- lapply(choices, function(ch) {
      tags$option(value = ch, selected = if (identical(ch, selected)) TRUE else NULL, ch)
    })
  }

  control <- do.call(tags$select, c(
    list(
      id                 = inputId,
      class              = "form-select",
      `data-tabler-input` = inputId,
      `data-tabler-type`  = "select"
    ),
    list(...),
    option_tags
  ))

  .input_wrap(inputId, label, control)
}

#' @title Slider Input
#' @description A horizontal range slider.
#' @param inputId The input identifier.
#' @param label   Display label.
#' @param min     Minimum value.
#' @param max     Maximum value.
#' @param value   Initial value.
#' @param step    Step size (default \code{1}).
#' @param ... Additional HTML attributes.
#' @return An HTML tag.
#' @rdname tabler-inputs
#' @export
sliderInput <- function(inputId, label, min, max, value, step = 1, ...) {
  control <- tags$input(
    type               = "range",
    id                 = inputId,
    class              = "form-range",
    min                = min,
    max                = max,
    step               = step,
    value              = value,
    `data-tabler-input` = inputId,
    `data-tabler-type`  = "range",
    ...
  )

  value_display <- span(
    id    = paste0(inputId, "_val"),
    class = "badge bg-azure ms-2",
    if (length(value) > 1L) paste(value, collapse = " - ") else value
  )

  .input_wrap(inputId, tagList(label, value_display), control)
}

#' @title Text Input
#' @description A single-line text field.
#' @param inputId The input identifier.
#' @param label   Display label.
#' @param value   Initial value (default \code{""}).
#' @param placeholder Placeholder text.
#' @param ... Additional HTML attributes.
#' @return An HTML tag.
#' @rdname tabler-inputs
#' @export
textInput <- function(inputId, label, value = "", placeholder = NULL, ...) {
  control <- tags$input(
    type                = "text",
    id                  = inputId,
    class               = "form-control",
    value               = value,
    placeholder         = placeholder,
    `data-tabler-input`  = inputId,
    `data-tabler-type`   = "text",
    ...
  )
  .input_wrap(inputId, label, control)
}

#' @title Numeric Input
#' @description A numeric input field.
#' @param inputId The input identifier.
#' @param label   Display label.
#' @param value   Initial value.
#' @param min     Minimum (optional).
#' @param max     Maximum (optional).
#' @param step    Step size (default \code{1}).
#' @param ... Additional HTML attributes.
#' @return An HTML tag.
#' @rdname tabler-inputs
#' @export
numericInput <- function(inputId, label, value, min = NULL, max = NULL, step = 1, ...) {
  control <- tags$input(
    type                = "number",
    id                  = inputId,
    class               = "form-control",
    value               = value,
    min                 = min,
    max                 = max,
    step                = step,
    `data-tabler-input`  = inputId,
    `data-tabler-type`   = "number",
    ...
  )
  .input_wrap(inputId, label, control)
}

#' @title Checkbox Input
#' @description A boolean on/off checkbox.
#' @param inputId The input identifier.
#' @param label   Label shown beside the checkbox.
#' @param value   Initial checked state (\code{FALSE}).
#' @param ... Additional HTML attributes.
#' @return An HTML tag.
#' @rdname tabler-inputs
#' @export
checkboxInput <- function(inputId, label, value = FALSE, ...) {
  div(
    class = "mb-3",
    div(
      class = "form-check",
      tags$input(
        class               = "form-check-input",
        type                = "checkbox",
        id                  = inputId,
        checked             = isTRUE(value),
        `data-tabler-input`  = inputId,
        `data-tabler-type`   = "checkbox",
        ...
      ),
      tags$label(class = "form-check-label", `for` = inputId, label)
    )
  )
}

#' @title Action Button
#' @description A clickable button.  Its value in \code{input} is an integer that
#'   increments by one on each click (starts at \code{0}).
#' @param inputId The input identifier.
#' @param label   Button label.
#' @param class   Additional CSS classes (default \code{"btn-primary"}).
#' @param icon    Optional icon name to prepend.
#' @param ... Additional HTML attributes.
#' @return An HTML tag.
#' @rdname tabler-inputs
#' @export
actionButton <- function(inputId, label, class = "btn-primary", icon = NULL, ...) {
  icon_tag <- if (!is.null(icon)) {
    tags$i(class = paste0("ti ti-", icon, " me-1"))
  }
  tags$button(
    id                  = inputId,
    class               = paste("btn", class),
    type                = "button",
    `data-tabler-input`  = inputId,
    `data-tabler-type`   = "button",
    `data-click-count`   = "0",
    icon_tag,
    label,
    ...
  )
}

#' @title Checkbox Group Input
#' @description A set of checkboxes for selecting multiple values.
#' @param inputId  The input identifier.
#' @param label    Display label.
#' @param choices  Named or unnamed character vector of choices.
#' @param selected Initially-selected values.
#' @return An HTML tag.
#' @rdname tabler-inputs
#' @export
checkboxGroupInput <- function(inputId, label, choices, selected = NULL) {
  if (is.null(names(choices))) names(choices) <- choices

  boxes <- lapply(seq_along(choices), function(i) {
    val <- choices[[i]]
    lbl <- names(choices)[[i]]
    box_id <- paste0(inputId, "_", i)
    div(
      class = "form-check",
      tags$input(
        class               = "form-check-input",
        type                = "checkbox",
        id                  = box_id,
        value               = val,
        checked             = val %in% selected,
        `data-tabler-input`  = inputId,
        `data-tabler-type`   = "checkbox-group"
      ),
      tags$label(class = "form-check-label", `for` = box_id, lbl)
    )
  })

  div(
    class = "mb-3",
    if (!is.null(label)) tags$label(class = "form-label", label),
    do.call(tagList, boxes)
  )
}

#' @title Radio Buttons
#' @description A group of mutually-exclusive radio buttons.
#' @param inputId  The input identifier.
#' @param label    Display label.
#' @param choices  Named or unnamed character vector of choices.
#' @param selected Initially-selected value (defaults to first choice).
#' @return An HTML tag.
#' @rdname tabler-inputs
#' @export
radioButtons <- function(inputId, label, choices, selected = NULL) {
  if (is.null(names(choices))) names(choices) <- choices
  if (is.null(selected)) selected <- choices[[1L]]

  buttons <- lapply(seq_along(choices), function(i) {
    val    <- choices[[i]]
    lbl    <- names(choices)[[i]]
    btn_id <- paste0(inputId, "_", i)
    div(
      class = "form-check",
      tags$input(
        class               = "form-check-input",
        type                = "radio",
        name                = inputId,
        id                  = btn_id,
        value               = val,
        checked             = identical(val, selected),
        `data-tabler-input`  = inputId,
        `data-tabler-type`   = "radio"
      ),
      tags$label(class = "form-check-label", `for` = btn_id, lbl)
    )
  })

  div(
    class = "mb-3",
    if (!is.null(label)) tags$label(class = "form-label", label),
    do.call(tagList, buttons)
  )
}
