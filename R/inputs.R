# Input widgets — each returns an HTML tag tree and uses data-tabler-input
# attributes so the browser JS can bind them to the WebSocket session.

# Helper: wrap a control with a Tabler-styled label
.input_wrap <- function(inputId, label, control, hint = NULL, label_for = inputId) {
  div(
    class = "mb-3",
    if (!is.null(label)) {
      tags$label(class = "form-label", `for` = label_for, label)
    },
    control,
    if (!is.null(hint)) {
      div(class = "form-text text-muted", hint)
    }
  )
}

# Internal: expand a Shiny-style choices vector/list into a flat list of
# list(value=, label=, group=) items. A top-level element that is itself a
# vector of length > 1 represents an <optgroup> (its name is the group
# label); scalar elements are plain top-level options.
.expand_choices <- function(choices) {
  nms <- names(choices)
  out <- list()
  for (i in seq_along(choices)) {
    item <- choices[[i]]
    nm <- if (!is.null(nms)) nms[[i]] else NULL
    if (length(item) > 1L) {
      sub_nms <- names(item)
      for (j in seq_along(item)) {
        val <- as.character(item[[j]])
        lbl <- if (!is.null(sub_nms)) sub_nms[[j]] else val
        out[[length(out) + 1L]] <- list(value = val, label = lbl, group = nm)
      }
    } else {
      val <- as.character(item)
      lbl <- if (!is.null(nm)) nm else val
      out[[length(out) + 1L]] <- list(value = val, label = lbl, group = NULL)
    }
  }
  out
}

#' @title Select Input
#' @description A dropdown that lets the user pick one item from a list.
#' @param inputId The input identifier used in \code{server}.
#' @param label   Display label shown above the control.
#' @param choices Named or unnamed character vector of choices. A top-level
#'   element that is itself a vector of length > 1 is rendered as an
#'   \code{<optgroup>} (its name becomes the group label), matching Shiny's
#'   grouped-choices convention.
#' @param selected Initially-selected value (defaults to the first choice).
#' @param searchable If \code{TRUE} (the default), overlay the dropdown with
#'   a text box that filters choices by substring match anywhere in the
#'   label as the user types (e.g. typing "Korea" narrows the list to
#'   "South Korea", "North Korea", ...), instead of relying on the browser's
#'   native jump-to-prefix \code{<select>} behavior. Set to \code{FALSE} to
#'   render a plain native dropdown.
#' @param ... Additional HTML attributes passed to the \code{<select>} element.
#' @return An HTML tag.
#' @rdname tabler-inputs
#' @export
selectInput <- function(inputId, label, choices, selected = NULL, searchable = TRUE, ...) {
  flat <- .expand_choices(choices)
  if (is.null(selected) && length(flat)) selected <- flat[[1L]]$value
  selected <- if (!is.null(selected)) as.character(selected) else NULL

  build_option <- function(it) {
    tags$option(value = it$value, selected = if (identical(it$value, selected)) TRUE else NULL, it$label)
  }

  option_tags <- list()
  i <- 1L
  n <- length(flat)
  while (i <= n) {
    grp <- flat[[i]]$group
    if (!is.null(grp)) {
      j <- i
      members <- list()
      while (j <= n && identical(flat[[j]]$group, grp)) {
        members[[length(members) + 1L]] <- flat[[j]]
        j <- j + 1L
      }
      option_tags[[length(option_tags) + 1L]] <- do.call(
        tags$optgroup,
        c(list(label = grp), lapply(members, build_option))
      )
      i <- j
    } else {
      option_tags[[length(option_tags) + 1L]] <- build_option(flat[[i]])
      i <- i + 1L
    }
  }

  select_class <- if (searchable) "form-select d-none" else "form-select"
  control <- do.call(tags$select, c(
    list(
      id = inputId,
      class = select_class,
      `data-tabler-input` = inputId,
      `data-tabler-type` = "select"
    ),
    list(...),
    option_tags
  ))

  if (!searchable) {
    return(.input_wrap(inputId, label, control))
  }

  selected_label <- NULL
  for (it in flat) {
    if (identical(it$value, selected)) selected_label <- it$label
  }
  if (is.null(selected_label) && length(flat)) selected_label <- flat[[1L]]$label

  search_id <- paste0(inputId, "_search")
  control <- div(
    class = "tabler-select-search",
    tags$input(
      type                        = "text",
      class                       = "form-control",
      id                          = search_id,
      role                        = "combobox",
      autocomplete                = "off",
      spellcheck                  = "false",
      `aria-expanded`             = "false",
      `data-tabler-select-search` = inputId,
      value                       = selected_label
    ),
    tags$ul(
      class = "dropdown-menu tabler-select-search-menu",
      id    = paste0(inputId, "_search_menu")
    ),
    control
  )

  .input_wrap(inputId, label, control, label_for = search_id)
}

#' @title Select Multiple Input
#' @description A searchable multi-select dropdown: type to filter the
#'   choices, click (or press Enter on) one to add it as a removable tag,
#'   click a tag's "x" (or press Backspace in the empty search box) to
#'   remove it. The server receives the current selection as a character
#'   vector.
#' @param inputId The input identifier used in \code{server}.
#' @param label   Display label shown above the control.
#' @param choices Named or unnamed character vector of choices. A top-level
#'   element that is itself a vector of length > 1 is rendered as an
#'   \code{<optgroup>} (its name becomes the group label), matching Shiny's
#'   grouped-choices convention (see \code{\link{selectInput}}).
#' @param selected Initially-selected values (character vector).
#' @param ... Additional HTML attributes passed to the underlying
#'   \code{<select>} element.
#' @return An HTML tag.
#' @rdname tabler-inputs
#' @export
selectMultipleInput <- function(inputId, label, choices, selected = NULL, ...) {
  flat <- .expand_choices(choices)
  selected <- if (!is.null(selected)) as.character(selected) else character(0)

  build_option <- function(it) {
    tags$option(value = it$value, selected = if (it$value %in% selected) TRUE else NULL, it$label)
  }

  option_tags <- list()
  i <- 1L
  n <- length(flat)
  while (i <= n) {
    grp <- flat[[i]]$group
    if (!is.null(grp)) {
      j <- i
      members <- list()
      while (j <= n && identical(flat[[j]]$group, grp)) {
        members[[length(members) + 1L]] <- flat[[j]]
        j <- j + 1L
      }
      option_tags[[length(option_tags) + 1L]] <- do.call(
        tags$optgroup,
        c(list(label = grp), lapply(members, build_option))
      )
      i <- j
    } else {
      option_tags[[length(option_tags) + 1L]] <- build_option(flat[[i]])
      i <- i + 1L
    }
  }

  select_el <- do.call(tags$select, c(
    list(
      id = inputId,
      class = "form-select d-none",
      multiple = "multiple",
      `data-tabler-input` = inputId,
      `data-tabler-type` = "select-multiple"
    ),
    list(...),
    option_tags
  ))

  search_id <- paste0(inputId, "_search")
  control <- div(
    class = "tabler-multi-select",
    div(
      class = "tabler-multi-select-control form-control h-auto",
      tags$div(class = "tabler-multi-select-tags", id = paste0(inputId, "_tags")),
      tags$input(
        type                              = "text",
        class                             = "tabler-multi-select-input",
        id                                = search_id,
        role                              = "combobox",
        autocomplete                      = "off",
        spellcheck                        = "false",
        `aria-expanded`                   = "false",
        `data-tabler-multi-select-search` = inputId
      )
    ),
    tags$ul(
      class = "dropdown-menu tabler-select-search-menu tabler-multi-select-menu",
      id    = paste0(inputId, "_search_menu")
    ),
    select_el
  )

  .input_wrap(inputId, label, control, label_for = search_id)
}

#' @title Slider Input
#' @description A horizontal range slider. When \code{value} has length 2,
#'   two thumbs are rendered (a "from"/"to" range slider) and the server
#'   receives a length-2 numeric vector, e.g. \code{c(2018, 2022)}.
#' @param inputId The input identifier.
#' @param label   Display label.
#' @param min     Minimum value.
#' @param max     Maximum value.
#' @param value   Initial value. A length-2 vector renders a dual-thumb range
#'   slider.
#' @param step    Step size (default \code{1}).
#' @param thumbSize Optional numeric multiplier of the default handle size
#'   (\code{1rem}), e.g. \code{1} for the normal size, \code{2} for double
#'   size, \code{2.5} for 250\%. Defaults to \code{NULL} (Tabler's built-in
#'   size).
#' @param fill    If \code{TRUE}, paint the track from the minimum up to the
#'   current value (single slider) or between the two handles (range
#'   slider), instead of leaving the whole track a flat color.
#' @param color   Optional accent color for the handle(s) and, when
#'   \code{fill = TRUE}, the filled track. Either one of Tabler's named
#'   theme colors (\code{"blue"}, \code{"azure"}, \code{"indigo"},
#'   \code{"purple"}, \code{"pink"}, \code{"red"}, \code{"orange"},
#'   \code{"yellow"}, \code{"lime"}, \code{"green"}, \code{"teal"},
#'   \code{"cyan"}) or any other valid CSS color. Defaults to \code{NULL},
#'   which uses the app's current primary/theme color (see \code{page()}).
#' @param ... Additional HTML attributes.
#' @return An HTML tag.
#' @rdname tabler-inputs
#' @export
sliderInput <- function(inputId, label, min, max, value, step = 1,
                         thumbSize = NULL, fill = FALSE, color = NULL, ...) {
  is_range <- length(value) > 1L

  tabler_colors <- c(
    "blue", "azure", "indigo", "purple", "pink", "red",
    "orange", "yellow", "lime", "green", "teal", "cyan"
  )

  style_parts <- character(0)
  if (!is.null(thumbSize)) style_parts <- c(style_parts, paste0("--tabler-range-thumb-size:", thumbSize, "rem"))
  if (!is.null(color)) {
    accent <- if (color %in% tabler_colors) paste0("var(--tblr-", color, ")") else color
    style_parts <- c(style_parts, paste0("--tblr-primary:", accent))
  }

  pct <- function(v) if (max == min) 0 else 100 * (v - min) / (max - min)

  if (is_range) {
    lo_value <- base::min(value)
    hi_value <- base::max(value)

    wrapper_style_parts <- style_parts
    if (fill) {
      wrapper_style_parts <- c(
        wrapper_style_parts,
        paste0("--tabler-range2-fill-lo:", pct(lo_value), "%"),
        paste0("--tabler-range2-fill-hi:", pct(hi_value), "%")
      )
    }
    wrapper_style <- if (length(wrapper_style_parts)) paste(wrapper_style_parts, collapse = ";") else NULL

    control <- div(
      class = "tabler-range2",
      id = inputId,
      style = wrapper_style,
      `data-tabler-input` = inputId,
      `data-tabler-type` = "range2",
      `data-tabler-range-fill` = if (fill) "true" else NULL,
      tags$input(
        type = "range", class = "form-range",
        min = min, max = max, step = step, value = lo_value,
        `data-tabler-range-role` = "lo"
      ),
      tags$input(
        type = "range", class = "form-range",
        min = min, max = max, step = step, value = hi_value,
        `data-tabler-range-role` = "hi"
      )
    )
  } else {
    input_style_parts <- style_parts
    if (fill) input_style_parts <- c(input_style_parts, paste0("--tabler-range-fill:", pct(value), "%"))
    input_style <- if (length(input_style_parts)) paste(input_style_parts, collapse = ";") else NULL

    control <- tags$input(
      type = "range",
      id = inputId,
      class = "form-range",
      min = min,
      max = max,
      step = step,
      value = value,
      style = input_style,
      `data-tabler-input` = inputId,
      `data-tabler-type` = "range",
      `data-tabler-range-fill` = if (fill) "true" else NULL,
      ...
    )
  }

  value_display <- span(
    id = paste0(inputId, "_val"),
    class = "badge bg-primary text-white ms-2",
    if (is_range) paste(value, collapse = " - ") else value
  )

  .input_wrap(inputId, tagList(label, value_display), control)
}

# Internal: send an "update this input" message to the browser, respecting
# the current module namespace (see moduleServer()/session$ns()).
.updateInputMessage <- function(session, inputId, params) {
  if (!is.list(session) || !is.function(session[["sendCustomMessage"]])) {
    warning("update*Input() requires a tablerApp session object - ignoring", call. = FALSE)
    return(invisible(NULL))
  }
  id <- inputId
  if (is.function(session[["ns"]])) id <- session$ns(id)
  params$id <- id
  session$sendCustomMessage("tabler-updateInput", params)
  invisible(NULL)
}

# Internal: normalize choices (named/unnamed vector or list, optionally
# grouped) into a flat list of list(value=, label=, group=) pairs for the
# "tabler-updateInput" browser message, reusing selectInput()'s own choice
# expansion so both stay in sync.
.normalize_choices_for_update <- function(choices) {
  lapply(.expand_choices(choices), function(it) {
    if (is.null(it$group)) {
      list(value = it$value, label = it$label)
    } else {
      list(value = it$value, label = it$label, group = it$group)
    }
  })
}

#' @title Update a Select Input
#' @description Change the choices and/or selected value of a
#'   \code{\link{selectInput}} already displayed in the browser, without a
#'   full page reload, similar to \code{shiny::updateSelectInput()}.
#' @param session  The session object.
#' @param inputId  The id of the input to update.
#' @param label    Ignored (kept for signature compatibility with Shiny).
#' @param choices  New choices (named or unnamed character vector/list). If
#'   \code{NULL} (default), the existing choices are left unchanged.
#' @param selected New selected value.
#' @param ... Ignored (kept for signature compatibility with Shiny, e.g. the
#'   \code{server} argument of \code{updateSelectizeInput()}).
#' @return Invisibly, \code{NULL}.
#' @rdname tabler-inputs
#' @export
updateSelectInput <- function(session, inputId, label = NULL, choices = NULL, selected = NULL, ...) {
  params <- list()
  if (!is.null(choices)) params$choices <- .normalize_choices_for_update(choices)
  if (!is.null(selected)) params$selected <- selected
  .updateInputMessage(session, inputId, params)
}

#' @rdname tabler-inputs
#' @export
updateSelectizeInput <- updateSelectInput

#' @title Update a Slider Input
#' @description Change the value/min/max/step of a \code{\link{sliderInput}}
#'   already displayed in the browser, similar to
#'   \code{shiny::updateSliderInput()}.
#' @param session The session object.
#' @param inputId The id of the input to update.
#' @param label   Ignored (kept for signature compatibility with Shiny).
#' @param value   New value (scalar, or length-2 vector for a range slider).
#' @param min     New minimum.
#' @param max     New maximum.
#' @param step    New step size.
#' @return Invisibly, \code{NULL}.
#' @rdname tabler-inputs
#' @export
updateSliderInput <- function(session, inputId, label = NULL, value = NULL, min = NULL, max = NULL, step = NULL) {
  params <- list()
  if (!is.null(value)) params$value <- value
  if (!is.null(min)) params$min <- min
  if (!is.null(max)) params$max <- max
  if (!is.null(step)) params$step <- step
  .updateInputMessage(session, inputId, params)
}

#' @title Text Input
#' @description A single-line text field, optionally with an input mask
#'   (e.g. for dates or phone numbers).
#' @param inputId The input identifier.
#' @param label   Display label.
#' @param value   Initial value (default \code{""}).
#' @param placeholder Placeholder text (defaults to \code{mask}, if given).
#' @param mask    Optional input mask made of \code{"0"} digit placeholders
#'   and literal separator characters, e.g. \code{"00/00/0000"} for a date
#'   or \code{"(00) 0000-0000"} for a phone number. As the user types
#'   digits, the literal characters are inserted automatically.
#' @param ... Additional HTML attributes.
#' @return An HTML tag.
#' @rdname tabler-inputs
#' @export
textInput <- function(inputId, label, value = "", placeholder = NULL, mask = NULL, ...) {
  if (!is.null(mask) && is.null(placeholder)) placeholder <- mask

  control <- tags$input(
    type = "text",
    id = inputId,
    class = "form-control",
    value = value,
    placeholder = placeholder,
    autocomplete = if (!is.null(mask)) "off" else NULL,
    `data-mask` = mask,
    `data-mask-visible` = if (!is.null(mask)) "true" else NULL,
    `data-tabler-input` = inputId,
    `data-tabler-type` = "text",
    ...
  )
  .input_wrap(inputId, label, control)
}

#' @title Numeric Input
#' @description A numeric input field with up/down stepper buttons (the
#'   browser's native number spinner is hidden by Tabler's form styling, so
#'   these buttons are rendered explicitly and adjust the value by
#'   \code{step}, clamped to \code{min}/\code{max}).
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
  input_tag <- tags$input(
    type = "number",
    id = inputId,
    class = "form-control text-center",
    value = value,
    min = min,
    max = max,
    step = step,
    `data-tabler-input` = inputId,
    `data-tabler-type` = "number",
    ...
  )

  step_button <- function(dir, icon, label_text) {
    tags$button(
      type = "button",
      class = "btn btn-icon",
      tabindex = "-1",
      `aria-label` = label_text,
      `data-tabler-number-step` = dir,
      `data-tabler-number-target` = inputId,
      tags$i(class = paste0("ti ti-", icon))
    )
  }

  control <- div(
    class = "input-group",
    step_button("down", "minus", "Decrease value"),
    input_tag,
    step_button("up", "plus", "Increase value")
  )

  .input_wrap(inputId, label, control)
}

#' @title Date Input
#' @description A date picker built on the browser's native date field: it
#'   shows a calendar picker (click the icon) and also accepts a typed date
#'   in \code{yyyy-mm-dd} order. Alternatively, \code{inline = TRUE} renders
#'   an always-visible month calendar instead of a text field.
#' @param inputId The input identifier.
#' @param label   Display label.
#' @param value   Initial value: a \code{Date} or a \code{"yyyy-mm-dd"}
#'   string (defaults to today).
#' @param min     Minimum selectable date (\code{Date} or \code{"yyyy-mm-dd"}).
#' @param max     Maximum selectable date (\code{Date} or \code{"yyyy-mm-dd"}).
#' @param icon    Where to place a calendar icon next to the field:
#'   \code{"none"} (default), \code{"left"} or \code{"right"}. Ignored when
#'   \code{inline = TRUE}.
#' @param inline  If \code{TRUE}, render an always-visible month calendar
#'   (with previous/next-month navigation) instead of a text field with a
#'   popup picker.
#' @param ... Additional HTML attributes.
#' @return An HTML tag.
#' @note The value received in \code{input[[inputId]]} on the server is a
#'   \code{"yyyy-mm-dd"} character string; wrap it in \code{as.Date()} if you
#'   need a \code{Date} object.
#' @rdname tabler-inputs
#' @export
dateInput <- function(inputId, label, value = Sys.Date(), min = NULL, max = NULL,
                       icon = c("none", "left", "right"), inline = FALSE, ...) {
  fmt <- function(x) if (is.null(x)) NULL else format(as.Date(x), "%Y-%m-%d")

  if (inline) {
    control <- do.call(div, c(
      list(
        id = inputId,
        class = "tabler-datepicker-inline",
        `data-tabler-input` = inputId,
        `data-tabler-type` = "date-inline",
        `data-value` = fmt(value),
        `data-min` = fmt(min),
        `data-max` = fmt(max)
      ),
      list(...)
    ))
    return(.input_wrap(inputId, label, control))
  }

  icon <- match.arg(icon)

  input_tag <- tags$input(
    type = "date",
    id = inputId,
    class = "form-control",
    value = fmt(value),
    min = fmt(min),
    max = fmt(max),
    `data-tabler-input` = inputId,
    `data-tabler-type` = "date",
    ...
  )

  if (icon == "none") {
    control <- input_tag
  } else {
    icon_addon <- span(class = "input-icon-addon", tags$i(class = "ti ti-calendar"))
    control <- if (icon == "left") {
      div(class = "input-icon", icon_addon, input_tag)
    } else {
      div(class = "input-icon", input_tag, icon_addon)
    }
  }

  .input_wrap(inputId, label, control)
}

#' @title Checkbox Input
#' @description A boolean on/off checkbox.
#' @param inputId The input identifier.
#' @param label   Label shown beside the checkbox.
#' @param value   Initial checked state (\code{FALSE}).
#' @param description Optional secondary text shown below the label.
#' @param inline  If \code{TRUE}, render without the default block-level
#'   margin wrapper (using \code{form-check-inline}) so several calls can be
#'   placed side-by-side inside your own container \code{div()}.
#' @param ... Additional HTML attributes.
#' @return An HTML tag.
#' @rdname tabler-inputs
#' @export
checkboxInput <- function(inputId, label, value = FALSE, description = NULL, inline = FALSE, ...) {
  check_class <- if (inline) "form-check form-check-inline" else "form-check"

  control <- tags$label(
    class = check_class,
    tags$input(
      class = "form-check-input",
      type = "checkbox",
      id = inputId,
      checked = isTRUE(value),
      `data-tabler-input` = inputId,
      `data-tabler-type` = "checkbox",
      ...
    ),
    span(class = "form-check-label", label),
    if (!is.null(description)) span(class = "form-check-description", description)
  )

  if (inline) control else div(class = "mb-3", control)
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
    id = inputId,
    class = paste("btn", class),
    type = "button",
    `data-tabler-input` = inputId,
    `data-tabler-type` = "button",
    `data-click-count` = "0",
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
        class = "form-check-input",
        type = "checkbox",
        id = box_id,
        value = val,
        checked = val %in% selected,
        `data-tabler-input` = inputId,
        `data-tabler-type` = "checkbox-group"
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
#' @param inline   If \code{TRUE}, lay the radio buttons out left-to-right
#'   instead of the default top-to-bottom stack.
#' @return An HTML tag.
#' @rdname tabler-inputs
#' @export
radioButtons <- function(inputId, label, choices, selected = NULL, inline = FALSE) {
  if (is.null(names(choices))) names(choices) <- choices
  if (is.null(selected)) selected <- choices[[1L]]

  check_class <- if (inline) "form-check form-check-inline" else "form-check"

  buttons <- lapply(seq_along(choices), function(i) {
    val <- choices[[i]]
    lbl <- names(choices)[[i]]
    btn_id <- paste0(inputId, "_", i)
    tags$label(
      class = check_class,
      tags$input(
        class = "form-check-input",
        type = "radio",
        name = inputId,
        id = btn_id,
        value = val,
        checked = identical(val, selected),
        `data-tabler-input` = inputId,
        `data-tabler-type` = "radio"
      ),
      span(class = "form-check-label", lbl)
    )
  })

  div(
    class = "mb-3",
    if (!is.null(label)) tags$label(class = "form-label", label),
    div(do.call(tagList, buttons))
  )
}
