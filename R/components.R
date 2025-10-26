## Diet components: minimal implementations used by tests

tabler_card <- function(..., title = NULL, footer = NULL, status = NULL, class = NULL) {
  status_class <- if (!is.null(status)) paste0("card-status-", status) else NULL
  dots <- list(...)
  # If first unnamed arg is a single string and title not provided, treat as title
  if (is.null(title) && length(dots) > 0 && is.character(dots[[1]]) && length(dots[[1]]) == 1) {
    title <- dots[[1]]
    dots <- dots[-1]
  }

  header <- if (!is.null(title)) shiny::tags$div(class = "card-header d-flex align-items-center", shiny::tags$div(class = "me-auto", if (inherits(title, "shiny.tag")) title else shiny::tags$h3(class = "card-title", title)))
  footer_tag <- if (!is.null(footer)) shiny::tags$div(class = "card-footer", footer)

  shiny::tags$div(class = paste("card", status_class, class), header, shiny::tags$div(class = "card-body", dots), footer_tag)
}

tabler_icon <- function(name, library = "tabler", class = NULL) {
  icon_class <- switch(library,
    "tabler" = "ti ti-",
    "bootstrap" = "bi bi-",
    "feather" = "fe fe-",
    "ti ti-"
  )
  shiny::tags$i(class = paste(paste0(icon_class, name), class))
}

tabler_value_box <- function(value, title, icon = NULL, color = "primary", width = 3) {
  icon_tag <- if (!is.null(icon)) tabler_icon(icon) else NULL
  shiny::tags$div(class = paste0("col-", width), shiny::tags$div(class = "card card-sm", shiny::tags$div(class = "card-body", shiny::tags$div(class = "row align-items-center", shiny::tags$div(class = "col-auto", if (!is.null(icon_tag)) shiny::tags$span(class = paste0("bg-", color, " text-white avatar"), icon_tag)), shiny::tags$div(class = "col", shiny::tags$div(class = "font-weight-medium", value), shiny::tags$div(class = "text-secondary", title))))))
}

tabler_alert <- function(..., type = "info", dismissible = FALSE, title = NULL) {
  dismiss_button <- if (isTRUE(dismissible)) shiny::tags$button(type = "button", class = "btn-close", `data-bs-dismiss` = "alert", `aria-label` = "Close")
  title_tag <- if (!is.null(title)) shiny::tags$h4(class = "alert-title", title)
  shiny::tags$div(class = paste0("alert alert-", type), role = "alert", title_tag, ..., dismiss_button)
}

tabler_button <- function(label, onclick = NULL, color = "primary", size = "md", outline = FALSE, icon = NULL, ...) {
  size_class <- if (identical(size, "md")) NULL else paste0("btn-", size)
  color_class <- paste0("btn-", if (isTRUE(outline)) "outline-" else "", color)
  icon_tag <- if (!is.null(icon)) list(tabler_icon(icon), " ") else NULL
  shiny::tags$button(type = "button", class = paste("btn", color_class, size_class), onclick = onclick, ..., icon_tag, label)
}

tabler_fix_fluid_vertical_tab_spacing <- function() {
  shiny::tags$style(shiny::HTML(paste(
    ".container-xl > .my-3 + .tab-content { margin-top: 0 !important; padding-top: 0 !important; }",
    ".container-xl > .my-3 { margin-top: 0 !important; margin-bottom: 0 !important; height: 0 !important; }",
    ".container-xl > .tab-content .tab-pane { padding-top: 1rem !important; }",
    sep = "\n"
  )))
}

# Minimal modal used in official boxed layout preview
tabler_modal_report <- function(id = "modal-report") {
  shiny::tags$div(
    class = "modal modal-blur fade",
    id = id,
    tabindex = "-1",
    role = "dialog",
    `aria-hidden` = "true",
    shiny::tags$div(class = "modal-dialog modal-lg", role = "document",
      shiny::tags$div(class = "modal-content",
        shiny::tags$div(class = "modal-header"),
        shiny::tags$div(class = "modal-body"),
        shiny::tags$div(class = "modal-body"),
        shiny::tags$div(class = "modal-footer")
      )
    )
  )
}

# Minimal settings UI (floating button + offcanvas form) used in preview
tabler_settings <- function(offcanvas_id = "offcanvasSettings") {
  shiny::tagList(
    shiny::tags$div(class = "settings",
      shiny::tags$a(href = paste0("#", offcanvas_id), class = "btn btn-floating btn-icon btn-primary", `data-bs-toggle` = "offcanvas", `data-bs-target` = paste0("#", offcanvas_id), `aria-controls` = offcanvas_id, `aria-label` = "Theme Settings",
        # lightweight icon placeholder
        shiny::tags$svg(class = "icon icon-1")
      ),
      shiny::tags$form(class = "offcanvas offcanvas-start offcanvas-narrow", tabindex = "-1", id = offcanvas_id,
        shiny::tags$div(class = "offcanvas-header",
          shiny::tags$h2(class = "offcanvas-title", "Theme Settings"),
          shiny::tags$button(type = "button", class = "btn-close", `data-bs-dismiss` = "offcanvas", `aria-label` = "Close")
        ),
        shiny::tags$div(class = "offcanvas-body d-flex flex-column",
          shiny::tags$div(),
          shiny::tags$div(class = "mt-auto space-y")
        )
      )
    )
  )
}

# Condensed-specific modal (matches the official condensed preview minimal content)
tabler_modal_report_condensed <- function(id = "modal-report") {
  shiny::tags$div(
    class = "modal modal-blur fade",
    id = id,
    tabindex = "-1",
    role = "dialog",
    `aria-hidden` = "true",
    shiny::tags$div(class = "modal-dialog modal-lg", role = "document",
      shiny::tags$div(class = "modal-content")
    )
  )
}

# Condensed-specific settings helper: empty offcanvas placeholders
tabler_settings_condensed <- function(offcanvas_id = "offcanvasSettings") {
  shiny::tags$div(class = "settings",
    shiny::tags$a(href = paste0("#", offcanvas_id), class = "btn btn-floating btn-icon btn-primary", `data-bs-toggle` = "offcanvas", `data-bs-target` = paste0("#", offcanvas_id), `aria-controls` = offcanvas_id, `aria-label` = "Theme Settings",
      shiny::tags$svg(class = "icon icon-1")
    ),
    shiny::tags$form(class = "offcanvas offcanvas-start offcanvas-narrow", tabindex = "-1", id = offcanvas_id,
      shiny::tags$div(class = "offcanvas-header"),
      shiny::tags$div(class = "offcanvas-body d-flex flex-column")
    )
  )
}

# Fluid-vertical helpers use the fuller modal/settings used in the fluid previews
tabler_modal_report_fluid <- tabler_modal_report
tabler_settings_fluid <- tabler_settings

# Combo layout uses the same fluid placeholders in official previews
tabler_modal_report_combo <- tabler_modal_report_fluid
tabler_settings_combo <- tabler_settings_fluid
