#' @title Build vertical transparent layout structure
#' @description Vertical layout with transparent sidebar
#' @keywords internal
layout_vertical_transparent <- function(navbar, sidebar, body, footer, theme = "light", color = NULL, show_theme_button = TRUE) {
  # same as vertical but kept for semantic clarity
  side_nav <- NULL
  if (is.list(navbar) && !inherits(navbar, "shiny.tag")) {
    side_nav <- navbar$side
  } else if (!is.null(navbar) && inherits(navbar, "shiny.tag")) {
    if (navbar$name == "aside") side_nav <- navbar
  }
  # Mark transparent sidebar class when appropriate
  if (!is.null(side_nav) && inherits(side_nav, "shiny.tag") && side_nav$name == "aside") {
    if (grepl("navbar-vertical", side_nav$attribs$class %||% "")) {
      if (!grepl("navbar-transparent", side_nav$attribs$class %||% "")) {
        side_nav$attribs$class <- paste(side_nav$attribs$class, "navbar-transparent")
      }
      if (!grepl("navbar-expand-lg", side_nav$attribs$class %||% "")) {
        side_nav$attribs$class <- paste(side_nav$attribs$class, "navbar-expand-lg")
      }
    }
    # Filter out theme toggle li if show_theme_button is FALSE
    if (!isTRUE(show_theme_button)) {
      side_nav <- filter_theme_li(side_nav)
    }
  }
  shiny::tagList(
    shiny::tags$script(src = "dist/js/tabler-theme.min.js"),
    shiny::tags$div(
      class = "page",
      side_nav,
      shiny::tags$div(
        class = "page-wrapper",
        body,
        if (!is.null(footer)) footer
      )
    )
  )
}
