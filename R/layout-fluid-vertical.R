#' @title Build fluid vertical layout structure
#' @description Vertical fluid layout uses a sidebar (<aside>) and page wrapper
#' @param navbar Navbar or sidebar component
#' @param sidebar Not used
#' @param body Body content
#' @param footer Footer content
#' @keywords internal
layout_fluid_vertical <- function(navbar, sidebar, body, footer, theme = "light", color = NULL, show_theme_button = TRUE) {
  side_nav <- NULL
  if (is.list(navbar) && !inherits(navbar, "shiny.tag")) {
    side_nav <- navbar$side
  } else if (!is.null(navbar) && inherits(navbar, "shiny.tag")) {
    tag_name <- navbar$name
    if (tag_name == "aside") side_nav <- navbar
  }

  # Ensure sidebar has data-bs-theme="dark" attribute
  if (!is.null(side_nav) && inherits(side_nav, "shiny.tag") && side_nav$name == "aside") {
    if (grepl("navbar-vertical", side_nav$attribs$class %||% "")) {
      side_nav$attribs[["data-bs-theme"]] <- "dark"
      if (!grepl("navbar-expand-lg", side_nav$attribs$class %||% "")) {
        side_nav$attribs$class <- paste(side_nav$attribs$class, "navbar-expand-lg")
      }
    }
    # Filter out theme toggle li if show_theme_button is FALSE
    if (!isTRUE(show_theme_button)) {
      side_nav <- filter_theme_li(side_nav)
    }
  }

  tagList(
    script(src = "dist/js/tabler-theme.min.js"),
    div(
      class = "page",
      if (!is.null(side_nav)) side_nav,
      div(
        class = "page-wrapper",
        body,
        if (!is.null(footer)) footer
      )
    )
  )
}
