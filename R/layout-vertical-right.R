#' @title Build vertical-right layout structure
#' @description Vertical layout with sidebar on the right
#' @keywords internal
layout_vertical_right <- function(navbar, sidebar, body, footer, theme = "light", color = NULL, show_theme_button = TRUE) {
  side_nav <- NULL
  if (is.list(navbar) && !inherits(navbar, "shiny.tag")) {
    side_nav <- navbar$side
  } else if (!is.null(navbar) && inherits(navbar, "shiny.tag")) {
    if (navbar$name == "aside") side_nav <- navbar
  }

  # If the provided side_nav is an <aside> tag, ensure it has the right classes
  # for a right vertical layout and annotate theme if provided.
  if (!is.null(side_nav) && inherits(side_nav, "shiny.tag") && side_nav$name == "aside") {
    # Official example uses a dark-themed sidebar for the right-vertical layout
    # regardless of the overall page theme. Force the aside theme to "dark"
    # to match the canonical HTML used in examples.
    side_nav <- tagAppendAttributes(side_nav, class = "navbar-end navbar-expand-lg", `data-bs-theme` = "dark")

    # Filter out theme toggle li if show_theme_button is FALSE
    if (!isTRUE(show_theme_button)) {
      side_nav <- filter_theme_li(side_nav)
    }
  }

  tagList(
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
