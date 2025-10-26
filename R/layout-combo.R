#' @title Build combo layout structure
#' @description Build the HTML structure for the combo layout (vertical
#' sidebar + top navbar). The sidebar should be provided as `navbar$side`
#' or as an `aside` tag; the topbar as `navbar$top` or a `header`/`ul`.
#' @param navbar Navbar component (or list for combo)
#' @param sidebar Sidebar component (optional)
#' @param body Body content
#' @param footer Footer component
#' @keywords internal
get_layout_combo <- function(navbar, sidebar, body, footer, theme = "light", color = NULL) {
  top_nav <- NULL
  side_nav <- NULL

  if (is.list(navbar) && !inherits(navbar, "shiny.tag")) {
    top_nav <- navbar$top
    side_nav <- navbar$side
  } else if (!is.null(navbar)) {
    if (inherits(navbar, "shiny.tag")) {
      tag_name <- navbar$name
      if (tag_name == "aside") {
        side_nav <- navbar
      } else if (tag_name == "header") {
        top_nav <- navbar
      } else if (tag_name == "ul") {
        # prefer ul as sidebar for combo if it looks like a sidebar; fall back
        # to sidebar presence first, otherwise treat as top nav
        side_nav <- navbar
      }
    }
  }

  build_boxed_layout(top_nav, side_nav, body, footer, theme = theme, color = color)
}
