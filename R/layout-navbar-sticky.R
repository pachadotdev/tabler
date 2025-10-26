#' @title Navbar sticky layout
#' @description Build a page structure that matches the official `layout-navbar-sticky.html` preview.
#' @inheritParams layout-boxed
#' @rdname layout-navbar-sticky
build_navbar_sticky_layout <- function(navbar, sidebar, body, footer, theme = "light", color = NULL) {
  if (!is.null(navbar) && inherits(navbar, "shiny.tag")) navbar <- shiny::tagAppendAttributes(navbar, class = "sticky-top")
  shiny::tags$div(
    class = "page",
    if (!is.null(navbar)) navbar,
    shiny::tags$div(class = "page-wrapper", shiny::tags$div(class = "page-body", body), if (!is.null(footer)) footer)
  )
}
