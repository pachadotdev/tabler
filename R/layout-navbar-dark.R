#' @title Navbar dark layout
#' @description Build a page structure that matches the official `layout-navbar-dark.html` preview.
#' @inheritParams layout-boxed
#' @rdname layout-navbar-dark
build_navbar_dark_layout <- function(navbar, sidebar, body, footer, theme = "light", color = NULL) {
  if (!is.null(navbar) && inherits(navbar, "shiny.tag")) navbar <- shiny::tagAppendAttributes(navbar, `data-bs-theme` = "dark")
  shiny::tags$div(
    class = "page",
    if (!is.null(navbar)) navbar,
    shiny::tags$div(class = "page-wrapper", shiny::tags$div(class = "page-body", body), if (!is.null(footer)) footer)
  )
}
