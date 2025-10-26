#' @title Navbar overlap layout
#' @description Build a page structure that matches the official `layout-navbar-overlap.html` preview.
#' @inheritParams layout-boxed
#' @rdname layout-navbar-overlap
build_navbar_overlap_layout <- function(navbar, sidebar, body, footer, theme = "light", color = NULL) {
  if (!is.null(navbar) && inherits(navbar, "shiny.tag")) navbar <- shiny::tagAppendAttributes(navbar, class = "navbar-overlap", `data-bs-theme` = "dark")
  shiny::tags$div(class = "page", if (!is.null(navbar)) navbar, shiny::tags$div(class = "page-wrapper", shiny::tags$div(class = "page-body", body), if (!is.null(footer)) footer))
}
