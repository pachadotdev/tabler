#' @title Horizontal layout helpers
#' @description Build a page structure that matches the official `layout-horizontal.html` preview.
#' @inheritParams layout-boxed
#' @rdname layout-horizontal
build_horizontal_layout <- function(navbar, topbar, body, footer, theme = "light", color = NULL) {
  shiny::tags$div(class = "page", if (!is.null(navbar)) navbar, if (!is.null(topbar)) topbar, shiny::tags$div(class = "page-wrapper", shiny::tags$div(class = "page-body", body), if (!is.null(footer)) footer))
}
