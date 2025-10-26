#' @title Helper function for condensed layout
#' @description Build a page structure that matches the official `layout-condensed.html` preview.
#' @inheritParams layout-boxed
#' @rdname layout-condensed
build_condensed_layout <- function(navbar, sidebar, body, footer, theme = "light", color = NULL) {
  # The official condensed preview uses a primary navbar header and a
  # .page-wrapper with an explicit .page-header and .page-body. We mirror
  # that structure; passed-in navbar is placed as the primary header if present.
  header_tag <- if (!is.null(navbar)) navbar else shiny::tags$header(class = "navbar navbar-expand-md d-print-none", shiny::tags$div(class = "container-xl"))

  shiny::tags$div(
    class = "page",
    header_tag,
    # Condensed layout doesn't render the sidebar when a ul-based sidebar
    # is provided in some previews; include sidebar only when it's not an ul
    if (!is.null(sidebar) && !(inherits(sidebar, "shiny.tag") && sidebar$name == "ul")) sidebar,
    shiny::tags$div(
      class = "page-wrapper",
      shiny::tags$div(class = "page-header d-print-none", `aria-label` = "Page header", shiny::tags$div(class = "container-xl")),
      shiny::tags$div(class = "page-body", body),
      if (!is.null(footer)) footer
    )
  )
}
