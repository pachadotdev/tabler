#' @title Helper function for boxed layout
#' @description Build a page structure that matches the official `layout-boxed.html` preview.
#' @param navbar Navbar component
#' @param sidebar Sidebar component
#' @param body Body content
#' @param footer Footer component
#' @return A Shiny tag representing the page structure
#' @rdname layout-boxed
build_boxed_layout <- function(navbar, sidebar, body, footer, theme = "light", color = NULL) {
  # The official preview uses <body class="layout-boxed"> and a specific
  # page structure. We recreate the minimal shell here while allowing the
  # passed-in navbar/sidebar/body/footer to be injected into the appropriate
  # places. For tests and generated pages we preserve the main wrappers.

  # If the sidebar is a plain <ul>, treat it as centered horizontal nav in header
  boxed_nav_header <- NULL
  sidebar_render <- sidebar
  if (!is.null(sidebar) && inherits(sidebar, "shiny.tag") && sidebar$name == "ul") {
    ul_tag <- sidebar
    existing_class <- ul_tag$attribs$class %||% ""
    existing_class <- gsub("\\bpt-[^ ]+\\b", "", existing_class)
    existing_class <- trimws(existing_class)
    ul_tag$attribs$class <- paste(existing_class, "navbar-nav mx-auto d-flex justify-content-center")

    brand_node <- NULL
    if (!is.null(sidebar$attribs) && !is.null(sidebar$attribs$title)) brand_node <- sidebar$attribs$title
    brand_el <- NULL
    if (!is.null(brand_node)) {
      if (is.list(brand_node) && !is.null(brand_node$img)) {
        brand_el <- shiny::tags$a(
          href = brand_node$href %||% "#", class = "navbar-brand navbar-brand-autodark me-3",
          shiny::tags$img(src = brand_node$img, alt = brand_node$text %||% "", class = "navbar-brand-image"),
          if (!is.null(brand_node$text)) brand_node$text
        )
      } else {
        brand_el <- shiny::tags$a(href = "#", class = "navbar-brand navbar-brand-autodark me-3", brand_node)
      }
    }

    if (!is.null(ul_tag$children) && length(ul_tag$children) > 0) {
      for (j in seq_along(ul_tag$children)) {
        li <- ul_tag$children[[j]]
        if (inherits(li, "shiny.tag") && li$name == "li") {
          li_class <- li$attribs$class %||% ""
          li$attribs$class <- paste(li_class, "nav-item d-flex align-items-center")
          if (!is.null(li$children) && length(li$children) > 0) {
            a <- li$children[[1]]
            if (inherits(a, "shiny.tag") && a$name == "a") {
              a_class <- a$attribs$class %||% ""
              a$attribs$class <- paste(a_class, "nav-link d-flex align-items-center")
              li$children[[1]] <- a
            }
          }
          ul_tag$children[[j]] <- li
        }
      }
    }

    brand_container <- if (!is.null(brand_el)) shiny::tags$div(class = "d-flex align-items-center me-3", brand_el) else NULL

    boxed_nav_header <- shiny::tags$header(
      class = "navbar navbar-expand-md d-print-none",
      shiny::tags$div(class = "container-xl d-flex align-items-center", brand_container, ul_tag)
    )
    sidebar_render <- NULL
  }

  # Build the main page with the body having class layout-boxed
  # The official boxed preview uses an outer .page and two header blocks
  # (an initial navbar and a collapsed navbar wrapper). We reproduce
  # that structure while keeping the injected components.
  shiny::tags$div(
    class = "page",
    # Primary navbar/header (may be provided via 'navbar' or boxed_nav_header)
    if (!is.null(boxed_nav_header)) boxed_nav_header else if (!is.null(navbar) && inherits(navbar, "shiny.tag") && navbar$name == "header") navbar else NULL,

    # A second header used by the official layout as a collapsed container
    shiny::tags$header(class = "navbar-expand-md", shiny::tags$div(class = "collapse navbar-collapse", id = "navbar-menu", list())),

    # If a sidebar was provided (and not converted to header), include it
    if (!is.null(sidebar_render)) sidebar_render,

    # Page wrapper holding header, body and footer
    shiny::tags$div(
      class = "page-wrapper",
      # Page header (kept empty here if none provided) - match official aria-label
      if (!is.null(navbar) && inherits(navbar, "shiny.tag") && navbar$name == "header" && is.null(boxed_nav_header)) {
        # If user passed a header but it wasn't boxed_nav_header, put it as page-header
        shiny::tags$div(class = "page-header d-print-none", aria.label = "Page header", navbar)
      } else {
        shiny::tags$div(class = "page-header d-print-none", aria.label = "Page header")
      },

      # Page body
      shiny::tags$div(class = "page-body", body),

      # Footer
      if (!is.null(footer)) footer
    )
  )
}
