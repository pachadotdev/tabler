#' @title Build navbar overlap layout structure
#' @description Overlap navbar variant - similar to condensed but with text-white page header
#' @keywords internal
layout_navbar_overlap <- function(navbar, sidebar, body, footer, theme = "light", color = NULL, show_theme_button = TRUE) {
  top_nav <- NULL

  if (is.list(navbar) && !inherits(navbar, "shiny.tag")) {
    top_nav <- navbar$top
  } else if (!is.null(navbar) && inherits(navbar, "shiny.tag")) {
    top_nav <- navbar
  }

  # Build header from aside navbar structure (similar to condensed layout)
  header_tag <- NULL
  brand_tag <- NULL

  if (!is.null(top_nav) && inherits(top_nav, "shiny.tag") && top_nav$name == "aside") {
    # Extract components from aside tag
    container <- NULL
    for (ch in top_nav$children) {
      if (inherits(ch, "shiny.tag") && ch$name == "div" && grepl("container-fluid", ch$attribs$class %||% "")) {
        container <- ch
        break
      }
    }

    nav_items <- list()
    theme_items <- list()

    if (!is.null(container)) {
      for (c2 in container$children) {
        # Skip non-tag children (like HTML comments)
        if (!inherits(c2, "shiny.tag")) next

        # Extract brand
        if (c2$name == "div" && grepl("navbar-brand", c2$attribs$class %||% "")) {
          if (length(c2$children) > 0) brand_tag <- c2$children[[1]]
        }

        # Extract menu items from the collapsible div
        if (c2$name == "div" &&
          grepl("collapse", c2$attribs$class %||% "") &&
          grepl("navbar-collapse", c2$attribs$class %||% "") &&
          !is.null(c2$attribs$id) &&
          c2$attribs$id == "sidebar-menu") {
          for (c3 in c2$children) {
            if (inherits(c3, "shiny.tag") && c3$name == "ul") {
              # The ul children might be wrapped in a list - unwrap them
              all_items <- list()
              for (child in c3$children) {
                if (is.list(child) && !inherits(child, "shiny.tag")) {
                  # Unwrap the list
                  all_items <- c(all_items, child)
                } else if (inherits(child, "shiny.tag")) {
                  all_items <- c(all_items, list(child))
                }
              }

              # Separate theme items from regular nav items
              for (item in all_items) {
                if (inherits(item, "shiny.tag") && item$name == "li") {
                  if (!is.null(item$attribs$class) && grepl("mt-auto", item$attribs$class %||% "")) {
                    # Theme toggle item - extract the <a> tags directly only if show_theme_button is TRUE
                    if (isTRUE(show_theme_button)) {
                      for (child in item$children) {
                        # Child might be wrapped in a list
                        if (is.list(child) && !inherits(child, "shiny.tag")) {
                          for (a_tag in child) {
                            if (inherits(a_tag, "shiny.tag") && a_tag$name == "a") {
                              theme_items <- c(theme_items, list(a_tag))
                            }
                          }
                        } else if (inherits(child, "shiny.tag") && child$name == "a") {
                          theme_items <- c(theme_items, list(child))
                        }
                      }
                    }
                  } else {
                    # Regular nav item - update dropdown aria-expanded
                    if (length(item$children) > 0) {
                      a <- item$children[[1]]
                      if (inherits(a, "shiny.tag") && !is.null(a$attribs[["data-bs-toggle"]]) && a$attribs[["data-bs-toggle"]] == "dropdown") {
                        a$attribs[["aria-expanded"]] <- "false"
                        a$attribs[["data-bs-auto-close"]] <- "outside"
                        item$children[[1]] <- a
                      }
                    }
                    nav_items <- c(nav_items, list(item))
                  }
                }
              }
            }
          }
        }
      }
    }

    # Build navbar-overlap layout header structure (like condensed but with navbar-overlap class and data-bs-theme="dark")
    header_tag <- shiny::tags$header(
      class = "navbar navbar-expand-md navbar-overlap d-print-none",
      `data-bs-theme` = "dark",
      shiny::tags$div(
        class = "container-xl",
        # Navbar toggler
        shiny::tags$button(
          class = "navbar-toggler",
          type = "button",
          `data-bs-toggle` = "collapse",
          `data-bs-target` = "#navbar-menu",
          `aria-controls` = "navbar-menu",
          `aria-expanded` = "false",
          `aria-label` = "Toggle navigation",
          shiny::tags$span(class = "navbar-toggler-icon")
        ),
        # Brand logo (inside header, not outside)
        if (!is.null(brand_tag)) {
          shiny::tags$div(
            class = "navbar-brand navbar-brand-autodark d-none-navbar-horizontal pe-0 pe-md-3",
            brand_tag
          )
        },
        # Collapsible navbar menu
        shiny::tags$div(
          class = "collapse navbar-collapse",
          id = "navbar-menu",
          shiny::tags$ul(
            class = "navbar-nav",
            nav_items
          ),
          # Theme buttons (outside ul, as sibling)
          if (length(theme_items) > 0) {
            shiny::tags$div(
              class = "nav-item ms-md-auto",
              theme_items
            )
          }
        )
      )
    )
  } else if (!is.null(top_nav)) {
    header_tag <- top_nav
  }

  # Add text-white class to page-header for navbar-overlap layout
  modified_body <- body
  if (is.list(body)) {
    modified_body <- lapply(body, function(item) {
      if (inherits(item, "shiny.tag") &&
        item$name == "div" &&
        !is.null(item$attribs$class) &&
        grepl("page-header", item$attribs$class)) {
        # Add text-white class to page-header
        item$attribs$class <- paste(item$attribs$class, "text-white")
      }
      item
    })
  }

  shiny::tagList(
    shiny::tags$script(src = "dist/js/tabler-theme.min.js"),
    shiny::tags$div(
      class = "page",
      if (!is.null(header_tag)) header_tag,
      shiny::tags$div(
        class = "page-wrapper",
        modified_body,
        if (!is.null(footer)) footer
      )
    )
  )
}
