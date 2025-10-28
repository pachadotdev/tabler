#' @title Build boxed layout structure
#' @description Build the HTML structure for the boxed layout. This layout
#' has a top navbar.
#' @param navbar Navbar component (or list with top/side for combo)
#' @param sidebar Sidebar component (not typically used when passing list)
#' @param body Body content
#' @param footer Footer component
#' @keywords internal
layout_boxed <- function(navbar, sidebar, body, footer, theme = "light", color = NULL, show_theme_button = TRUE) {
  top_nav <- NULL
  side_nav <- NULL

  if (is.list(navbar) && !inherits(navbar, "shiny.tag")) {
    top_nav <- navbar$top
    side_nav <- navbar$side
  } else if (!is.null(navbar) && inherits(navbar, "shiny.tag")) {
    if (navbar$name %in% c("header", "aside", "ul")) top_nav <- navbar
  }

  normalize_header_items <- function(items) {
    # Separate theme button items from regular nav items
    theme_items <- list()
    regular_items <- list()

    for (it in items) {
      # Check if this is a theme toggle item (has mt-auto class or is in last position)
      if (inherits(it, "shiny.tag") && it$name == "li" &&
        !is.null(it$attribs$class) && grepl("mt-auto", it$attribs$class %||% "")) {
        # This is a theme button container, extract its children
        theme_items <- c(theme_items, it$children)
      } else if (inherits(it, "shiny.tag") && it$name == "li" && length(it$children) > 0) {
        a <- it$children[[1]]
        if (inherits(a, "shiny.tag") && !is.null(a$attribs[["data-bs-toggle"]]) && a$attribs[["data-bs-toggle"]] == "dropdown") {
          a$attribs[["aria-expanded"]] <- "true"
          a$attribs[["data-bs-auto-close"]] <- "false"
          it$children[[1]] <- a
        }
        regular_items <- c(regular_items, list(it))
      } else {
        regular_items <- c(regular_items, list(it))
      }
    }

    list(regular = regular_items, theme = theme_items)
  }

  header_tag <- NULL
  if (!is.null(top_nav) && inherits(top_nav, "shiny.tag") && top_nav$name == "aside") {
    container <- NULL
    for (ch in top_nav$children) {
      if (inherits(ch, "shiny.tag") && ch$name == "div" && grepl("container-fluid", ch$attribs$class %||% "")) {
        container <- ch
        break
      }
    }

    brand_tag <- NULL
    nav_items <- list()
    if (!is.null(container)) {
      for (c2 in container$children) {
        if (inherits(c2, "shiny.tag") && c2$name == "div" && grepl("navbar-brand", c2$attribs$class %||% "")) {
          if (length(c2$children) > 0) brand_tag <- c2$children[[1]]
        }
        if (inherits(c2, "shiny.tag") && c2$name == "div" && !is.null(c2$attribs$id) && c2$attribs$id == "sidebar-menu") {
          for (c3 in c2$children) if (inherits(c3, "shiny.tag") && c3$name == "ul") nav_items <- c3$children
        }
      }
    }

    processed_items <- normalize_header_items(nav_items)

    header_tag <- header(
      class = "navbar navbar-expand-md",
      div(
        class = "collapse navbar-collapse",
        id = "navbar-menu",
        div(
          class = "container-xl",
          div(
            class = "row flex-column flex-md-row flex-fill align-items-center",
            div(
              class = "col",
              ul(
                class = "navbar-nav",
                if (!is.null(brand_tag)) div(class = "navbar-brand navbar-brand-autodark", brand_tag),
                processed_items$regular,
                # Add theme buttons at the end with ms-md-auto to push to right
                if (isTRUE(show_theme_button) && length(processed_items$theme) > 0) {
                  div(class = "nav-item ms-md-auto", processed_items$theme)
                }
              )
            )
          )
        )
      )
    )
  } else if (!is.null(top_nav)) {
    header_tag <- top_nav
  }

  page_wrapper <- div(class = "page-wrapper", body, if (!is.null(footer)) footer)

  tagList(
    div(
      class = "page",
      if (!is.null(side_nav)) side_nav,
      header_tag,
      page_wrapper
    )
  )
}
