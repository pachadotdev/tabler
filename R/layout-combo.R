#' @title Build combo layout structure
#' @description Build the HTML structure for the combo layout. This layout
#' has BOTH a sidebar (vertical navbar with dark theme) AND a top navbar.
#' @param navbar Navbar component (or list with top/side for combo)
#' @param sidebar Sidebar component (not typically used when passing list)
#' @param body Body content
#' @param footer Footer component
#' @keywords internal
layout_combo <- function(navbar, sidebar, body, footer, theme = "light", color = NULL, show_theme_button = TRUE) {
  top_nav <- NULL
  side_nav <- NULL

  # Support both list(navbar = list(top=..., side=...)) and direct tags
  if (is.list(navbar) && !inherits(navbar, "tabler.tag")) {
    top_nav <- navbar$top
    side_nav <- navbar$side
  } else if (!is.null(navbar)) {
    if (inherits(navbar, "tabler.tag")) {
      tag_name <- navbar$name
      if (tag_name == "header") {
        top_nav <- navbar
      } else if (tag_name == "aside") {
        side_nav <- navbar
      }
    }
  }

  # Ensure sidebar has data-bs-theme="dark" attribute
  if (!is.null(side_nav) && inherits(side_nav, "tabler.tag") && side_nav$name == "aside") {
    # Check if it has the navbar-vertical class
    if (grepl("navbar-vertical", side_nav$attribs$class %||% "")) {
      # Add data-bs-theme="dark" and ensure navbar-expand-lg
      side_nav$attribs[["data-bs-theme"]] <- "dark"
      if (!grepl("navbar-expand-lg", side_nav$attribs$class %||% "")) {
        side_nav$attribs$class <- paste(side_nav$attribs$class, "navbar-expand-lg")
      }
    }
    # show_theme_button on page()/layout_combo() is the single source of
    # truth: strip any toggle navbar_menu() may have built. The theme
    # settings trigger (gear icon) only lives in the topbar (top-right), not
    # the sidebar, so nothing is re-inserted here.
    side_nav <- filter_theme_li(side_nav)
  }

  # Build the top navbar if present
  header_tag <- NULL
  if (!is.null(top_nav) && inherits(top_nav, "tabler.tag") && top_nav$name == "aside") {
    # Extract navbar items from aside tag
    container <- NULL
    for (ch in top_nav$children) {
      if (inherits(ch, "tabler.tag") && ch$name == "div" && grepl("container-fluid", ch$attribs$class %||% "")) {
        container <- ch
        break
      }
    }

    nav_items <- list()
    if (!is.null(container)) {
      for (c2 in container$children) {
        if (inherits(c2, "tabler.tag") && c2$name == "div" && !is.null(c2$attribs$id) && c2$attribs$id == "sidebar-menu") {
          for (c3 in c2$children) {
            if (inherits(c3, "tabler.tag") && c3$name == "ul") {
              nav_items <- c3$children
            }
          }
        }
      }
    }

    # Sidebar-style theme toggle items (mt-auto) never go in the top navbar
    # for combo layout - strip them here. The theme settings panel is a
    # floating button injected at the page() level, not part of the navbar.
    regular_items <- list()

    for (item in nav_items) {
      if (inherits(item, "tabler.tag") && item$name == "li" &&
        !is.null(item$attribs$class) && grepl("mt-auto", item$attribs$class %||% "")) {
        # This is a sidebar-style theme toggle item - skip it here
      } else {
        regular_items <- c(regular_items, list(item))
      }
    }

    # Build horizontal navbar structure with d-none d-lg-flex
    header_tag <- tags$header(
      class = "navbar navbar-expand-md d-none d-lg-flex d-print-none",
      div(
        class = "container-xl",
        div(
          class = "collapse navbar-collapse",
          id = "navbar-menu",
          ul(
            class = "navbar-nav",
            regular_items
          )
        )
      )
    )
  } else if (!is.null(top_nav) && inherits(top_nav, "tabler.tag")) {
    # If top_nav is already a header tag: strip any toggle navbar_menu() may
    # have built. The theme settings panel is a floating button injected at
    # the page() level, not part of the navbar.
    if (top_nav$name == "header") {
      header_tag <- filter_theme_li(top_nav)
    } else {
      header_tag <- top_nav
    }
  }

  # Mirror the HTML structure in layout-combo.html: page contains aside (sidebar), header (top navbar), then page-wrapper
  tagList(
    div(
      class = "page",

      # Sidebar (if present)
      if (!is.null(side_nav)) side_nav,

      # Top navbar (if present) - shown on wide viewports with d-none d-lg-flex d-print-none
      if (!is.null(header_tag)) header_tag,

      # Main content wrapper
      div(
        class = "page-wrapper",
        # Body content (includes page-header and page-body)
        body,
        # Footer
        if (!is.null(footer)) footer
      )
    )
  )
}
