#' @title Null-coalescing operator
#' @name or_null
#' @rdname or_null
#' @aliases %||%
#' @param x First value
#' @param y Second value
#' @return x if not NULL, otherwise y
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x

#' @title Validate CSS color values
#' @param color Color value to validate
#' @return TRUE if valid, FALSE otherwise
#' @keywords internal
#' @noRd
validate_color <- function(color) {
  valid_colors <- c(
    "primary", "secondary", "success", "danger", "warning",
    "info", "light", "dark", "muted", "white"
  )
  color %in% valid_colors
}

#' @title Validate CSS size values
#' @param size Size value to validate
#' @return TRUE if valid, FALSE otherwise
#' @keywords internal
#' @noRd
validate_size <- function(size) {
  valid_sizes <- c("sm", "md", "lg", "xl")
  size %in% valid_sizes
}

#' @title Create CSS classes string
#' @param ... Class names to combine
#' @return Combined class string
#' @keywords internal
#' @noRd
css_class <- function(...) {
  classes <- c(...)
  classes <- classes[!is.null(classes) & classes != ""]
  if (length(classes) == 0) {
    return(NULL)
  }
  paste(classes, collapse = " ")
}

#' @title Validate tab name format
#' @param name Tab name to validate
#' @keywords internal
#' @noRd
validate_tab_name <- function(name) {
  if (grepl(".", name, fixed = TRUE)) {
    stop("tab_name must not have a '.' in it.")
  }
}

#' @title Filter theme toggle buttons from navbar tag
#' @param nav_tag Navbar tabler.tag (aside or header)
#' @return Modified navbar tag without theme toggle li
#' @keywords internal
#' @noRd
filter_theme_li <- function(nav_tag) {
  if (is.null(nav_tag) || !inherits(nav_tag, "tabler.tag")) {
    return(nav_tag)
  }

  # Recursively filter children
  nav_tag$children <- lapply(nav_tag$children, function(child) {
    if (inherits(child, "tabler.tag")) {
      # Check if this is a theme toggle li (has mt-auto or ms-md-auto class)
      if (child$name == "li" && (grepl("mt-auto", child$attribs$class %||% "") ||
        grepl("ms-md-auto", child$attribs$class %||% ""))) {
        # Check if any child contains hide-theme class
        has_theme <- FALSE
        check_children <- function(items) {
          for (item in items) {
            if (is.list(item) && !inherits(item, "tabler.tag")) {
              if (check_children(item)) {
                return(TRUE)
              }
            } else if (inherits(item, "tabler.tag")) {
              if (item$name == "a" && (grepl("hide-theme-dark", item$attribs$class %||% "") ||
                grepl("hide-theme-light", item$attribs$class %||% ""))) {
                return(TRUE)
              }
              if (check_children(item$children)) {
                return(TRUE)
              }
            }
          }
          FALSE
        }
        has_theme <- check_children(child$children)
        if (has_theme) {
          return(NULL)
        } # Filter out this li
      }
      # Recursively process children
      child <- filter_theme_li(child)
    }
    child
  })

  # Remove NULLs
  nav_tag$children <- Filter(Negate(is.null), nav_tag$children)
  nav_tag
}

#' @title Build the theme toggle anchor tags
#' @param sidebar_style If `TRUE`, build text-labelled anchors suited to a
#'   vertical sidebar; otherwise build compact icon-only anchors suited to a
#'   horizontal topbar.
#' @return A list of two `<a>` tags (dark/light toggles)
#' @keywords internal
#' @noRd
theme_toggle_anchors <- function(sidebar_style = FALSE) {
  dark_icon <- HTML('<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="icon icon-1"><path d="M12 3c.132 0 .263 0 .393 0a7.5 7.5 0 0 0 7.92 12.446a9 9 0 1 1 -8.313 -12.454z"/></svg>')
  light_icon <- HTML('<svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="icon icon-1"><path d="M12 12m-4 0a4 4 0 1 0 8 0a4 4 0 1 0 -8 0"/><path d="M3 12h1m8 -9v1m8 8h1m-9 8v1m-6.4 -15.4l.7 .7m12.1 -.7l-.7 .7m0 11.4l.7 .7m-12.1 -.7l-.7 .7"/></svg>')

  if (isTRUE(sidebar_style)) {
    list(
      a(
        href = "?theme=dark", class = "nav-link hide-theme-dark", title = "Enable dark mode", `data-bs-toggle` = "tooltip", `data-bs-placement` = "bottom", `aria-label` = "Enable dark mode",
        span(class = "nav-link-icon d-md-none d-lg-inline-block", dark_icon),
        span(class = "nav-link-title", "Dark theme")
      ),
      a(
        href = "?theme=light", class = "nav-link hide-theme-light", title = "Enable light mode", `data-bs-toggle` = "tooltip", `data-bs-placement` = "bottom", `aria-label` = "Enable light mode",
        span(class = "nav-link-icon d-md-none d-lg-inline-block", light_icon),
        span(class = "nav-link-title", "Light theme")
      )
    )
  } else {
    list(
      a(
        href = "?theme=dark", class = "nav-link px-0 hide-theme-dark", title = "Enable dark mode", `data-bs-toggle` = "tooltip", `data-bs-placement` = "bottom",
        dark_icon
      ),
      a(
        href = "?theme=light", class = "nav-link px-0 hide-theme-light", title = "Enable light mode", `data-bs-toggle` = "tooltip", `data-bs-placement` = "bottom",
        light_icon
      )
    )
  }
}

#' @title Build a theme toggle `<li>` for a sidebar or topbar
#' @param sidebar_style If `TRUE`, build the sidebar (`mt-auto`) variant;
#'   otherwise build the topbar (`ms-md-auto`) variant.
#' @return A `<li>` tag
#' @keywords internal
#' @noRd
theme_toggle_li <- function(sidebar_style = FALSE) {
  if (isTRUE(sidebar_style)) {
    li(class = "nav-item mt-auto", theme_toggle_anchors(sidebar_style = TRUE))
  } else {
    li(class = "nav-item ms-md-auto", theme_toggle_anchors(sidebar_style = FALSE))
  }
}

#' @title Insert a tag as the last child of the first `ul.navbar-nav` found
#' @param nav_tag Navbar tabler.tag (aside or header) to search within
#' @param li_tag The `<li>` tag to insert
#' @return The modified `nav_tag` (unchanged if no `ul.navbar-nav` was found)
#' @keywords internal
#' @noRd
insert_theme_li <- function(nav_tag, li_tag) {
  if (is.null(nav_tag) || !inherits(nav_tag, "tabler.tag")) {
    return(nav_tag)
  }

  inserted <- FALSE

  insert_into <- function(tag) {
    if (!inherits(tag, "tabler.tag")) {
      return(tag)
    }
    if (!inserted && tag$name == "ul" && grepl("navbar-nav", tag$attribs$class %||% "")) {
      tag$children <- c(tag$children, list(li_tag))
      inserted <<- TRUE
      return(tag)
    }
    tag$children <- lapply(tag$children, insert_into)
    tag
  }

  insert_into(nav_tag)
}
