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
#' @param nav_tag Navbar shiny.tag (aside or header)
#' @return Modified navbar tag without theme toggle li
#' @keywords internal
#' @noRd
filter_theme_li <- function(nav_tag) {
  if (is.null(nav_tag) || !inherits(nav_tag, "shiny.tag")) {
    return(nav_tag)
  }

  # Recursively filter children
  nav_tag$children <- lapply(nav_tag$children, function(child) {
    if (inherits(child, "shiny.tag")) {
      # Check if this is a theme toggle li (has mt-auto or ms-md-auto class)
      if (child$name == "li" && (grepl("mt-auto", child$attribs$class %||% "") ||
        grepl("ms-md-auto", child$attribs$class %||% ""))) {
        # Check if any child contains hide-theme class
        has_theme <- FALSE
        check_children <- function(items) {
          for (item in items) {
            if (is.list(item) && !inherits(item, "shiny.tag")) {
              if (check_children(item)) {
                return(TRUE)
              }
            } else if (inherits(item, "shiny.tag")) {
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
