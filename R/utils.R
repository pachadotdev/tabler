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
