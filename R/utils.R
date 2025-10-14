#' Validate CSS color values
#'
#' @param color Color value to validate
#' @return TRUE if valid, FALSE otherwise
#' @keywords internal
validate_color <- function(color) {
  valid_colors <- c(
    "primary", "secondary", "success", "danger", "warning",
    "info", "light", "dark", "muted", "white"
  )
  color %in% valid_colors
}

#' Validate CSS size values
#'
#' @param size Size value to validate
#' @return TRUE if valid, FALSE otherwise
#' @keywords internal
validate_size <- function(size) {
  valid_sizes <- c("sm", "md", "lg", "xl")
  size %in% valid_sizes
}

#' Create CSS classes string
#'
#' @param ... Class names to combine
#' @return Combined class string
#' @keywords internal
css_class <- function(...) {
  classes <- c(...)
  classes <- classes[!is.null(classes) & classes != ""]
  if (length(classes) == 0) {
    return(NULL)
  }
  paste(classes, collapse = " ")
}

#' Validate tab name format
#'
#' @param name Tab name to validate
#' @keywords internal
validate_tab_name <- function(name) {
  if (grepl(".", name, fixed = TRUE)) {
    stop("tabName must not have a '.' in it.")
  }
}
