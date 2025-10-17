#' Create a Progress Bar
#'
#' Display progress or completion status
#'
#' @param value Progress value (0-100)
#' @param color Progress bar color
#' @param size Size: "sm", "md", "lg"
#' @param label Show percentage label
#'
#' @export
tablerProgress <- function(value, color = "primary", size = "md", label = TRUE) {
  size_class <- if (size != "md") paste0("progress-", size) else NULL

  shiny::tags$div(
    class = paste("progress", size_class),
    shiny::tags$div(
      class = paste("progress-bar bg-", color),
      style = paste0("width: ", value, "%"),
      role = "progressbar",
      `aria-valuenow` = value,
      `aria-valuemin` = "0",
      `aria-valuemax` = "100",
      if (label) paste0(value, "%")
    )
  )
}
