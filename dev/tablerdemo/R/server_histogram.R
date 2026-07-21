#' @title Create a Reusable Controls Card and Histogram Output (Server)
#' @noRd
render_histogram <- function(data, col_reactive, bins_reactive) {
  renderPlot({
    x <- stats::na.omit(data[[col_reactive()]])
    bins <- seq(min(x), max(x), length.out = bins_reactive() + 1)
    x |>
      hist(
        breaks = bins,
        col    = "darkgray",
        border = "white",
        main   = NULL,
        xlab   = col_reactive()
      )
  })
}
