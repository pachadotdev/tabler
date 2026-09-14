#' @title Server Side
#' @noRd
app_server <- function(input, output, session) {
  output$mtcars_plot <- render_histogram(
    datasets::mtcars,
    reactive(input$mtcars_col %||% "mpg"),
    reactive(input$mtcars_bins %||% 5)
  )

  output$iris_plot <- render_histogram(
    datasets::iris,
    reactive(input$iris_col %||% "Sepal.Length"),
    reactive(input$iris_bins %||% 5)
  )

  output$airquality_plot <- render_histogram(
    datasets::airquality,
    reactive(input$airquality_col %||% "Temp"),
    reactive(input$airquality_bins %||% 5)
  )

  # Download handlers — export the full underlying dataset as CSV
  output$mtcars_download <- download_handler(
    filename = "mtcars.csv",
    content  = function(file) utils::write.csv(datasets::mtcars, file, row.names = TRUE)
  )

  output$iris_download <- download_handler(
    filename = "iris.csv",
    content  = function(file) utils::write.csv(datasets::iris, file, row.names = FALSE)
  )

  output$airquality_download <- download_handler(
    filename = "airquality.csv",
    content  = function(file) utils::write.csv(datasets::airquality, file, row.names = FALSE)
  )

  sync_url(session, exclude = c("parameters", "to", "not", "show"))
}
