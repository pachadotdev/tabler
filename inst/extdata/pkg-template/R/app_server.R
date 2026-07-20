server <- function(input, output, session) {
  output$mtcars_plot <- render_histogram(
    mtcars,
    reactive(input$mtcars_col %||% "mpg"),
    reactive(input$mtcars_bins %||% 5)
  )

  output$iris_plot <- render_histogram(
    iris,
    reactive(input$iris_col %||% "Sepal.Length"),
    reactive(input$iris_bins %||% 5)
  )

  output$airquality_plot <- render_histogram(
    airquality,
    reactive(input$airquality_col %||% "Temp"),
    reactive(input$airquality_bins %||% 5)
  )

  # Download handlers — export the full underlying dataset as CSV
  output$mtcars_download <- downloadHandler(
    filename = "mtcars.csv",
    content  = function(file) utils::write.csv(mtcars, file, row.names = TRUE)
  )

  output$iris_download <- downloadHandler(
    filename = "iris.csv",
    content  = function(file) utils::write.csv(iris, file, row.names = FALSE)
  )

  output$airquality_download <- downloadHandler(
    filename = "airquality.csv",
    content  = function(file) utils::write.csv(airquality, file, row.names = FALSE)
  )

  syncUrl(session, exclude = c("parameters", "to", "not", "show"))
}
