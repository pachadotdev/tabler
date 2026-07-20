#' @title UI Dispatcher
#' @noRd
ui <- page(
  theme = "light",
  color = "teal",
  base = "slate",
  radius = 1,
  # layout = "fluid-vertical",
  layout = "combo",
  show_theme_button = TRUE,
  title = "Combo Layout",
  navbar = list(side = sidebar_nav, top = topbar_nav),
  body = list(
    tab_items(
      tab_item(
        "mtcars",
        histogram_section(
          "Mtcars", "Histogram",
          "mtcars_col", c("mpg", "hp", "wt", "qsec", "disp", "drat"), "mpg",
          "mtcars_bins", "mtcars_plot", "mtcars_download"
        )
      ),
      tab_item(
        "iris",
        histogram_section(
          "Iris", "Histogram",
          "iris_col", c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"), "Sepal.Length",
          "iris_bins", "iris_plot", "iris_download"
        )
      ),
      tab_item(
        "airquality",
        histogram_section(
          "Airquality", "Histogram",
          "airquality_col", c("Ozone", "Solar.R", "Wind", "Temp"), "Temp",
          "airquality_bins", "airquality_plot", "airquality_download"
        )
      )
    )
  ),
  footer = footer(
    left = "Tabler",
    right = tags$span("v1.4.0")
  )
)
