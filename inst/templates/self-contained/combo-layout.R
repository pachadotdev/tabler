setwd("~/Documents/tabler/inst/templates/self-contained")

library(tabler)

# Sidebar/topbar ----

svg_text <- paste(
  readLines("./tabler-logo.svg", warn = FALSE),
  collapse = "\n"
)

svg_data_uri <- paste0(
  "data:image/svg+xml;utf8,",
  URLencode(svg_text, reserved = TRUE)
)

sidebar_nav <- navbar_menu(
  brand = sidebar_brand(text = "", img = svg_data_uri, href = "./"),
  menu_item("Mtcars", tab_name = "mtcars", icon = "car"),
  menu_item("Iris", tab_name = "iris", icon = "flower"),
  menu_item("Airquality", tab_name = "airquality", icon = "cloud")
)

topbar_nav <- navbar_menu(
  menu_item("Tabler docs", icon = NULL, href = "https://tabler.io/admin-template/preview"),
  menu_item("Tabler R docs", icon = NULL, href = "https://pacha.dev/tabler"),
  menu_item("Tabler Server docs", icon = NULL, href = "https://pacha.dev/tabler-server"),
  menu_dropdown(
    "Another button",
    icon = "layout-2",
    href = "./",
    items = list(
      c("Placeholder", "./")
    )
  )
)

# Histograms ----

# Helper to build one section: a controls card + a histogram output card,
histogram_section <- function(title, subtitle, col_input_id, col_choices, col_selected,
                               bins_input_id, plot_output_id, download_output_id) {
  list(
    header(title = title, subtitle = subtitle),
    div(
      class = "page-body",
      div(
        class = "container-xl",
        row(
          col4(
            card(
              title = "Controls",
              selectInput(col_input_id, "Column", choices = col_choices, selected = col_selected),
              sliderInput(bins_input_id, "Number of bins:", min = 1, max = 50, value = 5),
              downloadButton(download_output_id, label = "Download CSV")
            )
          ),
          col8(
            card(
              title  = "Output",
              footer = "Histogram",
              plotOutput(plot_output_id)
            )
          )
        )
      )
    )
  )
}

# UI ----

ui <- page(
  theme = "light",
  color = "teal",
  title = "Combo Layout",
  # layout = "fluid-vertical",
  layout = "combo",
  show_theme_button = TRUE,
  # navbar = sidebar_nav,
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

# Server ----

server <- function(input, output, session) {

  # Generic histogram renderer, shared across the three sections
  render_histogram <- function(data, col_reactive, bins_reactive) {
    renderPlot({
      x    <- stats::na.omit(data[[col_reactive()]])
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

tablerApp(ui, server)
