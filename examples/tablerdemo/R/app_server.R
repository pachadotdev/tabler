#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import tabler
#' @import dplyr
#' @import rlang
#' @import highcharter
#' @importFrom stats na.omit
#' @noRd
app_server <- function(input, output, session) {
  # Load required packages inside the server to ensure namespace availability
  #' @importFrom highcharter renderHighchart highchart hc_add_series_list hc_add_series hc_chart hc_title hc_xAxis hc_yAxis hc_legend hc_colors hc_plotOptions hc_tooltip hc_add_series_list

  # Prepare data
  penguins_clean <- na.omit(palmerpenguins::penguins)

  # Helper: data_to_boxplot may come from package; assume available in namespace

  # Penguins Tab Charts
  output$mass_boxplot <- renderHighchart({
    d <- data_to_boxplot(
      penguins_clean,
      !!sym("body_mass_g"),
      !!sym("species")
    )

    highchart() %>%
      hc_add_series_list(d) %>%
      hc_title(text = NULL) %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Body Mass (g)")) %>%
      hc_legend(enabled = FALSE)
  })

  output$species_pie <- renderHighchart({
    species_counts <- penguins_clean %>% dplyr::count(!!sym("species"))

    highchart() %>%
      hc_chart(type = "pie") %>%
      hc_add_series(
        data = species_counts,
        type = "pie",
        hcaes(name = !!sym("species"), y = !!sym("n")),
        name = "Count"
      ) %>%
      hc_colors(c("#4299e1", "#f56565", "#48bb78")) %>%
      hc_title(text = NULL) %>%
      hc_plotOptions(
        pie = list(
          dataLabels = list(
            enabled = TRUE,
            format = "{point.name}: {point.y}"
          )
        )
      )
  })

  output$scatter_plot <- renderHighchart({
    highchart() %>%
      hc_chart(type = "scatter") %>%
      hc_add_series(
        data = penguins_clean %>%
          dplyr::filter(!!sym("species") == "Adelie"),
        type = "scatter",
        hcaes(x = !!sym("flipper_length_mm"), y = !!sym("body_mass_g")),
        name = "Adelie",
        color = "#4299e1"
      ) %>%
      hc_add_series(
        data = penguins_clean %>%
          dplyr::filter(!!sym("species") == "Chinstrap"),
        type = "scatter",
        hcaes(x = !!sym("flipper_length_mm"), y = !!sym("body_mass_g")),
        name = "Chinstrap",
        color = "#f56565"
      ) %>%
      hc_add_series(
        data = penguins_clean %>%
          dplyr::filter(!!sym("species") == "Gentoo"),
        type = "scatter",
        hcaes(x = !!sym("flipper_length_mm"), y = !!sym("body_mass_g")),
        name = "Gentoo",
        color = "#48bb78"
      ) %>%
      hc_title(text = NULL) %>%
      hc_xAxis(title = list(text = "Flipper Length (mm)")) %>%
      hc_yAxis(title = list(text = "Body Mass (g)")) %>%
      hc_tooltip(
        pointFormat = "<b>{series.name}</b><br/>Flipper: {point.x} mm<br/>Mass: {point.y} g"
      )
  })

  output$bill_column <- renderHighchart({
    bill_data <- penguins_clean %>%
      dplyr::group_by(!!sym("island"), !!sym("species")) %>%
      dplyr::summarise(
        avg_bill = mean(!!sym("bill_length_mm"), na.rm = TRUE),
        .groups = "drop"
      )

    highchart() %>%
      hc_chart(type = "column") %>%
      hc_xAxis(categories = unique(bill_data$island)) %>%
      hc_add_series(
        data = bill_data %>%
          dplyr::filter(!!sym("species") == "Adelie"),
        type = "column",
        hcaes(x = !!sym("island"), y = !!sym("avg_bill")),
        name = "Adelie",
        color = "#4299e1"
      ) %>%
      hc_add_series(
        data = bill_data %>%
          dplyr::filter(!!sym("species") == "Chinstrap"),
        type = "column",
        hcaes(x = !!sym("island"), y = !!sym("avg_bill")),
        name = "Chinstrap",
        color = "#f56565"
      ) %>%
      hc_add_series(
        data = bill_data %>%
          dplyr::filter(!!sym("species") == "Gentoo"),
        type = "column",
        hcaes(x = !!sym("island"), y = !!sym("avg_bill")),
        name = "Gentoo",
        color = "#48bb78"
      ) %>%
      hc_title(text = NULL) %>%
      hc_yAxis(title = list(text = "Average Bill Length (mm)")) %>%
      hc_tooltip(pointFormat = "<b>{series.name}</b>: {point.y:.1f} mm")
  })

  output$year_line <- renderHighchart({
    year_data <- penguins_clean %>%
      dplyr::count(!!sym("year"), !!sym("species"))

    highchart() %>%
      hc_chart(type = "line") %>%
      hc_xAxis(categories = unique(year_data$year)) %>%
      hc_add_series(
        data = year_data %>%
          dplyr::filter(!!sym("species") == "Adelie"),
        type = "line",
        hcaes(x = !!sym("year"), y = !!sym("n")),
        name = "Adelie",
        color = "#4299e1"
      ) %>%
      hc_add_series(
        data = year_data %>%
          dplyr::filter(!!sym("species") == "Chinstrap"),
        type = "line",
        hcaes(x = !!sym("year"), y = !!sym("n")),
        name = "Chinstrap",
        color = "#f56565"
      ) %>%
      hc_add_series(
        data = year_data %>%
          dplyr::filter(!!sym("species") == "Gentoo"),
        type = "line",
        hcaes(x = !!sym("year"), y = !!sym("n")),
        name = "Gentoo",
        color = "#48bb78"
      ) %>%
      hc_title(text = NULL) %>%
      hc_yAxis(title = list(text = "Count")) %>%
      hc_tooltip(pointFormat = "<b>{series.name}</b>: {point.y}")
  })

  # MTCars Tab Charts
  output$mpg_boxplot <- renderHighchart({
    mtcars2 <- datasets::mtcars %>%
      dplyr::mutate(cyl = as.factor(!!sym("cyl")))

    d <- data_to_boxplot(mtcars2, !!sym("mpg"), !!sym("cyl"))

    highchart() %>%
      hc_add_series_list(d) %>%
      hc_title(text = NULL) %>%
      hc_xAxis(type = "category", title = list(text = "Cylinders")) %>%
      hc_yAxis(title = list(text = "Miles per Gallon")) %>%
      hc_legend(enabled = FALSE) %>%
      hc_colors(c("#206bc4", "#d63939", "#f59f00"))
  })

  output$transmission_pie <- renderHighchart({
    trans_counts <- datasets::mtcars %>%
      dplyr::mutate(
        transmission = ifelse(!!sym("am") == 0, "Automatic", "Manual")
      ) %>%
      dplyr::count(!!sym("transmission"))

    highchart() %>%
      hc_chart(type = "pie") %>%
      hc_add_series(
        data = trans_counts,
        type = "pie",
        hcaes(name = !!sym("transmission"), y = !!sym("n")),
        name = "Count"
      ) %>%
      hc_colors(c("#206bc4", "#f59f00")) %>%
      hc_title(text = NULL) %>%
      hc_plotOptions(
        pie = list(
          dataLabels = list(
            enabled = TRUE,
            format = "{point.name}: {point.y}"
          )
        )
      )
  })

  output$weight_scatter <- renderHighchart({
    highchart() %>%
      hc_chart(type = "scatter") %>%
      hc_add_series(
        data = datasets::mtcars,
        type = "scatter",
        hcaes(x = !!sym("wt"), y = !!sym("mpg")),
        name = "Cars",
        color = "#2fb344"
      ) %>%
      hc_title(text = NULL) %>%
      hc_xAxis(title = list(text = "Weight (1000 lbs)")) %>%
      hc_yAxis(title = list(text = "Miles per Gallon")) %>%
      hc_tooltip(
        pointFormat = "<b>Weight:</b> {point.x:.2f}<br/><b>MPG:</b> {point.y:.1f}"
      )
  })

  output$hp_column <- renderHighchart({
    hp_data <- datasets::mtcars %>%
      dplyr::mutate(cyl = as.factor(!!sym("cyl"))) %>%
      dplyr::group_by(!!sym("cyl")) %>%
      dplyr::summarise(
        avg_hp = mean(!!sym("hp"), na.rm = TRUE),
        .groups = "drop"
      )

    highchart() %>%
      hc_chart(type = "column") %>%
      hc_xAxis(
        categories = hp_data$cyl,
        title = list(text = "Cylinders")
      ) %>%
      hc_add_series(
        data = hp_data,
        type = "column",
        hcaes(x = !!sym("cyl"), y = !!sym("avg_hp")),
        name = "Average Horsepower",
        color = "#d63939"
      ) %>%
      hc_title(text = NULL) %>%
      hc_yAxis(title = list(text = "Average Horsepower")) %>%
      hc_tooltip(pointFormat = "<b>{point.y:.1f}</b> HP") %>%
      hc_legend(enabled = FALSE)
  })

  output$mpg_column <- renderHighchart({
    mpg_data <- datasets::mtcars %>%
      dplyr::mutate(cyl = as.factor(!!sym("cyl"))) %>%
      dplyr::group_by(!!sym("cyl")) %>%
      dplyr::summarise(
        avg_mpg = mean(!!sym("mpg"), na.rm = TRUE),
        .groups = "drop"
      )

    highchart() %>%
      hc_chart(type = "column") %>%
      hc_xAxis(
        categories = mpg_data$cyl,
        title = list(text = "Cylinders")
      ) %>%
      hc_add_series(
        data = mpg_data,
        type = "column",
        hcaes(x = !!sym("cyl"), y = !!sym("avg_mpg")),
        name = "Average MPG",
        color = "#ae3ec9"
      ) %>%
      hc_title(text = NULL) %>%
      hc_yAxis(title = list(text = "Average MPG")) %>%
      hc_tooltip(pointFormat = "<b>{point.y:.1f}</b> MPG") %>%
      hc_legend(enabled = FALSE)
  })
}
