# Example of the new simplified tabler package with different layouts
library(shiny)
library(highcharter)
library(dplyr)
library(palmerpenguins)

# either this
library(tabler)

# or this
# document()
# load_all()

# Choose which layout to test
LAYOUT_TYPES <- c(
  "boxed", "combo", "condensed",
  "fluid", "fluid-vertical", "horizontal",
  "navbar-dark", "navbar-overlap", "navbar-sticky",
  "rtl", "vertical", "vertical-right",
  "vertical-transparent"
)

LAYOUT_TYPE <- LAYOUT_TYPES[11]

HW <- "no"

# Common content for all layouts
dashboard_content <- tabler_body(
  # Page header
  tabler_page_header(
    title = "Dashboard Overview",
    subtitle = "Explore penguin and car datasets with interactive visualizations"
  ),
  
  tabler_tab_items(
    tabler_tab_item(
      tab_name = "penguins",
      fluidRow(
        tabler_value_box(
          value = "344",
          title = "Total Penguins",
          icon = "sum",
          color = "primary",
          width = 3
        ),
        tabler_value_box(
          value = "3",
          title = "Species",
          icon = "paw",
          color = "success",
          width = 3
        ),
        tabler_value_box(
          value = "3",
          title = "Islands",
          icon = "map-pin",
          color = "warning",
          width = 3
        ),
        tabler_value_box(
          value = "2007-2009",
          title = "Study Years",
          icon = "calendar",
          color = "info",
          width = 3
        )
      ),

      fluidRow(
        column(12)
      ),

      fluidRow(
        column(
          8,
          tabler_card(
            title = "Body Mass by Species",
            highchartOutput("mass_boxplot", height = "400px"),
            status = "primary"
          )
        ),
        column(
          4,
          tabler_alert(
            title = "Dataset Information",
            ifelse(HW != "yes",
              paste("Palmer Penguins data with", LAYOUT_TYPE, "layout."),
              paste("נתוני פינגווינים עם פריסת", LAYOUT_TYPE)
            ),
            type = "info",
            dismissible = TRUE
          ),
          tabler_card(
            title = "Species Distribution",
            highchartOutput("species_pie", height = "300px")
          )
        )
      ),
      
      fluidRow(
        column(
          6,
          tabler_card(
            title = "Flipper Length vs Body Mass",
            highchartOutput("scatter_plot", height = "400px"),
            status = "success"
          )
        ),
        column(
          6,
          tabler_card(
            title = "Bill Length by Island",
            highchartOutput("bill_column", height = "400px"),
            status = "warning"
          )
        )
      ),
      
      fluidRow(
        column(
          12,
          tabler_card(
            title = "Penguins Count by Year and Species",
            highchartOutput("year_line", height = "350px"),
            status = "info"
          )
        )
      )
    ),
    
    tabler_tab_item(
      tab_name = "mtcars",
      fluidRow(
        tabler_value_box(
          value = "32",
          title = "Total Cars",
          icon = "car",
          color = "blue",
          width = 3
        ),
        tabler_value_box(
          value = "3-8",
          title = "Cylinder Range",
          icon = "engine",
          color = "red",
          width = 3
        ),
        tabler_value_box(
          value = "10.4-33.9",
          title = "MPG Range",
          icon = "gauge",
          color = "green",
          width = 3
        ),
        tabler_value_box(
          value = "1973-74",
          title = "Model Years",
          icon = "calendar",
          color = "purple",
          width = 3
        )
      ),
      
      fluidRow(
        column(
          8,
          tabler_card(
            title = "MPG Distribution by Cylinders",
            highchartOutput("mpg_boxplot", height = "400px"),
            status = "blue"
          )
        ),
        column(
          4,
          tabler_alert(
            title = "Dataset Information",
            "Motor Trend Car Road Tests (1974 Motor Trend magazine).",
            type = "info",
            dismissible = TRUE
          ),
          tabler_card(
            title = "Transmission Types",
            highchartOutput("transmission_pie", height = "300px")
          )
        )
      ),
      
      fluidRow(
        column(
          6,
          tabler_card(
            title = "Weight vs MPG",
            highchartOutput("weight_scatter", height = "400px"),
            status = "green"
          )
        ),
        column(
          6,
          tabler_card(
            title = "Horsepower by Cylinders",
            highchartOutput("hp_column", height = "400px"),
            status = "red"
          )
        )
      ),
      
      fluidRow(
        column(
          12,
          tabler_card(
            title = "Average MPG by Cylinders",
            highchartOutput("mpg_column", height = "350px"),
            status = "purple"
          )
        )
      )
    )
  )
)

# Build UI based on selected layout
ui <- switch(
  LAYOUT_TYPE,
  "boxed" = tabler_page(
    title = "Boxed Layout - Tabler Dashboard",
    layout = "boxed",
    navbar = tabler_navbar(title = "Boxed Layout"),
    body = dashboard_content,
    footer = tabler_footer(
      left = "© 2025 Pacha",
      right = "Built with Tabler - Boxed Layout"
    )
  ),

  "combo" = tabler_page(
    title = "Combo Layout - Tabler Dashboard",
    layout = "combo",
    navbar = list(
      top = tabler_navbar(title = "Combo Layout"),
      side = tabler_sidebar(
        title = "Combo Layout",
        sidebar_menu(
          menu_item("Palmer penguins", tab_name = "penguins", icon = "home"),
          menu_item("Motor Trend Cars", tab_name = "mtcars", icon = "car", badge = "New")
        ),
        theme = "dark"
      )
    ),
    body = dashboard_content,
    footer = tabler_footer(
      left = "© 2025 Pacha",
      right = "Built with Tabler - Combo Layout"
    )
  ),

  "vertical" = tabler_page(
    title = "Vertical Layout - Tabler Dashboard",
    layout = "vertical",
    navbar = tabler_sidebar(
      title = "Vertical Layout",
      sidebar_menu(
        menu_item("Palmer penguins", tab_name = "penguins", icon = "home"),
        menu_item("Motor Trend Cars", tab_name = "mtcars", icon = "car")
      ),
      theme = "dark"
    ),
    body = dashboard_content,
    footer = tabler_footer(
      left = "© 2025 Pacha",
      right = "Built with Tabler - Vertical Layout"
    )
  ),

  "condensed" = tabler_page(
    title = "Condensed Layout - Tabler Dashboard",
    layout = "condensed",
    navbar = tabler_navbar(title = "Condensed Layout"),
    body = dashboard_content,
    footer = tabler_footer(
      left = "© 2025 Pacha",
      right = "Built with Tabler - Condensed Layout"
    )
  ),

  "navbar-dark" = tabler_page(
    title = "Dark Navbar Layout - Tabler Dashboard",
    layout = "navbar-dark",
    navbar = tabler_navbar(title = "Dark Navbar Layout"),
    body = dashboard_content,
    footer = tabler_footer(
      left = "© 2025 Pacha",
      right = "Built with Tabler - Dark Navbar Layout"
    )
  ),
  
  "rtl" = tabler_page(
    title = "RTL Layout - Tabler Dashboard",
    layout = "rtl",
    navbar = tabler_navbar(title = "פריסת RTL"),
    body = dashboard_content,
    footer = tabler_footer(
      left = "© 2025 החברה שלי",
      right = "נבנה עם Tabler"
    )
  ),
  
  "fluid" = tabler_page(
    title = "Fluid Layout - Tabler Dashboard",
    layout = "fluid",
    navbar = tabler_navbar(title = "Fluid Layout"),
    body = dashboard_content,
    footer = tabler_footer(
      left = "© 2025 Pacha",
      right = "Built with Tabler - Fluid Layout"
    )
  ),
  
  "fluid-vertical" = tabler_page(
    title = "Fluid Vertical Layout - Tabler Dashboard",
    layout = "fluid-vertical",
    navbar = tabler_sidebar(
      title = "Fluid Vertical Layout",
      sidebar_menu(
        menu_item("Palmer penguins", tab_name = "penguins", icon = "home"),
        menu_item("Motor Trend Cars", tab_name = "mtcars", icon = "car")
      ),
      theme = "dark"
    ),
    body = dashboard_content,
    footer = tabler_footer(
      left = "© 2025 Pacha",
      right = "Built with Tabler - Fluid Vertical Layout"
    )
  ),
  
  "horizontal" = tabler_page(
    title = "Horizontal Layout - Tabler Dashboard",
    layout = "horizontal",
    navbar = horizontalMenu(
      menu_item("Palmer Penguins", tab_name = "penguins", icon = "home"),
      menu_item("Motor Trend Cars", tab_name = "mtcars", icon = "car")
    ),
    body = dashboard_content,
    footer = tabler_footer(
      left = "© 2025 Pacha",
      right = "Built with Tabler - Horizontal Layout"
    )
  ),
  
  "navbar-overlap" = tabler_page(
    title = "Navbar Overlap Layout - Tabler Dashboard",
    layout = "navbar-overlap",
    navbar = tabler_navbar(title = "Navbar Overlap Layout"),
    body = dashboard_content,
    footer = tabler_footer(
      left = "© 2025 Pacha",
      right = "Built with Tabler - Navbar Overlap Layout"
    )
  ),
  
  "navbar-sticky" = tabler_page(
    title = "Navbar Sticky Layout - Tabler Dashboard",
    layout = "navbar-sticky",
    navbar = tabler_navbar(title = "Navbar Sticky Layout"),
    body = dashboard_content,
    footer = tabler_footer(
      left = "© 2025 Pacha",
      right = "Built with Tabler - Navbar Sticky Layout"
    )
  ),
  
  "vertical-right" = tabler_page(
    title = "Vertical Right Layout - Tabler Dashboard",
    layout = "vertical-right",
    navbar = tabler_sidebar(
      title = "Vertical Right Layout",
      sidebar_menu(
        menu_item("Palmer penguins", tab_name = "penguins", icon = "home"),
        menu_item("Motor Trend Cars", tab_name = "mtcars", icon = "car")
      ),
      theme = "dark"
    ),
    body = dashboard_content,
    footer = tabler_footer(
      left = "© 2025 Pacha",
      right = "Built with Tabler - Vertical Right Layout"
    )
  ),
  
  "vertical-transparent" = tabler_page(
    title = "Vertical Transparent Layout - Tabler Dashboard",
    layout = "vertical-transparent",
    navbar = tabler_sidebar(
      title = "Vertical Transparent Layout",
      sidebar_menu(
        menu_item("Palmer penguins", tab_name = "penguins", icon = "home"),
        menu_item("Motor Trend Cars", tab_name = "mtcars", icon = "car")
      ),
      theme = "light"
    ),
    body = dashboard_content,
    footer = tabler_footer(
      left = "© 2025 Pacha",
      right = "Built with Tabler - Vertical Transparent Layout"
    )
  )
)

server <- function(input, output, session) {
  # Prepare data
  penguins_clean <- na.omit(palmerpenguins::penguins)
  
  # Penguins Tab Charts
  # Boxplot: Body Mass by Species
  output$mass_boxplot <- renderHighchart({
    d <- data_to_boxplot(
      penguins_clean,
      body_mass_g,
      species
    )

    highchart() %>%
      hc_add_series_list(d) %>%
      hc_title(text = NULL) %>%
      hc_xAxis(type = "category") %>%
      hc_yAxis(title = list(text = "Body Mass (g)")) %>%
      hc_legend(enabled = FALSE)
  })
  
  # Pie Chart: Species Distribution
  output$species_pie <- renderHighchart({
    species_counts <- penguins_clean %>%
      count(species)
    
    highchart() %>%
      hc_chart(type = "pie") %>%
      hc_add_series(
        data = species_counts,
        type = "pie",
        hcaes(name = species, y = n),
        name = "Count"
      ) %>%
      hc_colors(c("#4299e1", "#f56565", "#48bb78")) %>%
      hc_title(text = NULL) %>%
      hc_plotOptions(
        pie = list(
          dataLabels = list(enabled = TRUE, format = "{point.name}: {point.y}")
        )
      )
  })
  
  # Scatter Plot: Flipper Length vs Body Mass
  output$scatter_plot <- renderHighchart({
    highchart() %>%
      hc_chart(type = "scatter") %>%
      hc_add_series(
        data = penguins_clean %>% filter(species == "Adelie"),
        type = "scatter",
        hcaes(x = flipper_length_mm, y = body_mass_g),
        name = "Adelie",
        color = "#4299e1"
      ) %>%
      hc_add_series(
        data = penguins_clean %>% filter(species == "Chinstrap"),
        type = "scatter",
        hcaes(x = flipper_length_mm, y = body_mass_g),
        name = "Chinstrap",
        color = "#f56565"
      ) %>%
      hc_add_series(
        data = penguins_clean %>% filter(species == "Gentoo"),
        type = "scatter",
        hcaes(x = flipper_length_mm, y = body_mass_g),
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
  
  # Column Chart: Bill Length by Island
  output$bill_column <- renderHighchart({
    bill_data <- penguins_clean %>%
      group_by(island, species) %>%
      summarise(avg_bill = mean(bill_length_mm, na.rm = TRUE), .groups = "drop")
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_xAxis(categories = unique(bill_data$island)) %>%
      hc_add_series(
        data = bill_data %>% filter(species == "Adelie"),
        type = "column",
        hcaes(x = island, y = avg_bill),
        name = "Adelie",
        color = "#4299e1"
      ) %>%
      hc_add_series(
        data = bill_data %>% filter(species == "Chinstrap"),
        type = "column",
        hcaes(x = island, y = avg_bill),
        name = "Chinstrap",
        color = "#f56565"
      ) %>%
      hc_add_series(
        data = bill_data %>% filter(species == "Gentoo"),
        type = "column",
        hcaes(x = island, y = avg_bill),
        name = "Gentoo",
        color = "#48bb78"
      ) %>%
      hc_title(text = NULL) %>%
      hc_yAxis(title = list(text = "Average Bill Length (mm)")) %>%
      hc_tooltip(pointFormat = "<b>{series.name}</b>: {point.y:.1f} mm")
  })
  
  # Line Chart: Penguins Count by Year and Species
  output$year_line <- renderHighchart({
    year_data <- penguins_clean %>%
      count(year, species)
    
    highchart() %>%
      hc_chart(type = "line") %>%
      hc_xAxis(categories = unique(year_data$year)) %>%
      hc_add_series(
        data = year_data %>% filter(species == "Adelie"),
        type = "line",
        hcaes(x = year, y = n),
        name = "Adelie",
        color = "#4299e1"
      ) %>%
      hc_add_series(
        data = year_data %>% filter(species == "Chinstrap"),
        type = "line",
        hcaes(x = year, y = n),
        name = "Chinstrap",
        color = "#f56565"
      ) %>%
      hc_add_series(
        data = year_data %>% filter(species == "Gentoo"),
        type = "line",
        hcaes(x = year, y = n),
        name = "Gentoo",
        color = "#48bb78"
      ) %>%
      hc_title(text = NULL) %>%
      hc_yAxis(title = list(text = "Count")) %>%
      hc_tooltip(pointFormat = "<b>{series.name}</b>: {point.y}")
  })
  
  # MTCars Tab Charts
  # Boxplot: MPG by Cylinders
  output$mpg_boxplot <- renderHighchart({
    mtcars2 <- mtcars %>%
      mutate(cyl = as.factor(cyl))
    
    d <- data_to_boxplot(mtcars2, mpg, cyl)
    
    highchart() %>%
      hc_add_series_list(d) %>%
      hc_title(text = NULL) %>%
      hc_xAxis(type = "category", title = list(text = "Cylinders")) %>%
      hc_yAxis(title = list(text = "Miles per Gallon")) %>%
      hc_legend(enabled = FALSE) %>%
      hc_colors(c("#206bc4", "#d63939", "#f59f00"))
  })
  
  # Pie Chart: Transmission Distribution
  output$transmission_pie <- renderHighchart({
    trans_counts <- mtcars %>%
      mutate(transmission = ifelse(am == 0, "Automatic", "Manual")) %>%
      count(transmission)
    
    highchart() %>%
      hc_chart(type = "pie") %>%
      hc_add_series(
        data = trans_counts,
        type = "pie",
        hcaes(name = transmission, y = n),
        name = "Count"
      ) %>%
      hc_colors(c("#206bc4", "#f59f00")) %>%
      hc_title(text = NULL) %>%
      hc_plotOptions(
        pie = list(
          dataLabels = list(enabled = TRUE, format = "{point.name}: {point.y}")
        )
      )
  })
  
  # Scatter Plot: Weight vs MPG
  output$weight_scatter <- renderHighchart({
    highchart() %>%
      hc_chart(type = "scatter") %>%
      hc_add_series(
        data = mtcars,
        type = "scatter",
        hcaes(x = wt, y = mpg),
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
  
  # Column Chart: Horsepower by Cylinders
  output$hp_column <- renderHighchart({
    hp_data <- mtcars %>%
      mutate(cyl = as.factor(cyl)) %>%
      group_by(cyl) %>%
      summarise(avg_hp = mean(hp, na.rm = TRUE), .groups = "drop")
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_xAxis(categories = hp_data$cyl, title = list(text = "Cylinders")) %>%
      hc_add_series(
        data = hp_data,
        type = "column",
        hcaes(x = cyl, y = avg_hp),
        name = "Average Horsepower",
        color = "#d63939"
      ) %>%
      hc_title(text = NULL) %>%
      hc_yAxis(title = list(text = "Average Horsepower")) %>%
      hc_tooltip(pointFormat = "<b>{point.y:.1f}</b> HP") %>%
      hc_legend(enabled = FALSE)
  })
  
  # Column Chart: Average MPG by Cylinders
  output$mpg_column <- renderHighchart({
    mpg_data <- mtcars %>%
      mutate(cyl = as.factor(cyl)) %>%
      group_by(cyl) %>%
      summarise(avg_mpg = mean(mpg, na.rm = TRUE), .groups = "drop")
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_xAxis(categories = mpg_data$cyl, title = list(text = "Cylinders")) %>%
      hc_add_series(
        data = mpg_data,
        type = "column",
        hcaes(x = cyl, y = avg_mpg),
        name = "Average MPG",
        color = "#ae3ec9"
      ) %>%
      hc_title(text = NULL) %>%
      hc_yAxis(title = list(text = "Average MPG")) %>%
      hc_tooltip(pointFormat = "<b>{point.y:.1f}</b> MPG") %>%
      hc_legend(enabled = FALSE)
  })
}

# Run the app
if (interactive()) {
  shinyApp(ui, server)
}
