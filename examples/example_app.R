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
dashboardContent <- tablerBody(
  # Page header
  tablerPageHeader(
    title = "Dashboard Overview",
    subtitle = "Explore penguin and car datasets with interactive visualizations"
  ),
  
  tablerTabItems(
    tablerTabItem(
      tabName = "penguins",
      fluidRow(
        tablerValueBox(
          value = "344",
          title = "Total Penguins",
          icon = "users",
          color = "primary",
          width = 3
        ),
        tablerValueBox(
          value = "3",
          title = "Species",
          icon = "brand-github",
          color = "success",
          width = 3
        ),
        tablerValueBox(
          value = "3",
          title = "Islands",
          icon = "map-pin",
          color = "warning",
          width = 3
        ),
        tablerValueBox(
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
          tablerCard(
            title = "Body Mass by Species",
            highchartOutput("mass_boxplot", height = "400px"),
            status = "primary"
          )
        ),
        column(
          4,
          tablerAlert(
            title = "Dataset Information",
            ifelse(HW != "yes",
              paste("Palmer Penguins data with", LAYOUT_TYPE, "layout."),
              paste("נתוני פינגווינים עם פריסת", LAYOUT_TYPE)
            ),
            type = "info",
            dismissible = TRUE
          ),
          tablerCard(
            title = "Species Distribution",
            highchartOutput("species_pie", height = "300px")
          )
        )
      ),
      
      fluidRow(
        column(
          6,
          tablerCard(
            title = "Flipper Length vs Body Mass",
            highchartOutput("scatter_plot", height = "400px"),
            status = "success"
          )
        ),
        column(
          6,
          tablerCard(
            title = "Bill Length by Island",
            highchartOutput("bill_column", height = "400px"),
            status = "warning"
          )
        )
      ),
      
      fluidRow(
        column(
          12,
          tablerCard(
            title = "Penguins Count by Year and Species",
            highchartOutput("year_line", height = "350px"),
            status = "info"
          )
        )
      )
    ),
    
    tablerTabItem(
      tabName = "mtcars",
      fluidRow(
        tablerValueBox(
          value = "32",
          title = "Total Cars",
          icon = "car",
          color = "blue",
          width = 3
        ),
        tablerValueBox(
          value = "3-8",
          title = "Cylinder Range",
          icon = "engine",
          color = "red",
          width = 3
        ),
        tablerValueBox(
          value = "10.4-33.9",
          title = "MPG Range",
          icon = "gauge",
          color = "green",
          width = 3
        ),
        tablerValueBox(
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
          tablerCard(
            title = "MPG Distribution by Cylinders",
            highchartOutput("mpg_boxplot", height = "400px"),
            status = "blue"
          )
        ),
        column(
          4,
          tablerAlert(
            title = "Dataset Information",
            "Motor Trend Car Road Tests (1974 Motor Trend magazine).",
            type = "info",
            dismissible = TRUE
          ),
          tablerCard(
            title = "Transmission Types",
            highchartOutput("transmission_pie", height = "300px")
          )
        )
      ),
      
      fluidRow(
        column(
          6,
          tablerCard(
            title = "Weight vs MPG",
            highchartOutput("weight_scatter", height = "400px"),
            status = "green"
          )
        ),
        column(
          6,
          tablerCard(
            title = "Horsepower by Cylinders",
            highchartOutput("hp_column", height = "400px"),
            status = "red"
          )
        )
      ),
      
      fluidRow(
        column(
          12,
          tablerCard(
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
  "boxed" = tablerPage(
    title = "Boxed Layout - Tabler Dashboard",
    layout = "boxed",
    navbar = tablerNavbar(title = "Boxed Layout"),
    body = dashboardContent,
    footer = tablerFooter(
      left = "© 2025 Pacha",
      right = "Built with Tabler - Boxed Layout"
    )
  ),

  "combo" = tablerPage(
    title = "Combo Layout - Tabler Dashboard",
    layout = "combo",
    navbar = list(
      top = tablerNavbar(title = "Combo Layout"),
      side = tablerSidebar(
        title = "Combo Layout",
        sidebarMenu(
          menuItem("Palmer penguins", tabName = "penguins", icon = "home"),
          menuItem("Motor Trend Cars", tabName = "mtcars", icon = "car", badge = "New")
        ),
        theme = "dark"
      )
    ),
    body = dashboardContent,
    footer = tablerFooter(
      left = "© 2025 Pacha",
      right = "Built with Tabler - Combo Layout"
    )
  ),

  "vertical" = tablerPage(
    title = "Vertical Layout - Tabler Dashboard",
    layout = "vertical",
    navbar = tablerSidebar(
      title = "Vertical Layout",
      sidebarMenu(
        menuItem("Palmer penguins", tabName = "penguins", icon = "home"),
        menuItem("Motor Trend Cars", tabName = "mtcars", icon = "car")
      ),
      theme = "dark"
    ),
    body = dashboardContent,
    footer = tablerFooter(
      left = "© 2025 Pacha",
      right = "Built with Tabler - Vertical Layout"
    )
  ),

  "condensed" = tablerPage(
    title = "Condensed Layout - Tabler Dashboard",
    layout = "condensed",
    navbar = tablerNavbar(title = "Condensed Layout"),
    body = dashboardContent,
    footer = tablerFooter(
      left = "© 2025 Pacha",
      right = "Built with Tabler - Condensed Layout"
    )
  ),

  "navbar-dark" = tablerPage(
    title = "Dark Navbar Layout - Tabler Dashboard",
    layout = "navbar-dark",
    navbar = tablerNavbar(title = "Dark Navbar Layout"),
    body = dashboardContent,
    footer = tablerFooter(
      left = "© 2025 Pacha",
      right = "Built with Tabler - Dark Navbar Layout"
    )
  ),
  
  "rtl" = tablerPage(
    title = "RTL Layout - Tabler Dashboard",
    layout = "rtl",
    navbar = tablerNavbar(title = "פריסת RTL"),
    body = dashboardContent,
    footer = tablerFooter(
      left = "© 2024 החברה שלי",
      right = "נבנה עם Tabler"
    )
  ),
  
  "fluid" = tablerPage(
    title = "Fluid Layout - Tabler Dashboard",
    layout = "fluid",
    navbar = tablerNavbar(title = "Fluid Layout"),
    body = dashboardContent,
    footer = tablerFooter(
      left = "© 2025 Pacha",
      right = "Built with Tabler - Fluid Layout"
    )
  ),
  
  "fluid-vertical" = tablerPage(
    title = "Fluid Vertical Layout - Tabler Dashboard",
    layout = "fluid-vertical",
    navbar = tablerSidebar(
      title = "Fluid Vertical Layout",
      sidebarMenu(
        menuItem("Palmer penguins", tabName = "penguins", icon = "home"),
        menuItem("Motor Trend Cars", tabName = "mtcars", icon = "car")
      ),
      theme = "dark"
    ),
    body = dashboardContent,
    footer = tablerFooter(
      left = "© 2025 Pacha",
      right = "Built with Tabler - Fluid Vertical Layout"
    )
  ),
  
  "horizontal" = tablerPage(
    title = "Horizontal Layout - Tabler Dashboard",
    layout = "horizontal",
    navbar = horizontalMenu(
      menuItem("Palmer Penguins", tabName = "penguins", icon = "home"),
      menuItem("Motor Trend Cars", tabName = "mtcars", icon = "car")
    ),
    body = dashboardContent,
    footer = tablerFooter(
      left = "© 2025 Pacha",
      right = "Built with Tabler - Horizontal Layout"
    )
  ),
  
  "navbar-overlap" = tablerPage(
    title = "Navbar Overlap Layout - Tabler Dashboard",
    layout = "navbar-overlap",
    navbar = tablerNavbar(title = "Navbar Overlap Layout"),
    body = dashboardContent,
    footer = tablerFooter(
      left = "© 2025 Pacha",
      right = "Built with Tabler - Navbar Overlap Layout"
    )
  ),
  
  "navbar-sticky" = tablerPage(
    title = "Navbar Sticky Layout - Tabler Dashboard",
    layout = "navbar-sticky",
    navbar = tablerNavbar(title = "Navbar Sticky Layout"),
    body = dashboardContent,
    footer = tablerFooter(
      left = "© 2025 Pacha",
      right = "Built with Tabler - Navbar Sticky Layout"
    )
  ),
  
  "vertical-right" = tablerPage(
    title = "Vertical Right Layout - Tabler Dashboard",
    layout = "vertical-right",
    navbar = tablerSidebar(
      title = "Vertical Right Layout",
      sidebarMenu(
        menuItem("Palmer penguins", tabName = "penguins", icon = "home"),
        menuItem("Motor Trend Cars", tabName = "mtcars", icon = "car")
      ),
      theme = "dark"
    ),
    body = dashboardContent,
    footer = tablerFooter(
      left = "© 2025 Pacha",
      right = "Built with Tabler - Vertical Right Layout"
    )
  ),
  
  "vertical-transparent" = tablerPage(
    title = "Vertical Transparent Layout - Tabler Dashboard",
    layout = "vertical-transparent",
    navbar = tablerSidebar(
      title = "Vertical Transparent Layout",
      sidebarMenu(
        menuItem("Palmer penguins", tabName = "penguins", icon = "home"),
        menuItem("Motor Trend Cars", tabName = "mtcars", icon = "car")
      ),
      theme = "light"
    ),
    body = dashboardContent,
    footer = tablerFooter(
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
