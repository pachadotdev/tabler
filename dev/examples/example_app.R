# Example of the new simplified tabler package with different layouts
library(shiny)
library(highcharter)
library(dplyr)
library(palmerpenguins)
# library(tabler)
document()
load_all()

# Choose which layout to test
# Options: "default", "boxed", "combo", "condensed", "fluid",
#          "fluid-vertical", "horizontal", "navbar-dark",
#          "navbar-overlap", "navbar-sticky", "rtl", "vertical",
#          "vertical-right", "vertical-transparent"
LAYOUT_TYPE <- "vertical-transparent"

HW <- "no"

# Common content for all layouts
dashboardContent <- tablerBody(
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
)

# Build UI based on selected layout
ui <- switch(
  LAYOUT_TYPE,
  "default" = tablerPage(
    title = "Default Layout - Tabler Dashboard",
    layout = "default",
    navbar = tablerNavbar(title = "Default Layout"),
    body = dashboardContent,
    footer = tablerFooter(
      left = "© 2024 My Company",
      right = "Built with Tabler - Default Layout"
    )
  ),

  "boxed" = tablerPage(
    title = "Boxed Layout - Tabler Dashboard",
    layout = "boxed",
    navbar = tablerNavbar(title = "Boxed Layout"),
    body = dashboardContent,
    footer = tablerFooter(
      left = "© 2024 My Company",
      right = "Built with Tabler - Boxed Layout"
    )
  ),

  "combo" = tablerPage(
    title = "Combo Layout - Tabler Dashboard",
    layout = "combo",
    navbar = tablerNavbar(title = "Combo Layout"),
    sidebar = tablerSidebar(
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = "home"),
        menuItem(
          "Analytics",
          tabName = "analytics",
          icon = "chart-bar",
          badge = "New"
        ),
        menuItem("Users", tabName = "users", icon = "users"),
        menuItem("Settings", tabName = "settings", icon = "settings")
      ),
      theme = "dark"
    ),
    body = dashboardContent,
    footer = tablerFooter(
      left = "© 2024 My Company",
      right = "Built with Tabler - Combo Layout"
    )
  ),

  "vertical" = tablerPage(
    title = "Vertical Layout - Tabler Dashboard",
    layout = "vertical",
    sidebar = tablerSidebar(
      sidebarMenu(
        menuItem("Dashboard", icon = "home"),
        menuItem("Analytics", icon = "chart-bar"),
        menuItem("Users", icon = "users"),
        menuItem("Settings", icon = "settings")
      ),
      theme = "dark"
    ),
    body = dashboardContent,
    footer = tablerFooter(
      left = "© 2024 My Company",
      right = "Built with Tabler - Vertical Layout"
    )
  ),

  "condensed" = tablerPage(
    title = "Condensed Layout - Tabler Dashboard",
    layout = "condensed",
    navbar = tablerNavbar(title = "Condensed Layout"),
    body = dashboardContent,
    footer = tablerFooter(
      left = "© 2024 My Company",
      right = "Built with Tabler - Condensed Layout"
    )
  ),

  "navbar-dark" = tablerPage(
    title = "Dark Navbar Layout - Tabler Dashboard",
    layout = "navbar-dark",
    navbar = tablerNavbar(title = "Dark Navbar Layout"),
    body = dashboardContent,
    footer = tablerFooter(
      left = "© 2024 My Company",
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
      left = "© 2024 My Company",
      right = "Built with Tabler - Fluid Layout"
    )
  ),
  
  "fluid-vertical" = tablerPage(
    title = "Fluid Vertical Layout - Tabler Dashboard",
    layout = "fluid-vertical",
    sidebar = tablerSidebar(
      sidebarMenu(
        menuItem("Dashboard", icon = "home"),
        menuItem("Analytics", icon = "chart-bar"),
        menuItem("Users", icon = "users"),
        menuItem("Settings", icon = "settings")
      ),
      theme = "dark"
    ),
    body = dashboardContent,
    footer = tablerFooter(
      left = "© 2024 My Company",
      right = "Built with Tabler - Fluid Vertical Layout"
    )
  ),
  
  "horizontal" = tablerPage(
    title = "Horizontal Layout - Tabler Dashboard",
    layout = "horizontal",
    navbar = tablerNavbar(title = "Horizontal Layout"),
    body = dashboardContent,
    footer = tablerFooter(
      left = "© 2024 My Company",
      right = "Built with Tabler - Horizontal Layout"
    )
  ),
  
  "navbar-overlap" = tablerPage(
    title = "Navbar Overlap Layout - Tabler Dashboard",
    layout = "navbar-overlap",
    navbar = tablerNavbar(title = "Navbar Overlap Layout"),
    body = dashboardContent,
    footer = tablerFooter(
      left = "© 2024 My Company",
      right = "Built with Tabler - Navbar Overlap Layout"
    )
  ),
  
  "navbar-sticky" = tablerPage(
    title = "Navbar Sticky Layout - Tabler Dashboard",
    layout = "navbar-sticky",
    navbar = tablerNavbar(title = "Navbar Sticky Layout"),
    body = dashboardContent,
    footer = tablerFooter(
      left = "© 2024 My Company",
      right = "Built with Tabler - Navbar Sticky Layout"
    )
  ),
  
  "vertical-right" = tablerPage(
    title = "Vertical Right Layout - Tabler Dashboard",
    layout = "vertical-right",
    sidebar = tablerSidebar(
      sidebarMenu(
        menuItem("Dashboard", icon = "home"),
        menuItem("Analytics", icon = "chart-bar"),
        menuItem("Users", icon = "users"),
        menuItem("Settings", icon = "settings")
      ),
      theme = "dark"
    ),
    body = dashboardContent,
    footer = tablerFooter(
      left = "© 2024 My Company",
      right = "Built with Tabler - Vertical Right Layout"
    )
  ),
  
  "vertical-transparent" = tablerPage(
    title = "Vertical Transparent Layout - Tabler Dashboard",
    layout = "vertical-transparent",
    sidebar = tablerSidebar(
      sidebarMenu(
        menuItem("Dashboard", icon = "home"),
        menuItem("Analytics", icon = "chart-bar"),
        menuItem("Users", icon = "users"),
        menuItem("Settings", icon = "settings")
      ),
      theme = "light"
    ),
    body = dashboardContent,
    footer = tablerFooter(
      left = "© 2024 My Company",
      right = "Built with Tabler - Vertical Transparent Layout"
    )
  )
)

server <- function(input, output, session) {
  # Prepare data
  penguins_clean <- na.omit(palmerpenguins::penguins)
  
  # Boxplot: Body Mass by Species
  output$mass_boxplot <- renderHighchart({
    hcboxplot(
      x = penguins_clean$body_mass_g,
      var = penguins_clean$species,
      name = "Body Mass (g)",
      color = c("#4299e1", "#f56565", "#48bb78")
    ) %>%
      hc_title(text = NULL) %>%
      hc_xAxis(title = list(text = "Species")) %>%
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
}

# Run the app
if (interactive()) {
  shinyApp(ui, server)
}
