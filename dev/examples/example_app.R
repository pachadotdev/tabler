# Example of the new simplified tabler package with different layouts
library(shiny)
# library(tabler)
document()
load_all()

# Choose which layout to test
# Options: "default", "boxed", "combo", "condensed", "fluid" PENDING,
#          "fluid-vertical" PENDING, "horizontal" PENDING, "navbar-dark",
#          "navbar-overlap" PENDING, "navbar-sticky" PENDING, "rtl" PENDING, "vertical",
#          "vertical-right" PENDING, "vertical-transparent"
LAYOUT_TYPE <- "vertical-transparent"

# Common content for all layouts
dashboardContent <- tablerBody(
  fluidRow(
    tablerValueBox(
      value = "1,352",
      title = "Total Users",
      icon = "users",
      # color options: "primary", "secondary", "success", "warning", "danger", "info", "dark", "light"
      color = "primary",
      width = 3
    )
  ),
  
  fluidRow(
    column(8,
      tablerCard(
        title = "Sample Chart",
        plotOutput("plot"),
        status = "primary"
      )
    ),
    column(4,
      tablerCard(
        title = "Progress Status",
        tablerProgress(75, color = "success", label = TRUE),
        br(),
        tablerProgress(45, color = "warning", label = TRUE),
        br(),
        tablerProgress(90, color = "info", label = TRUE)
      ),
      
      tablerAlert(
        title = "Important Notice",
        paste("This is a", LAYOUT_TYPE, "layout example."),
        type = "info",
        dismissible = TRUE
      )
    )
  )
)

# Build UI based on selected layout
ui <- switch(LAYOUT_TYPE,
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
        menuItem("Analytics", tabName = "analytics", icon = "chart-bar", badge = "New"),
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
  )
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    plot(cars, main = "Sample Plot", col = "steelblue", pch = 19)
  })
}

# Run the app
if (interactive()) {
  shinyApp(ui, server)
}
