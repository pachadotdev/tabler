# Example of the new simplified tabler package with different layouts
library(shiny)
library(d3po)
library(dplyr)
library(stringr)
# library(tabler)
document()
load_all()

# Choose which layout to test
# Options: "default", "boxed", "combo", "condensed", "fluid" PENDING,
#          "fluid-vertical" PENDING, "horizontal" PENDING, "navbar-dark",
#          "navbar-overlap" PENDING, "navbar-sticky" PENDING, "rtl" PENDING, "vertical",
#          "vertical-right" PENDING, "vertical-transparent" PENDING
LAYOUT_TYPE <- "condensed"

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
    column(12)
  ),

  fluidRow(
    column(
      8,
      tablerCard(
        title = "Sample Chart",
        d3po_output("count_tree", height = "650px"),
        status = "primary"
      )
    ),
    column(
      4,
      tablerAlert(
        title = "Important Notice",
        paste("This is a", LAYOUT_TYPE, "layout example."),
        type = "info",
        dismissible = TRUE
      ),
      tablerCard(
        d3po_output("count_bar", height = "650px")
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
  )
)

server <- function(input, output, session) {
  d1 <- d3po::pokemon %>%
    mutate(
      type_2 = ifelse(is.na(!!sym("type_2")), "None", !!sym("type_2")),
      color_2 = ifelse(is.na(!!sym("color_2")), "#d3d3d3", !!sym("color_2"))
    ) %>%
    group_by(
      !!sym("type_1"),
      !!sym("color_1"),
      !!sym("type_2"),
      !!sym("color_2")
    ) %>%
    count() %>%
    ungroup()

  d2 <- d3po::pokemon %>%
    group_by(!!sym("type_1"), !!sym("color_1")) %>%
    count() %>%
    ungroup()

  output$count_tree <- render_d3po({
    d3po(d1) %>%
      po_treemap(
        daes(
          size = !!sym("n"),
          group = !!sym("type_1"),
          color = !!sym("color_1")
        )
      ) %>%
      po_title("Count of Pokemon by Type 1 and 2")
  })

  output$count_bar <- render_d3po({
    d3po(d2) %>%
      po_bar(
        daes(
          x = !!sym("type_1"),
          y = !!sym("n"),
          group = !!sym("type_1"),
          color = !!sym("color_1")
        )
      ) %>%
      po_title("Count of Pokemon by Type")
  })
}

# Run the app
if (interactive()) {
  shinyApp(ui, server)
}
