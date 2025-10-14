library(shiny)
library(highcharter)

ui <- dashboardPageVertical(
  header = dashboardHeader(title = "Tabler Vertical Demo"),
  sidebar = tablerSidebarMenu(tablerMenuItem("Dashboard", tabName = "dashboard", icon = "home")),
  body = dashboardBody(tablerTabItems(tablerTabItem(tabName = "dashboard", highchartOutput("hc", height = "360px")))),
  footer = dashboardFooter(left = "© Vertical Demo")
)

server <- function(input, output, session) {
  output$hc <- renderHighchart({ highchart() %>% hc_add_series(data = sample(1:12)) })
}

shinyApp(ui = ui, server = server)
