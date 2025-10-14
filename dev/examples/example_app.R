# Example of the new simplified tabler package structure
library(shiny)
# library(tabler)
load_all()

ui <- tabler_page(
  title = "My Tabler Dashboard",
  
  navbar = tabler_navbar(
    title = "My App"
  ),
  
  body = tabler_body(
    fluidRow(
      tabler_value_box(
        value = "1,352",
        title = "Total Users",
        icon = "users",
        color = "primary",
        width = 3
      ),
      tabler_value_box(
        value = "87.5%",
        title = "Success Rate", 
        icon = "trending-up",
        color = "success",
        width = 3
      ),
      tabler_value_box(
        value = "24",
        title = "Active Projects",
        icon = "folder",
        color = "warning",
        width = 3
      ),
      tabler_value_box(
        value = "$12,345",
        title = "Revenue",
        icon = "currency-dollar",
        color = "info",
        width = 3
      )
    ),
    
    fluidRow(
      column(8,
        tabler_card(
          title = "Sample Chart",
          plotOutput("plot"),
          status = "primary"
        )
      ),
      column(4,
        tabler_card(
          title = "Progress Status",
          tabler_progress(75, color = "success", label = TRUE),
          br(),
          tabler_progress(45, color = "warning", label = TRUE),
          br(),
          tabler_progress(90, color = "info", label = TRUE)
        ),
        
        tabler_alert(
          title = "Important Notice",
          "This is a sample alert message.",
          type = "info",
          dismissible = TRUE
        )
      )
    )
  ),
  
  footer = tabler_footer(
    left = "© 2024 My Company",
    right = "Built with Tabler"
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
