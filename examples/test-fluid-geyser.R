library(tabler)

ui <- page(
  layout = "fluid",
  title  = "Old Faithful Geyser Data",
  navbar = navbar_menu(
    menu_item("Home", icon = "home")
  ),
  body = list(
    header(title = "Old Faithful Geyser Data", subtitle = "Histogram"),
    body(
      column(
        4,
        card(
          title = "Controls",
          sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30)
        )
      ),
      column(
        8,
        card(
          title  = "Output",
          footer = "Eruption duration (minutes)",
          plotOutput("distPlot")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  output$distPlot <- renderPlot({
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    x |>
      hist(
        breaks = bins,
        col    = "darkgray",
        border = "white",
        main   = NULL,
        xlab   = "Eruption duration (min)"
      )
  })
}

tablerApp(ui, server)
