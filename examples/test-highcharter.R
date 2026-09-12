if (!require("highcharter")) {
  install.packages("highcharter")
}

library(tabler)
library(highcharter)

svg_text <- paste(
  readLines("./examples/tabler-logo.svg", warn = FALSE),
  collapse = "\n"
)

svg_data_uri <- paste0(
  "data:image/svg+xml;utf8,",
  URLencode(svg_text, reserved = TRUE)
)

# Full menu for the sidebar
sidebar_nav <- navbar_menu(
  brand = sidebar_brand(text = "", img = svg_data_uri, href = "./"),
  menu_item("Home", icon = "home"),
  menu_dropdown(
    "Layout",
    icon = "layout-2",
    href = "./",
    items = list(c("Item 1", "./"))
  )
)

# Simplified menu for the top navbar (just labels, no icons for simplicity)
top_nav <- navbar_menu(
  menu_item("Button 1", icon = NULL),
  menu_dropdown(
    "Button 2",
    icon = "layout-2",
    href = "./",
    items = list(
      c("Button 3", "./")
    )
  )
)

# Combine both for combo layout
main_navbar <- list(side = sidebar_nav, top = top_nav)

ui <- page(
  theme = "light",
  color = "teal",
  title = "Combo Layout",
  layout = "combo",
  show_theme_button = FALSE,
  navbar = main_navbar,
  body = list(
    # Page header
    header(
      title = "Combo Layout",
      subtitle = "Overview"
    ),
    # Page body content
    body(
      col12(
        card(
          title = "My title",
          footer = "Footer.",
          p("My text"),
          p("More text", class = "text-muted"),
          highchartOutput("plot", width = "100%", height = "500px")
        )
      )
    )
  ),
  footer = footer(
    left = "Tabler",
    right = tags$span("v1.4.0")
  )
)

server <- function(input, output, session) {
  output$plot <- renderHighchart({
    set.seed(123)

    hchart(mtcars, "point", hcaes(x = disp, y = mpg, group = am))
  })
}

tabler_app(ui, server)
