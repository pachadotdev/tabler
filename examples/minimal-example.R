load_all()

ui <- tabler_page(
  title = "Combo Dashboard",
  layout = "combo",
  theme = "dark",
  color = "teal",
  navbar = list(
    top = topbar(title = "My App"),
    side = sidebar_menu(
      menu_item("Dogs", icon = "dog"),
      menu_item("Cats", icon = "cat")
    )
  ),
  body = column(
    12,
    tabler_body("Welcome to Tabler!"),
    actionButton("btn", "Click me")
  )
)

server <- function(input, output, session) {}

shiny::shinyApp(ui, server)
