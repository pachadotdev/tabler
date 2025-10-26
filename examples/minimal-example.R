document()
load_all()

library(shiny)

svg_text <- paste(readLines("./examples/tabler-logo.svg", warn = FALSE), collapse = "\n")
svg_data_uri <- paste0("data:image/svg+xml;utf8,", URLencode(svg_text, reserved = TRUE))

ui <- tabler_page(
  theme = "light",
  title = "Boxed Dashboard",
  navbar = navbar_menu(
    brand = sidebar_brand(text = "", img = svg_data_uri, href = "./"),
    menu_item("Home", icon = "home"),
    menu_dropdown(
      "Layout",
      icon = "layout-2",
      href = "layout-boxed.html#navbar-layout",
      items = list(
        c("Boxed", "layout-boxed.html"),
        c("Combined", "./layout-combo.html"),
        c("Condensed", "./layout-condensed.html"),
        c("Fluid", "./layout-fluid.html"),
        c("Fluid vertical", "./layout-fluid-vertical.html"),
        c("Horizontal", "./layout-horizontal.html"),
        c("Navbar dark", "./layout-navbar-dark.html"),
        c("Navbar overlap", "./layout-navbar-overlap.html"),
        c("Navbar sticky", "./layout-navbar-sticky.html"),
        c("Right vertical", "./layout-vertical-right.html"),
        c("RTL mode", "./layout-rtl.html"),
        c("Vertical", "./layout-vertical.html"),
        c("Vertical transparent", "./layout-vertical-transparent.html")
      )
    )
  ),
  body = list(
    tabler_body(
      # todo add width parameter to tabler_card to control card width
      tabler_card("My title", "My text.")
    )
  ),
  footer = tabler_footer(
    left = "Tabler",
    right = shiny::tags$span("v1.4.0")
  )
)

writeLines(as.character(ui), "examples/minimal-example-boxed.html")

server <- function(input, output, session) {}

shinyApp(ui, server)
