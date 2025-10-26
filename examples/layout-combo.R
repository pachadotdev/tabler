document()
load_all()

library(shiny)
library(d3po)

svg_text <- paste(readLines("./examples/tabler-logo.svg", warn = FALSE), collapse = "\n")
svg_data_uri <- paste0("data:image/svg+xml;utf8,", URLencode(svg_text, reserved = TRUE))

ui <- tabler_page(
  theme = "light",
  title = "Combo Dashboard",
  layout = "combo",
  navbar = navbar_menu(
    brand = sidebar_brand(text = "", img = svg_data_uri, href = "./"),
    menu_item("Home", icon = "home"),
    menu_dropdown(
      "Layout",
      icon = "layout-2",
      href = "layout-boxed.html#navbar-layout",
      items = list(
        c("Boxed", "layout-boxed.html"),
        c("Combined", "./"),
        c("Condensed", "./"),
        c("Fluid", "./"),
        c("Fluid vertical", "./"),
        c("Horizontal", "./"),
        c("Navbar dark", "./"),
        c("Navbar overlap", "./"),
        c("Navbar sticky", "./"),
        c("Right vertical", "./"),
        c("RTL mode", "./"),
        c("Vertical", "./"),
        c("Vertical transparent", "./")
      )
    )
  ),
  body = list(
    tabler_body(
      # todo add width parameter to tabler_card to control card width
      column(6,
        tabler_card(
          title = "My title",
          footer = "Footer.",
          p("My text"),
          p("More text", class = "text-muted"),
          d3po_output("plot", width = "100%", height = "500px")
        )
      )
    )
  ),
  footer = tabler_footer(
    left = "Tabler",
    right = shiny::tags$span("v1.4.0")
  )
)

# remove text before "<body"
ui_string <- as.character(ui)
body_start <- regexpr("<body", ui_string)
ui_string <- substr(ui_string, body_start, nchar(ui_string))

# remove text after "</body>"
body_end <- regexpr("</body>", ui_string)
ui_string <- substr(ui_string, 1, body_end + attr(body_end, "match.length") - 1)

writeLines(as.character(ui_string), "examples/shiny/shiny-layout-boxed.html")

server <- function(input, output, session) {
  output$plot <- render_d3po({
    set.seed(123)

    sim <- data.frame(
      x = rnorm(100),
      y = rnorm(100),
      letter = sample(letters[1:3], 100, replace = TRUE)
    )

    d3po(sim) %>%
        po_scatter(daes(x = x, y = y, group = letter)) %>%
        po_labels(title = "Weight Distribution by Type")
  })
}

shinyApp(ui, server)
