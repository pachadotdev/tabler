# change layout index
i <- 8
show_theme_button <- T

document()
load_all()

library(shiny)
library(d3po)

svg_text <- paste(
  readLines("./examples/tabler-logo.svg", warn = FALSE),
  collapse = "\n"
)
svg_data_uri <- paste0(
  "data:image/svg+xml;utf8,",
  URLencode(svg_text, reserved = TRUE)
)

layouts <- c(
  "boxed",
  "combo",
  "condensed",
  "fluid-vertical",
  "fluid",
  "horizontal",
  "navbar-dark",
  "navbar-overlap",
  "navbar-sticky",
  "rtl",
  "vertical-right",
  "vertical-transparent",
  "vertical"
)

colors <- c(
  "blue",
  "azure",
  "indigo",
  "purple",
  "pink",
  "red",
  "orange",
  "yellow",
  "lime",
  "green",
  "teal",
  "cyan"
)

themes <- c("light", "dark")

# for (i in seq_along(layouts)) {
my_layout <- layouts[i]

print(my_layout)

my_color <- colors[11]
my_theme <- themes[1]

# For combo layout, we need BOTH sidebar menu AND simplified top navbar
if (my_layout == "combo") {
  # Full menu for the sidebar
  sidebar_nav <- navbar_menu(
    brand = sidebar_brand(text = "", img = svg_data_uri, href = "./"),
    menu_item("Home", icon = "home"),
    menu_dropdown(
      "Layout",
      icon = "layout-2",
      href = "./",
      items = list(
        c("Boxed", "./"),
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
} else {
  # Other layouts use a single navbar
  main_navbar <- navbar_menu(
    brand = sidebar_brand(text = "", img = svg_data_uri, href = "./"),
    menu_item("Home", icon = "home"),
    menu_dropdown(
      "Layout",
      icon = "layout-2",
      href = "./",
      items = list(
        c("Boxed", "./"),
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
  )
}

ui <- tabler_page(
  theme = my_theme,
  color = my_color,
  title = paste(toupper(my_layout), "Layout"),
  layout = my_layout,
  show_theme_button = show_theme_button,
  navbar = main_navbar,
  body = list(
    # Page header
    page_header(
      title_text = paste(toupper(my_layout), "layout"),
      pretitle_text = "Overview"
    ),
    # Page body content
    shiny::tags$div(
      class = "page-body",
      shiny::tags$div(
        class = "container-xl",
        column(
          6,
          tabler_card(
            title = "My title",
            footer = "Footer.",
            p("My text"),
            p("More text", class = "text-muted"),
            d3po_output("plot", width = "100%", height = "500px")
          )
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
ui_string <- substr(
  ui_string,
  1,
  body_end + attr(body_end, "match.length") - 1
)

writeLines(
  as.character(ui_string),
  sprintf("examples/shiny/shiny-layout-%s.html", my_layout)
)

server <- function(input, output, session) {
  output$plot <- render_d3po({
    set.seed(123)

    sim <- data.frame(
      x = rnorm(100),
      y = rnorm(100),
      letter = sample(letters[1:3], 100, replace = TRUE)
    )

    if (my_theme == "light") {
      axis_color <- "#000"
      tooltip_color <- "#fff"
    } else {
      axis_color <- "#fff"
      tooltip_color <- "#000"
    }

    d3po(sim) %>%
      po_scatter(daes(x = x, y = y, group = letter)) %>%
      po_labels(title = "Weight Distribution by Type") %>%
      po_background("transparent") %>%
      po_theme(axis = axis_color, tooltips = tooltip_color)
  })
}
# }

# shinyApp(ui, server)
