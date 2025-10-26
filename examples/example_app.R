library(shiny)
library(highcharter)
library(dplyr)
library(palmerpenguins)

document()
load_all()

if (interactive()) {
  shiny::addResourcePath("www", file.path("examples", "tablerdemo", "inst", "app", "www"))
}

# Choose which layout to test
LAYOUT_TYPES <- c(
  "boxed",
  "combo",
  "condensed",
  "fluid",
  "fluid-vertical", # 5
  "horizontal",
  "navbar-dark",
  "navbar-overlap",
  "navbar-sticky",
  "rtl", # 10
  "vertical",
  "vertical-right",
  "vertical-transparent"
)

LAYOUT_TYPE <- LAYOUT_TYPES[1]

# Shared UI pieces: two tabs only to keep the example minimal
dashboard_body <- tabler_body(
  tabler_page_header(title = "Tabler minimal demo", subtitle = "Penguins + mtcars"),
  tabler_tab_items(
    tabler_tab_item(
      tab_name = "welcome",
      fluidRow(
        column(8, tabler_card(
          "Welcome",
          shiny::tags$p("This is the welcome tab for the Tabler minimal demo.")
        )),
        column(4, tabler_card("Here is another text", NULL))
      )
    ),
    tabler_tab_item(
      tab_name = "penguins",
      fluidRow(
        column(8, tabler_card("Body Mass by Species", highchartOutput("mass_boxplot"))),
        column(4, tabler_card("Species Distribution", highchartOutput("species_pie")))
      ),
      fluidRow(
        column(6, tabler_card("Flipper vs Mass", highchartOutput("scatter_plot"))),
        column(6, tabler_card("Bill by Island", highchartOutput("bill_column")))
      )
    ),
    tabler_tab_item(
      tab_name = "mtcars",
      fluidRow(
        column(8, tabler_card("MPG by Cylinders", highchartOutput("mpg_boxplot"))),
        column(4, tabler_card("Transmission", highchartOutput("transmission_pie")))
      ),
      fluidRow(
        column(6, tabler_card("Weight vs MPG", highchartOutput("weight_scatter"))),
        column(6, tabler_card("HP by Cylinders", highchartOutput("hp_column")))
      )
    )
  )
)

# Build a simple page with a sidebar menu
ui <- tabler_page(
  title = "Tabler minimal",
  layout = LAYOUT_TYPE,
  navbar = sidebar_menu(
    title = sidebar_brand(img = "www/logo.svg", text = "Tabler demo"),
    # title = "Example App",
    menu_item("Welcome", tab_name = "welcome", icon = "home"),
    menu_item("Penguins", tab_name = "penguins", icon = "paw"),
    menu_item("MTCars", tab_name = "mtcars", icon = "car")
  ),
  body = dashboard_body,
  footer = tabler_footer(left = "Demo", right = "Minimal examples")
)

# Helper: color palette for species/cylinders
pal <- c("#4299e1", "#f56565", "#48bb78")

# Server: use small shared helpers to reduce repetition
server <- function(input, output, session) {
  penguins_clean <- na.omit(palmerpenguins::penguins)

  output$mass_boxplot <- renderHighchart({
    d <- data_to_boxplot(penguins_clean, body_mass_g, species)
    highchart() %>%
      hc_add_series_list(d) %>%
      hc_title(text = NULL) %>%
      hc_legend(enabled = FALSE)
  })

  output$species_pie <- renderHighchart({
    tbl <- penguins_clean %>% count(species)
    highchart() %>%
      hc_chart(type = "pie") %>%
      hc_add_series(data = tbl, type = "pie", hcaes(name = species, y = n)) %>%
      hc_colors(pal)
  })

  output$scatter_plot <- renderHighchart({
    # grouped scatter using species colors; build single highchart and add series in a loop
    hc <- highchart() %>% hc_chart(type = "scatter")
    species <- unique(penguins_clean$species)
    for (i in seq_along(species)) {
      sp <- species[i]
      df <- penguins_clean %>% filter(species == sp)
      hc <- hc %>% hc_add_series(data = df, type = "scatter", hcaes(x = flipper_length_mm, y = body_mass_g), name = sp, color = pal[((i - 1) %% length(pal)) + 1])
    }
    hc %>%
      hc_title(text = NULL) %>%
      hc_xAxis(title = list(text = "Flipper Length (mm)")) %>%
      hc_yAxis(title = list(text = "Body Mass (g)"))
  })

  output$bill_column <- renderHighchart({
    bill <- penguins_clean %>%
      group_by(island, species) %>%
      summarise(avg = mean(bill_length_mm, na.rm = TRUE), .groups = "drop")
    hc <- highchart() %>%
      hc_chart(type = "column") %>%
      hc_xAxis(categories = unique(bill$island))
    species <- unique(bill$species)
    for (i in seq_along(species)) {
      sp <- species[i]
      df <- bill %>% filter(species == sp)
      hc <- hc %>% hc_add_series(data = df, type = "column", hcaes(x = island, y = avg), name = sp, color = pal[((i - 1) %% length(pal)) + 1])
    }
    hc %>%
      hc_title(text = NULL) %>%
      hc_yAxis(title = list(text = "Average Bill Length (mm)"))
  })

  # mtcars
  output$mpg_boxplot <- renderHighchart({
    mt2 <- mtcars %>% mutate(cyl = as.factor(cyl))
    d <- data_to_boxplot(mt2, mpg, cyl)
    highchart() %>% hc_add_series_list(d)
  })

  output$transmission_pie <- renderHighchart({
    t <- mtcars %>%
      mutate(trans = ifelse(am == 0, "Auto", "Manual")) %>%
      count(trans)
    highchart() %>%
      hc_chart(type = "pie") %>%
      hc_add_series(data = t, type = "pie", hcaes(name = trans, y = n))
  })

  output$weight_scatter <- renderHighchart({
    highchart() %>%
      hc_chart(type = "scatter") %>%
      hc_add_series(data = mtcars, type = "scatter", hcaes(x = wt, y = mpg), name = "cars")
  })

  output$hp_column <- renderHighchart({
    hp <- mtcars %>%
      mutate(cyl = as.factor(cyl)) %>%
      group_by(cyl) %>%
      summarise(avg_hp = mean(hp, na.rm = TRUE), .groups = "drop")
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_xAxis(categories = hp$cyl) %>%
      hc_add_series(data = hp, type = "column", hcaes(x = cyl, y = avg_hp))
  })
}

if (interactive()) shinyApp(ui, server)
