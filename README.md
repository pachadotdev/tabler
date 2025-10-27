<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R-CMD-check](https://github.com/pachadotdev/tabler/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pachadotdev/tabler/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/cpp4r)](https://CRAN.R-project.org/package=cpp4r)
[![Test
coverage](https://raw.githubusercontent.com/USER/REPO/coverage/badges/coverage.svg)](https://github.com/USER/REPO/actions/workflows/test-coverage.yaml)
[![BuyMeACoffee](https://raw.githubusercontent.com/pachadotdev/buymeacoffee-badges/main/bmc-blue.svg)](https://buymeacoffee.com/pacha)
<!-- badges: end -->

# Tabler Dashboard for Shiny

A modern dashboard framework for R Shiny using the beautiful Tabler
Bootstrap theme.

<iframe width="560" height="315" src="https://www.youtube.com/embed/_PWVmmis-AE?si=wJYMvUQUpoZz_k3_" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen></iframe>

## Installation

``` r
# using the R-Universe
install.packages("tabler", repos = "https://pachadotdev.r-universe.dev")

# or using the remotes package
remotes::install_github("pachadotdev/tabler")
```

## Quick Start

Please see the documentation: <https://pacha.dev/tabler/>

Here is a complete example that I use to test all layouts with the theme (light/dark) and colour options:
<https://github.com/pachadotdev/tabler/blob/main/examples/shiny-test-layouts.R>

Here’s a minimal example using the "combo" layout with sidebar and top navbar: 
<https://github.com/pachadotdev/tabler/blob/main/examples/shiny-test-combo.R>

``` r
if (!require("d3po")) {
  install.packages("d3po", repos = "https://pachadotdev.r-universe.dev")
}

library(shiny)

# library(tabler)
load_all()

library(d3po)

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

ui <- tabler_page(
  theme = "light",
  color = "teal",
  title = "Combo Layout",
  layout = "combo",
  show_theme_button = FALSE,
  navbar = main_navbar,
  body = list(
    # Page header
    page_header(
      title_text = "Combo Layout",
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

server <- function(input, output, session) {
  output$plot <- render_d3po({
    set.seed(123)

    sim <- data.frame(
      x = rnorm(100),
      y = rnorm(100),
      letter = sample(letters[1:3], 100, replace = TRUE)
    )

    # for light theme
    axis_color <- "#000"
    tooltip_color <- "#fff"

    # for dark theme
    axis_color <- "#fff"
    tooltip_color <- "#000"

    d3po(sim) %>%
      po_scatter(daes(x = x, y = y, group = letter)) %>%
      po_labels(title = "Weight Distribution by Type") %>%
      po_background("transparent") %>%
      po_theme(axis = axis_color, tooltips = tooltip_color)
  })
}

shinyApp(ui, server)
```

## Available Layouts

All examples use the same basic structure from the Quick Start example above. Here are the key differences for each
layout:

* **Boxed (Default)**: Basic dashboard with top navbar and constrained width content area. This
is the default layout.
* **Combo**:  Combines vertical sidebar navigation with top header.
* **Condensed**:  Compact layout with reduced padding/margins.
* **Fluid**:  Full-width layout without container constraints.
* **Fluid Vertical**: Full-width layout with vertical sidebar.
* **Horizontal**: Layout with horizontal navigation menu.
* **Navbar Dark**: Layout with dark navbar theme.
*  **Navbar Overlap**:  Layout where content overlaps with navbar for a modern look.
* **Navbar Sticky**: Layout with sticky/fixed navbar that stays at the top when scrolling.
* **RTL**: Right-to-left layout for Hebrew/Arabic languages.
* **Vertical**: Vertical sidebar layout without top navbar.
* **Vertical Right**: Vertical sidebar positioned on the right side.
* **Vertical Transparent**: Vertical layout with transparent sidebar.

## License

Apache License (\>= 2)
