# Tabler for R

<!-- badges: start -->
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R-CMD-check](https://github.com/pachadotdev/tabler/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pachadotdev/tabler/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/tabler)](https://CRAN.R-project.org/package=tabler)
[![Test
coverage](https://raw.githubusercontent.com/pachadotdev/tabler/coverage/badges/coverage.svg)](https://github.com/pachadotdev/tabler/actions/workflows/test-coverage.yaml)
[![BuyMeACoffee](https://raw.githubusercontent.com/pachadotdev/buymeacoffee-badges/main/bmc-blue.svg)](https://buymeacoffee.com/pacha)
<!-- badges: end -->

A modern dashboard framework for R using the beautiful Tabler Bootstrap theme. To render Tabler apps using a server, see [Tabler Server](https://github.com/pachadotdev/tabler-server).

<iframe width="560" height="315" src="https://www.youtube.com/embed/_PWVmmis-AE?si=wJYMvUQUpoZz_k3_" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen>

</iframe>

## Installation

Old version, depends on Shiny:

```r
install.packages("tabler", repos = "https://cran.r-project.org")
```

New version, does not use Shiny:

```r
# using the R-Universe
install.packages("tabler", repos = "https://pachadotdev.r-universe.dev")

# or using the remotes package
remotes::install_github("pachadotdev/tabler")
```

## Quick Start

Please see the documentation: <https://pacha.dev/tabler/>.

### Single-script app

The following example uses the "combo" layout to recreate Shiny's geyser example. The theme options
can be adjusted from the code or the theme setting icon that can be hidden.

<figure>
<img src="combo-layout-light.png" title="Light theme + teal colour + zinc base"
alt="layout-geyser" />
</figure>

<figure>
<img src="combo-layour-dark.png" title="Dark theme + cyan colour + slate base"
alt="layout-geyser" />
</figure>

```r
library(tabler)

# Sidebar/topbar ----

svg_data_uri <- paste0(
  "data:image/svg+xml;utf8,",
  URLencode(
    paste(
      readLines(
        system.file("extdata", "app-template", "tabler-logo.svg", package = "tabler"),
        warn = FALSE
      ),
      collapse = "\n"
    ),
    reserved = TRUE
  )
)

sidebar_nav <- navbar_menu(
  brand = sidebar_brand(text = "", img = svg_data_uri, href = "./"),
  menu_item("Mtcars", tab_name = "mtcars", icon = "car"),
  menu_item("Iris", tab_name = "iris", icon = "flower"),
  menu_item("Airquality", tab_name = "airquality", icon = "cloud")
)

topbar_nav <- navbar_menu(
  menu_item("Tabler docs", icon = NULL, href = "https://tabler.io/admin-template/preview"),
  menu_item("Tabler R docs", icon = NULL, href = "https://pacha.dev/tabler"),
  menu_item("Tabler Server docs", icon = NULL, href = "https://pacha.dev/tabler-server"),
  menu_dropdown(
    "Another button",
    icon = "layout-2",
    href = "./",
    items = list(
      c("Placeholder", "./")
    )
  )
)

# Histograms ----

# Helper to build one section: a controls card + a histogram output card,
histogram_section <- function(title, subtitle, col_input_id, col_choices, col_selected,
                               bins_input_id, plot_output_id, download_output_id) {
  list(
    header(title = title, subtitle = subtitle),
    div(
      class = "page-body",
      div(
        class = "container-xl",
        row(
          col4(
            card(
              title = "Controls",
              selectInput(col_input_id, "Column", choices = col_choices, selected = col_selected),
              sliderInput(bins_input_id, "Number of bins:", min = 1, max = 10, value = 5),
              downloadButton(download_output_id, label = "Download CSV")
            )
          ),
          col8(
            card(
              title  = "Output",
              footer = "Histogram",
              plotOutput(plot_output_id)
            )
          )
        )
      )
    )
  )
}

# UI ----

ui <- page(
  theme = "light",
  color = "teal",
  base = "slate",
  radius = 1,
  # layout = "fluid-vertical",
  layout = "combo",
  show_theme_button = TRUE,
  title = "Combo Layout",
  navbar = list(side = sidebar_nav, top = topbar_nav),
  body = list(
    tab_items(
      tab_item(
        "mtcars",
        histogram_section(
          "Mtcars", "Histogram",
          "mtcars_col", c("mpg", "hp", "wt", "qsec", "disp", "drat"), "mpg",
          "mtcars_bins", "mtcars_plot", "mtcars_download"
        )
      ),
      tab_item(
        "iris",
        histogram_section(
          "Iris", "Histogram",
          "iris_col", c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"), "Sepal.Length",
          "iris_bins", "iris_plot", "iris_download"
        )
      ),
      tab_item(
        "airquality",
        histogram_section(
          "Airquality", "Histogram",
          "airquality_col", c("Ozone", "Solar.R", "Wind", "Temp"), "Temp",
          "airquality_bins", "airquality_plot", "airquality_download"
        )
      )
    )
  ),
  footer = footer(
    left = "Tabler",
    right = tags$span("v1.4.0")
  )
)

# Server ----

server <- function(input, output, session) {
  # Generic histogram renderer, shared across the three sections
  render_histogram <- function(data, col_reactive, bins_reactive) {
    renderPlot({
      x    <- stats::na.omit(data[[col_reactive()]])
      bins <- seq(min(x), max(x), length.out = bins_reactive() + 1)
      x |>
        hist(
          breaks = bins,
          col    = "darkgray",
          border = "white",
          main   = NULL,
          xlab   = col_reactive()
        )
    })
  }

  output$mtcars_plot <- render_histogram(
    mtcars,
    reactive(input$mtcars_col %||% "mpg"),
    reactive(input$mtcars_bins %||% 5)
  )

  output$iris_plot <- render_histogram(
    iris,
    reactive(input$iris_col %||% "Sepal.Length"),
    reactive(input$iris_bins %||% 5)
  )

  output$airquality_plot <- render_histogram(
    airquality,
    reactive(input$airquality_col %||% "Temp"),
    reactive(input$airquality_bins %||% 5)
  )

  # Download handlers — export the full underlying dataset as CSV
  output$mtcars_download <- downloadHandler(
    filename = "mtcars.csv",
    content  = function(file) utils::write.csv(mtcars, file, row.names = TRUE)
  )

  output$iris_download <- downloadHandler(
    filename = "iris.csv",
    content  = function(file) utils::write.csv(iris, file, row.names = FALSE)
  )

  output$airquality_download <- downloadHandler(
    filename = "airquality.csv",
    content  = function(file) utils::write.csv(airquality, file, row.names = FALSE)
  )

  syncUrl(session, exclude = c("parameters", "to", "not", "show"))
}

tablerApp(ui, server)
```

### Modular R package app

Create a package template with:

```
library(tabler)

pkg_template("mydashboard")
```

## Available Layouts

I added [full examples](https://github.com/pachadotdev/tabler/tree/main/inst/extdada) showing how to use 'Tabler' as a
single-script app or as an R package organized in modules.

There are [additional examples](https://github.com/pachadotdev/tabler/tree/main/examples) for each of the following layouts:

- **Boxed (Default)**: Basic dashboard with top navbar and constrained
  width content area. This is the default layout.
- **Combo**: Combines vertical sidebar navigation with top header.
- **Condensed**: Compact layout with reduced padding/margins.
- **Fluid**: Full-width layout without container constraints.
- **Fluid Vertical**: Full-width layout with vertical sidebar.
- **Horizontal**: Layout with horizontal navigation menu.
- **Navbar Dark**: Layout with dark navbar theme.
- **Navbar Overlap**: Layout where content overlaps with navbar for a
  modern look.
- **Navbar Sticky**: Layout with sticky/fixed navbar that stays at the
  top when scrolling.
- **RTL**: Right-to-left layout for Hebrew/Arabic languages.
- **Vertical**: Vertical sidebar layout without top navbar.
- **Vertical Right**: Vertical sidebar positioned on the right side.
- **Vertical Transparent**: Vertical layout with transparent sidebar.

Note: `tabler` allows to pass `layout = "navbar"` and `layout = "navbar-sticky-dark"` which are wrappers
for a light theme navbar layout and a dark theme sticky navbar layour, respectively.

## Differences with Shiny

- Static plots (base, ggplot, tinyplot, etc.) render as SVG and can be downloaded with the right click button.
- URLs are of the form `my.site/myapp?year=2000&country=gbr` instead of `my.site/myapp?year=2000&country=%22gbr%22` 

## License

Apache License (\>= 2)
