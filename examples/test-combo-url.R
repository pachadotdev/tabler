library(tabler)

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
      div(
        class = "row",
        # Left column: controls ----
        column(
          4,
          card(
            title = "Controls",
            selectInput(
              "dataset", "Dataset",
              choices = c("mtcars", "iris", "airquality"),
              selected = "mtcars"
            ),
            sliderInput(
              "n_rows", "Rows to display",
              min = 1, max = 30, value = 10
            ),
            sliderInput(
              "multiplier", "Value multiplier",
              min = 0.5, max = 5, value = 1, step = 0.5
            ),
            textInput(
              "label", "Dashboard label",
              value = "My Dashboard"
            ),
            checkboxInput(
              "show_cols", "Show column names",
              value = FALSE
            ),
            radioButtons(
              "stat", "Summary statistic",
              choices = c("Mean" = "mean", "Median" = "median", "SD" = "sd")
            ),
            actionButton("refresh", "Recalculate", icon = "refresh")
          )
        ),
        
        # Right column: outputs ----
        column(
          8,
          card(
            title = uiOutput("card_title"),
            textOutput("summary_text")
          ),
          card(
            title = "Computed statistic",
            textOutput("computed")
          ),
          card(
            title = "Data preview",
            verbatimTextOutput("data_preview")
          )
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

  # Reactive dataset — re-evaluates only when input$dataset changes
  current_data <- reactive({
    switch(input$dataset %||% "mtcars",
      mtcars     = mtcars,
      iris       = iris,
      airquality = airquality
    )
  })

  # Dynamic card title reflects the chosen label
  output$card_title <- renderUI({
    lbl <- input$label %||% "Dataset summary"
    span(lbl, class = "text-teal")
  })

  # One-line summary
  output$summary_text <- renderText({
    df <- current_data()
    n  <- min(input$n_rows %||% 10L, nrow(df))
    paste0(
      input$dataset %||% "—", ": ",
      nrow(df), " rows × ", ncol(df), " cols.",
      " Showing top ", n, "."
    )
  })

  # Apply the chosen statistic across numeric columns, scaled by multiplier
  stat_val <- reactive({
    df       <- current_data()
    num_cols <- df[, vapply(df, is.numeric, logical(1L)), drop = FALSE]
    if (ncol(num_cols) == 0L) return(NA_real_)

    fn <- switch(input$stat %||% "mean",
      mean   = function(x) mean(x, na.rm = TRUE),
      median = function(x) stats::median(x, na.rm = TRUE),
      sd     = function(x) stats::sd(x, na.rm = TRUE)
    )
    round(mean(vapply(num_cols, fn, numeric(1L))) * (input$multiplier %||% 1), 3L)
  })

  output$computed <- renderText({
    val  <- stat_val()
    cols <- if (isTRUE(input$show_cols)) {
      df <- current_data()
      nm <- names(df[, vapply(df, is.numeric, logical(1L)), drop = FALSE])
      paste0("  [", paste(nm, collapse = ", "), "]")
    } else ""
    paste0(input$stat %||% "mean", " of column ", input$stat %||% "mean",
           "s × multiplier = ", val, cols)
  })

  # Verbatim data preview — number of rows driven by slider
  output$data_preview <- renderPrint({
    df <- current_data()
    n  <- min(input$n_rows %||% 10L, nrow(df))
    head(df, n)
  })

  # Action button: recalculate forces stat_val to reprint (it's already reactive,
  # but this shows observeEvent wiring)
  click_count <- reactiveVal(0L)
  observeEvent(input$refresh, {
    click_count(click_count() + 1L)
    message("Recalculate clicked (", click_count(), " times)")
  })

  # URL sync — action buttons are always excluded automatically
  syncUrl(session, exclude = "label")
}

tablerApp(ui, server)
