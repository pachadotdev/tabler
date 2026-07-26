library(tabler)

# Pizza party planner ----
#
# One page, three tabs, showcasing most of inputs.R against a single
# "planning a pizza party" theme:
#   - Ingredients: all 12 sliderInput() theme colours (one per topping)
#   - Party details: numericInput, selectInput, dateInput (popup + inline
#     calendar), radioButtons, selectMultipleInput, checkboxGroupInput,
#     checkboxInput
#   - Contact & actions: textInput (with an input mask), actionButton, and a
#     gallery of components.R's button() variants (colour/outline/pill/
#     square/size/loading/disabled/icon/link), plus alert() and value_box()

# Approximate hex values of Tabler's 12 named theme colours, used only to
# colour the base-R shopping-list bar chart to match the sliders above it.
tabler_palette <- c(
  blue = "#206bc4", azure = "#4299e1", indigo = "#4263eb", purple = "#ae3ec9",
  pink = "#d6336c", red = "#d63939", orange = "#f76707", yellow = "#f59f00",
  lime = "#74b816", green = "#2fb344", teal = "#0ca678", cyan = "#17a2b8"
)

# Default topping quantities, one per Tabler theme colour
ingredient_defaults <- c(
  "Dough (kg)"          = 2,
  "Tomato sauce (dl)"   = 4,
  "Mozzarella (kg)"     = 1.5,
  "Parmesan (kg)"       = 0.3,
  "Pepperoni (slices)"  = 20,
  "Chili flakes (g)"    = 10,
  "Bell peppers (g)"    = 100,
  "Pineapple chunks (g)" = 0,
  "Basil leaves (count)" = 10,
  "Olives (g)"          = 50,
  "Mushrooms (g)"       = 80,
  "Onions (g)"          = 60
)

# Navbar ----
#
# The tab_item()s below are only reachable by clicking a menu_item() that
# targets their tab_name - without this navbar the "party" and "contact"
# tabs (calendars, buttons, text/phone inputs) would never become visible.
top_nav <- navbar_menu(
  menu_item("Ingredients", tab_name = "ingredients", icon = "pizza"),
  menu_item("Party details", tab_name = "party", icon = "calendar-event"),
  menu_item("Contact & actions", tab_name = "contact", icon = "phone")
)

# UI ----

ui <- page(
  theme = "light",
  color = "red",
  base = "slate",
  radius = 1,
  layout = "boxed",
  show_theme_button = TRUE,
  title = "Pizza Party Planner",
  navbar = top_nav,
  body = list(
    tab_items(
      tab_item(
        "ingredients",
        header(title = "Pizza ingredients", subtitle = "One slider colour per topping"),
        div(
          class = "page-body",
          div(
            class = "container-xl",
            row(
              col12(
                card(
                  title = "Ingredient sliders (all 12 theme colours)",
                  row(
                    col3(sliderInput("dough_kg", "Dough (kg)", min = 0, max = 5, value = 2, step = 0.5, color = "blue", fill = TRUE)),
                    col3(sliderInput("tomato_dl", "Tomato sauce (dl)", min = 0, max = 10, value = 4, color = "azure", fill = TRUE)),
                    col3(sliderInput("mozzarella_kg", "Mozzarella (kg)", min = 0, max = 5, value = 1.5, step = 0.1, color = "indigo", fill = TRUE)),
                    col3(sliderInput("parmesan_kg", "Parmesan (kg)", min = 0, max = 2, value = 0.3, step = 0.1, color = "purple", fill = TRUE))
                  ),
                  row(
                    col3(sliderInput("pepperoni_slices", "Pepperoni (slices)", min = 0, max = 40, value = 20, color = "pink", fill = TRUE)),
                    col3(sliderInput("chili_g", "Chili flakes (g)", min = 0, max = 50, value = 10, color = "red", fill = TRUE)),
                    col3(sliderInput("peppers_g", "Bell peppers (g)", min = 0, max = 300, value = 100, step = 10, color = "orange", fill = TRUE)),
                    col3(sliderInput("pineapple_g", "Pineapple chunks (g)", min = 0, max = 300, value = 0, step = 10, color = "yellow", fill = TRUE))
                  ),
                  row(
                    col3(sliderInput("basil_count", "Basil leaves (count)", min = 0, max = 30, value = 10, color = "lime", fill = TRUE)),
                    col3(sliderInput("olives_g", "Olives (g)", min = 0, max = 200, value = 50, step = 10, color = "green", fill = TRUE)),
                    col3(sliderInput("mushrooms_g", "Mushrooms (g)", min = 0, max = 300, value = 80, step = 10, color = "teal", fill = TRUE)),
                    col3(sliderInput("onions_g", "Onions (g)", min = 0, max = 200, value = 60, step = 10, color = "cyan", fill = TRUE))
                  )
                )
              )
            ),
            row(
              col8(
                card(
                  title  = "Shopping list preview",
                  footer = "Quantities update live as you move the sliders",
                  plotOutput("shopping_plot")
                )
              ),
              col4(
                card(
                  title = "Export",
                  p("Download the current quantities as a shopping list."),
                  downloadButton("shopping_download", label = "Download shopping list (CSV)")
                )
              )
            )
          )
        )
      ),
      tab_item(
        "party",
        header(title = "Party details", subtitle = "Who, when and how"),
        div(
          class = "page-body",
          div(
            class = "container-xl",
            row(
              col6(
                card(
                  title = "Guests & schedule",
                  numericInput("num_guests", "Number of guests", value = 8, min = 1, max = 50),
                  selectInput(
                    "planned_day", "Planned day",
                    choices  = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                    selected = "Saturday"
                  ),
                  dateInput("planned_date", "Planned date", value = Sys.Date() + 7, icon = "left"),
                  radioButtons("crust", "Crust type", choices = c("Thin", "Thick", "Stuffed"), inline = TRUE),
                  selectMultipleInput(
                    "extra_toppings", "Extra add-ons",
                    choices  = c("Extra cheese", "Garlic butter crust", "Truffle oil", "Spicy honey"),
                    selected = "Extra cheese"
                  ),
                  checkboxGroupInput("dietary", "Dietary options", choices = c("Vegetarian", "Vegan", "Gluten-free")),
                  checkboxInput("rsvp", "RSVP confirmed", value = TRUE, description = "All guests have replied")
                )
              ),
              col6(
                card(
                  title = "Pick the date on a calendar",
                  dateInput("planned_date_inline", NULL, value = Sys.Date() + 7, inline = TRUE)
                )
              )
            ),
            uiOutput("party_summary")
          )
        )
      ),
      tab_item(
        "contact",
        header(
          title = "Contact & actions", subtitle = "Confirm the order",
          header_actions = button("Call now", icon = "phone", color = "success")
        ),
        div(
          class = "page-body",
          div(
            class = "container-xl",
            row(
              col6(
                card(
                  title = "Call the customer",
                  textInput("customer_name", "Customer name", value = "Jane Doe"),
                  textInput("customer_phone", "Phone number", mask = "(00) 0000-0000"),
                  actionButton("call_customer", "Call customer", icon = "phone"),
                  tags$hr(),
                  verbatimTextOutput("call_log")
                )
              ),
              col6(
                uiOutput("kitchen_status")
              )
            ),
            row(
              col12(
                card(
                  title = "Button gallery (components.R button())",
                  div(
                    class = "d-flex flex-wrap gap-2",
                    button("Primary"),
                    button("Azure outline", outline = TRUE, color = "azure"),
                    button("Pink pill", pill = TRUE, color = "pink"),
                    button("Purple square", square = TRUE, color = "purple"),
                    button("Teal large", size = "lg", color = "teal"),
                    button("Orange small", size = "sm", color = "orange"),
                    button("Loading", loading = TRUE, color = "lime"),
                    button("Disabled", disabled = TRUE, color = "red"),
                    button("Download menu", icon = "download", color = "green"),
                    button("Visit site", href = "#", color = "cyan")
                  )
                )
              )
            ),
            row(
              col12(
                alert("Order confirmed - the pizzas are on their way!", type = "success", title = "Success", dismissible = TRUE)
              )
            ),
            row(
              col12(
                card(title = "Export order", downloadButton("order_download", label = "Download order summary (CSV)"))
              )
            )
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

# Server ----

server <- function(input, output, session) {
  # Current ingredient quantities, read live from the sliders. Called both
  # from a reactive context (renderPlot -> tracked) and a non-reactive one
  # (downloadHandler content -> just reads the current value).
  get_quantities <- function() {
    d <- ingredient_defaults
    stats::setNames(
      c(
        input$dough_kg %||% d[["Dough (kg)"]],
        input$tomato_dl %||% d[["Tomato sauce (dl)"]],
        input$mozzarella_kg %||% d[["Mozzarella (kg)"]],
        input$parmesan_kg %||% d[["Parmesan (kg)"]],
        input$pepperoni_slices %||% d[["Pepperoni (slices)"]],
        input$chili_g %||% d[["Chili flakes (g)"]],
        input$peppers_g %||% d[["Bell peppers (g)"]],
        input$pineapple_g %||% d[["Pineapple chunks (g)"]],
        input$basil_count %||% d[["Basil leaves (count)"]],
        input$olives_g %||% d[["Olives (g)"]],
        input$mushrooms_g %||% d[["Mushrooms (g)"]],
        input$onions_g %||% d[["Onions (g)"]]
      ),
      names(d)
    )
  }

  output$shopping_plot <- renderPlot({
    qty <- get_quantities()
    graphics::barplot(
      qty,
      col    = unname(tabler_palette),
      border = "white",
      las    = 2,
      main   = NULL,
      cex.names = 0.8
    )
  })

  output$shopping_download <- downloadHandler(
    filename = "shopping-list.csv",
    content  = function(file) {
      qty <- get_quantities()
      utils::write.csv(data.frame(ingredient = names(qty), quantity = qty), file, row.names = FALSE)
    }
  )

  output$party_summary <- renderUI({
    total_cheese <- (input$mozzarella_kg %||% 1.5) + (input$parmesan_kg %||% 0.3)
    days_away    <- as.integer(as.Date(input$planned_date %||% Sys.Date()) - Sys.Date())

    row(
      value_box(input$num_guests %||% 8, "Guests", icon = "users", color = "azure"),
      value_box(paste0(round(total_cheese, 1), " kg"), "Total cheese", icon = "cheese", color = "yellow"),
      value_box(days_away, "Days until the party", icon = "calendar-event", color = "teal")
    )
  })

  output$kitchen_status <- renderUI({
    if (isTRUE(input$rsvp)) {
      card(title = "Kitchen status", "RSVP confirmed - start cooking!", status = "success")
    } else {
      card(title = "Kitchen status", "Waiting for RSVP confirmation...", status = "warning")
    }
  })

  output$call_log <- renderText({
    n <- input$call_customer %||% 0
    if (n == 0) {
      "Press \"Call customer\" to place the call."
    } else {
      sprintf(
        "Calling %s at %s... (attempt %d)",
        input$customer_name %||% "the customer",
        input$customer_phone %||% "unknown number",
        n
      )
    }
  })

  output$order_download <- downloadHandler(
    filename = "order-summary.csv",
    content  = function(file) {
      order <- data.frame(
        field = c("customer_name", "customer_phone", "num_guests", "planned_day", "planned_date", "crust"),
        value = c(
          input$customer_name %||% "",
          input$customer_phone %||% "",
          input$num_guests %||% 8,
          input$planned_day %||% "Saturday",
          as.character(input$planned_date %||% Sys.Date()),
          input$crust %||% "Thin"
        )
      )
      utils::write.csv(order, file, row.names = FALSE)
    }
  )

  syncUrl(session, exclude = c("parameters", "to", "not", "show"))
}

tablerApp(ui, server)
