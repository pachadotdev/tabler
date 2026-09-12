library(tabler)

# Inputs showcase --
#
# Pure UI, no server logic - just every input widget from inputs.R side by
# side with its main variants, so you can see what each option looks like:
#   - Select: plain <select> vs searchable "selectize" style, grouped choices
#   - Select multiple (tag-style, searchable)
#   - Slider: single vs range, with vs without fill, a few theme colours
#   - Date: popup (icon left/right/none) vs always-visible inline calendar
#   - Radio buttons: stacked vs inline
#   - Checkbox group, single checkbox (block vs inline), text (with mask),
#     numeric, action button

# Navbar --
#
# The tab_item()s below are only reachable by clicking a menu_item() that
# targets their tab_name.
top_nav <- navbar_menu(
  menu_item("Select & multi-select", tab_name = "selects", icon = "list"),
  menu_item("Sliders", tab_name = "sliders", icon = "adjustments"),
  menu_item("Dates", tab_name = "dates", icon = "calendar-event"),
  menu_item("Radio & checkboxes", tab_name = "choices", icon = "checkbox"),
  menu_item("Text, numeric & buttons", tab_name = "misc", icon = "forms"),
  menu_item("Flags & icons", tab_name = "decor", icon = "library-photo")
)

# UI --

ui <- page(
  theme = "light",
  color = "red",
  base = "slate",
  radius = 1,
  layout = "boxed",
  show_theme_button = TRUE,
  title = "Inputs Showcase",
  navbar = top_nav,
  body = list(
    tab_items(
      tab_item(
        "selects",
        header(title = "Select & multi-select", subtitle = "Plain vs searchable dropdowns"),
        div(
          class = "page-body",
          div(
            class = "container-xl",
            row(
              col6(
                card(
                  title = "select_input() - searchable (selectize style)",
                  select_input(
                    "country_selectize", "Country",
                    choices  = c("Chile", "Peru", "Colombia"),
                    selected = "Chile"
                  )
                )
              ),
              col6(
                card(
                  title = "select_input() - non-searchable (plain native select)",
                  select_input(
                    "country_plain", "Country",
                    choices    = c("Chile", "Peru", "Colombia"),
                    selected   = "Chile",
                    searchable = FALSE
                  )
                )
              )
            ),
            row(
              col6(
                card(
                  title = "select_input() - grouped choices",
                  select_input(
                    "country_grouped", "Country",
                    choices = list(
                      "South America" = c("Chile", "Peru", "Colombia"),
                      "Europe"        = c("United Kingdom", "Spain", "France")
                    )
                  )
                )
              ),
              col6(
                card(
                  title = "select_multiple_input() - searchable, tag-style multi-select",
                  select_multiple_input(
                    "country_tag", "Country",
                    choices  = c("Chile", "Peru", "Colombia"),
                    selected = "Chile"
                  )
                )
              )
            )
          )
        )
      ),
      tab_item(
        "sliders",
        header(title = "Sliders", subtitle = "Single vs range, with vs without fill"),
        div(
          class = "page-body",
          div(
            class = "container-xl",
            row(
              col6(
                card(
                  title = "Single value - no fill",
                  sliderInput("single_nofill", "Value", min = 0, max = 100, value = 40)
                )
              ),
              col6(
                card(
                  title = "Single value - fill -  blue",
                  sliderInput("single_fill", "Value", min = 0, max = 100, value = 40, fill = TRUE, color = "blue")
                )
              )
            ),
            row(
              col6(
                card(
                  title = "Range (two thumbs) - no fill",
                  sliderInput("range_nofill", "Year range", min = 2000, max = 2025, value = c(2010, 2020))
                )
              ),
              col6(
                card(
                  title = "Range (two thumbs) - fill - teal",
                  sliderInput("range_fill", "Year range", min = 2000, max = 2025, value = c(2010, 2020), fill = TRUE, color = "teal")
                )
              )
            ),
            row(
              col12(
                card(
                  sliderInput("c_purple", "Purple - thumbSize 2", min = 0, max = 10, value = 6, color = "purple", fill = TRUE, thumbSize = 2),
                  sliderInput("c_azure", "Azure", min = 0, max = 10, value = 3, color = "azure", fill = TRUE),
                  sliderInput("c_indigo", "Indigo", min = 0, max = 10, value = 3, color = "indigo", fill = TRUE),
                  sliderInput("c_red", "Red", min = 0, max = 10, value = 3, color = "red", fill = TRUE),
                  sliderInput("c_orange", "Orange", min = 0, max = 10, value = 3, color = "orange", fill = TRUE),
                  sliderInput("c_yellow", "Yellow", min = 0, max = 10, value = 3, color = "yellow", fill = TRUE),
                  sliderInput("c_lime", "Lime", min = 0, max = 10, value = 8, color = "lime", fill = TRUE),
                  sliderInput("c_pink", "Pink", min = 0, max = 10, value = 5, color = "pink", fill = TRUE)
                )
              )
            )
          )
        )
      ),
      tab_item(
        "dates",
        header(title = "Dates", subtitle = "Popup field vs inline calendar"),
        div(
          class = "page-body",
          div(
            class = "container-xl",
            row(
              col4(
                card(
                  title = "dateInput() - no icon",
                  dateInput("date_none", "Date", value = Sys.Date())
                )
              ),
              col4(
                card(
                  title = "dateInput() - icon to the left",
                  dateInput("date_left", "Date", value = Sys.Date(), icon = "left")
                )
              ),
              col4(
                card(
                  title = "dateInput() - icon to the right",
                  dateInput("date_right", "Date", value = Sys.Date(), icon = "right")
                )
              )
            ),
            row(
              col12(
                card(
                  title = "dateInput() - inline (always-visible calendar)",
                  dateInput("date_inline", NULL, value = Sys.Date(), inline = TRUE)
                )
              )
            )
          )
        )
      ),
      tab_item(
        "choices",
        header(title = "Radio & checkboxes", subtitle = "Stacked vs inline layout"),
        div(
          class = "page-body",
          div(
            class = "container-xl",
            row(
              col6(
                card(
                  title = "radio_buttons() - no inline (default, stacked)",
                  radio_buttons("crust_stacked", "Crust type", choices = c("Thin", "Thick", "Stuffed"))
                )
              ),
              col6(
                card(
                  title = "radio_buttons() - inline",
                  radio_buttons("crust_inline", "Crust type", choices = c("Thin", "Thick", "Stuffed"), inline = TRUE)
                )
              )
            ),
            row(
              col6(
                card(
                  title = "checkbox_group_input()",
                  checkbox_group_input("dietary", "Dietary options", choices = c("Vegetarian", "Vegan", "Gluten-free"), selected = "Vegetarian")
                )
              ),
              col6(
                card(
                  title = "checkbox_input() - block vs inline",
                  checkbox_input("rsvp1", "RSVP confirmed", value = TRUE, description = "Block layout (default)"),
                  div(
                    class = "d-flex gap-3",
                    checkbox_input("opt_a", "Option A", inline = TRUE),
                    checkbox_input("opt_b", "Option B", value = TRUE, inline = TRUE),
                    checkbox_input("opt_c", "Option C", inline = TRUE)
                  )
                )
              )
            )
          )
        )
      ),
      tab_item(
        "misc",
        header(title = "Text, numeric & buttons", subtitle = "Remaining inputs.R widgets"),
        div(
          class = "page-body",
          div(
            class = "container-xl",
            row(
              col4(
                card(
                  title = "text_input() - plain",
                  text_input("customer_name", "Customer name", value = "Jane Doe")
                )
              ),
              col4(
                card(
                  title = "text_input() - with mask",
                  text_input("customer_phone", "Phone number", mask = "(000) 0000-0000")
                )
              ),
              col4(
                card(
                  title = "numeric_input() - with stepper buttons",
                  numeric_input("num_guests", "Number of guests", value = 8, min = 1, max = 50)
                )
              )
            ),
            row(
              col4(
                card(
                  title = "action_button()",
                  action_button("call_customer", "Call customer", icon = "phone")
                )
              ),
              col8(
                card(
                  title = "button()",
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
            )
          )
        )
      ),
      tab_item(
        "decor",
        header(title = "Flags & icons", subtitle = "Country flags and social/brand icons"),
        div(
          class = "page-body",
          div(
            class = "container-xl",
            row(
              col6(
                card(
                  title = "social() - sign-in buttons",
                  div(
                    class = "d-flex flex-wrap gap-2",
                    button("Sign in with Google", icon = social("google"), color = "dark"),
                    button("Sign in with GitHub", icon = social("github"), outline = TRUE, color = "secondary")
                  )
                )
              ),
              col6(
                card(
                  title = "social() - sizes & gray variant",
                  div(
                    class = "d-flex flex-wrap align-items-center gap-3",
                    social("discord", size = "lg"),
                    social("facebook", size = "lg", gray = TRUE),
                    social("instagram", size = "lg"),
                    social("linkedin", size = "lg", gray = TRUE),
                    social("x", size = "lg")
                  )
                )
              )
            ),
            row(
              col6(
                card(
                  title = "flag() - country flags",
                  div(
                    class = "d-flex flex-wrap align-items-center gap-2",
                    flag("us"), flag("gb"), flag("de"), flag("fr"), flag("jp"), flag("br")
                  )
                )
              ),
              col6(
                card(
                  title = "Combining icon(), social() and flag()",
                  p(
                    class = "mb-2",
                    "Star this project on GitHub ", social("github", size = "xs"),
                    ", made in the UK ", flag("gb", size = "xs", class = "align-middle"), "."
                  ),
                  button("Star on GitHub", icon = social("github"), href = "#", color = "dark")
                )
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

tabler_app(ui, function(input, output, session) {})
