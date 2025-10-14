#' Tabler: Create Dashboards with Tabler and Shiny
#'
#' @description
#' The tabler package provides a simplified way to create beautiful dashboards
#' using the modern Tabler CSS framework with Shiny. This package offers a
#' clean, consistent API with essential components for building professional
#' web applications.
#'
#' @section Main Functions:
#'
#' Core dashboard structure:
#' \itemize{
#'   \item \code{\link{tabler_page}}: Main dashboard page
#'   \item \code{\link{tabler_body}}: Dashboard body container
#'   \item \code{\link{tabler_navbar}}: Navigation bar
#'   \item \code{\link{tabler_footer}}: Footer component
#' }
#'
#' UI Components:
#' \itemize{
#'   \item \code{\link{tabler_card}}: Card component
#'   \item \code{\link{tabler_value_box}}: Value/metric display boxes
#'   \item \code{\link{tabler_icon}}: Icon component
#'   \item \code{\link{tabler_progress}}: Progress bars
#'   \item \code{\link{tabler_alert}}: Alert/notification messages
#'   \item \code{\link{tabler_button}}: Styled buttons
#' }
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(tabler)
#'
#'   ui <- tabler_page(
#'     title = "My Tabler Dashboard",
#'     navbar = tabler_navbar(
#'       title = "My App"
#'     ),
#'     body = tabler_body(
#'       fluidRow(
#'         tabler_value_box(
#'           value = "1,352",
#'           title = "Total Users",
#'           icon = "users",
#'           color = "primary",
#'           width = 3
#'         ),
#'         tabler_value_box(
#'           value = "87.5%",
#'           title = "Success Rate",
#'           icon = "trending-up",
#'           color = "success",
#'           width = 3
#'         ),
#'         tabler_value_box(
#'           value = "24",
#'           title = "Active Projects",
#'           icon = "folder",
#'           color = "warning",
#'           width = 3
#'         ),
#'         tabler_value_box(
#'           value = "$12,345",
#'           title = "Revenue",
#'           icon = "currency-dollar",
#'           color = "info",
#'           width = 3
#'         )
#'       ),
#'       fluidRow(
#'         column(
#'           8,
#'           tabler_card(
#'             title = "Sample Chart",
#'             plotOutput("plot"),
#'             status = "primary"
#'           )
#'         ),
#'         column(
#'           4,
#'           tabler_card(
#'             title = "Progress Status",
#'             tabler_progress(75, color = "success", label = TRUE),
#'             br(),
#'             tabler_progress(45, color = "warning", label = TRUE),
#'             br(),
#'             tabler_progress(90, color = "info", label = TRUE)
#'           ),
#'           tabler_alert(
#'             title = "Important Notice",
#'             "This is a sample alert message.",
#'             type = "info",
#'             dismissible = TRUE
#'           )
#'         )
#'       )
#'     ),
#'     footer = tabler_footer(
#'       left = "© 2024 My Company",
#'       right = "Built with Tabler"
#'     )
#'   )
#'
#'   server <- function(input, output, session) {
#'     output$plot <- renderPlot({
#'       plot(cars, main = "Sample Plot", col = "steelblue", pch = 19)
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
#'
#' @keywords internal
"_PACKAGE"
