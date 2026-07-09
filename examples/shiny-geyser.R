#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(magrittr)

# Application title
title  <- "Old Faithful Geyser Data" %>% 
            titlePanel()

# Sidebar with a slider input for number of bins 
side   <- sliderInput("bins",
                      "Number of bins:",
                      min = 1,
                      max = 50,
                      value = 30) %>%   
            sidebarPanel()

# Show a plot of the generated distribution
main   <- plotOutput("distPlot") %>% 
            mainPanel()

# Combine sidebar and main panels into layout
layout <- sidebarLayout(side, 
                        main)

# Define UI for application that draws a histogram
ui     <- fluidPage(title, 
                    layout)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
    output$distPlot <- renderPlot({
       # Generate bins based on input$bins from ui.R
       bins <- input$bins + 1
        
       # Draw the histogram with the specified number of bins
       faithful[, 2] %>% 
         hist(breaks = seq(min(.), 
                           max(.), 
                           length.out = bins), 
              col = 'darkgray', 
              border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
