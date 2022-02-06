#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    submitButton(text = "Gather Sentiments"),
    
        textInput("query",NULL,placeholder="Enter search terms here"),


        textInput("filters",NULL,placeholder =
                      "Filtered Words (separate with a semicolon,e.g.'bread,toast'"),
        
        actionButton("button", "Fire!"),
        
        plotOutput("histogram"),
        
        downloadLink("full_data",label = "Download Data"),

    
)

server <- function(input, output) {
        results <- sentiment_cannon(input$query,input$filters)
        plotted <- renderPlot(results$distribution)
        output$histogram <- plotted
        output$full_data <- results
        
    }

# Run the application 
shinyApp(ui = ui, server = server)
