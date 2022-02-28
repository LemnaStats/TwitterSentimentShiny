require(shiny)
require(tidyverse)
require(tidytext)
require(httr)
require(rdrop2)

ui <- fluidPage(
    
        titlePanel("Twitter Sentiment Cannon"),
        
        textInput("query",NULL,placeholder="Enter search terms here",
                  width = "500px"),
        textInput("filters",NULL,placeholder =
                      "Filtered Words (separate with a semicolon and space, e.g.'bread; toast')",
                  width = "500px"),
        
        
        actionButton("button", "Fire!"),
        
        p(textOutput("tweet_count")),
        p(textOutput("score")),
        p(textOutput("average")),
        p(textOutput("median")),
        
        imageOutput("histogram", width = "150px"),
        imageOutput("topten", width = "150px"),
        h6("2022 Max Kupperman / Lemna Statistics")

)

server <- function(input, output) {
    
    observeEvent(input$button, {
        
    #Shoot the cannon    
    write_sentiments(input$query,input$filters)
        
    #read sentiment table into a file
    sentiments <- read_csv("sentiments.csv")
    
    #get tweet count
    tweet_count <- sentiments$tweet_count %>% as.character()
    word_count <- sentiments$word_count %>% as.character()
    output$tweet_count <- str_glue("The sentiment cannon has gathered ",
                                   word_count,
                                   " emotionally charged words in ",tweet_count,
                                   " recent tweets.") %>% renderText()
    
    #get score
    score <- sentiments$score %>% as.character()
    output$score <- str_glue("Total score: ", score) %>%
        renderText()
    
    #get average
    average_score <- sentiments$average_score %>% as.character()
    output$average <- str_glue("Average score: ", average_score) %>%
        renderText()
    
    #get median
    median_score <- sentiments$median %>% as.character()
    output$median <- str_glue("Median score: ", median_score) %>%
        renderText()

    #Plot the histogram
    output$histogram <- renderImage({
        
        filename <- 'histogram.png'
                list(src = filename,
                     contentType = 'image/png',
                     alt = "Histogram of sentiments",
                     width = '400%'
             )
        
    }, deleteFile = FALSE)
    
    #Plot the top ten
    output$topten <- renderImage({
        
        filename <- 'topten.png'
        list(src = filename,
             contentType = 'image/png',
             alt = "Top ten words",
             width = '400%'
        )
        
    }, deleteFile = FALSE)
    
    })
    
        
}


shinyApp(ui = ui, server = server)

