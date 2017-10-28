# This is the server logic of the app. 
# The code to predict the next word is in a separate file.

source("prediction_from_corpus.R")

shinyServer(function(input, output) {
  
  output$text <- renderText({
    validate(
      need(input$textsource != "", "No query entered.")
    )
    Results <- predict(input$textsource)
    BestWord <- strsplit(Results$Ngram[1], " ")
    BestWord <- unlist(BestWord)
    BestWord <- rev(BestWord)
    BestWord <- BestWord[1]
  })
  

  output$text1 <- renderText({
    validate(
      need(input$textsource != "", "No query entered.")
    )
    Results <- predict(input$textsource)
    
    head(Results$Evidence, 1)
  })
  
})