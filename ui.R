# This is the user interface of the Shiny app.

shinyUI(fluidPage(
  
  titlePanel("Next Word Prediction App"),
  br(),
  br(),
  h4("Pinaki Bose"),
  h4("29 Oct 2017"),
  
  sidebarLayout(
    sidebarPanel(
      h5("Enter text:"),
      textInput("textsource", label = ""),
      hr()
      ), # sidebarLayout(
    
    mainPanel(
            br(),
      h4("Below is the best predicted word (using Stupid Backoff algorithm on 2% of the swiftkey dataset provided ) :"),
      h3(textOutput("text")),
      hr(),
      h4("Above word predicted using :"),
      h4(textOutput("text1")),
      hr()
                 ) # mainPanel(
) # mainPanel(
)) # shinyUI(fluidPage(