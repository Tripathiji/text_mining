#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Hult_Text_mining"),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Wordcloud", plotOutput("distPlot",  width = "150%", height = "700px")),
      tabPanel("N-Gram", plotOutput("ngram",  width = "150%", height = "700px")),
      tabPanel("Correlograms", plotOutput("correlation",  width = "150%", height = "700px")),
      tabPanel("Sentiment Analysis", plotOutput("sentiment",  width = "150%", height = "700px")),
      tabPanel("Contribution to Sentiment", plotOutput("contribution",  width = "150%", height = "700px"))
    )
    
  )
)
)
