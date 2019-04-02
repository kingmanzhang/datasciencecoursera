#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Predict The Next Word"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      textInput("query", "query text", value = ""),
      submitButton("Submit")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h4("How to use"),
      p("Type in some text and click Submit to predict the next word"),
      p("\n\n"),
      h4("Predicted word"),
      p(textOutput("pred")),
      p("\n\n")
      )
  )))
