#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Predict Your Car Fuel Efficiency"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
        checkboxInput("am", "Automatic Transmission", value = TRUE ),
        numericInput("cyl", "Number of cylinders", value = 6, min = 4, max = 8, step = 2),
        sliderInput("wt", "Weight:", min = 1513, max = 5424, value = 3500, step = 10)
        #submitButton("Submit")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
        h4("How to use"),
        p("Select car transmission type, choose the number of cylinders (4, 6, or 8) and 
          then specify the weight in pounds. The predicted value will be printed below, 
          and the data point with a cross will show up in the following plot."),
        p("\n\n"),
        h4("User Input Parameters"),
        p(textOutput("userParam")),
        h4("Predicted Mileage per Gallon(MPG)"),
        p(textOutput("pred")),
        p("\n\n"),
        plotlyOutput("modelplot"),
        p("Note: data point size corresponds to the number of cylinders, 
          the color correspond to the type of transmissions, 
          training data is shown as circles and user data point is shown as a cross."),
        h4("Predictive Model"),
        p("A simple linear model mpg ~ transmission type + cylinders + weight.")
        
    )
  )
))
