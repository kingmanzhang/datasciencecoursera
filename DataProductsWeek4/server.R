# This is a Shiny App that predicts fuel efficiency based on transmission type, number of cylinders and weight(lb)

library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)

# build a linear model to predict fuel efficiencies
mtcars_model <- mtcars %>% mutate(cyl = as.factor(cyl), am = as.factor(am))
model <- lm(mpg ~ am + cyl + wt, data = mtcars_model)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    userPred <- reactive({
        userAm = as.factor(ifelse(input$am, 0, 1))
        userCyl = as.factor(input$cyl)
        userWt = input$wt / 1000
        userPred = round(predict(model, newdata = data.frame(am = userAm, cyl = userCyl, wt = userWt)), 1)
    })
    
    output$userParam <- renderText({
        paste(ifelse(input$am, "autotransmission", "manual transmission"), "\tcyl: ", input$cyl, "\tweight: ", input$wt, sep = " ")
    })
    
    output$pred <- renderText({
        paste("Prediction", userPred(), sep = ": ")})

  output$modelplot <- renderPlotly({
      colorcoding <- c(`0` = "orange", `1` = "blue")
    ## plot mtcars mpg ~ am + cyl + wt
      #ggplot(data = mtcars %>% mutate(am = as.factor(am))) + 
      #    geom_point(aes(x = wt, y = mpg, size = cyl, color = am), shape = 1) + 
      #    scale_size_continuous(guide = FALSE) +
      #    scale_color_manual(breaks = c(0, 1), values = colorcoding, labels = c("auto", "manual")) + 
      #    geom_point(data = data.frame(), aes(x = input$wt / 1000, y = userPred(), size = input$cyl),  color = "red")
      # ggplotly(p)
      userData <- data.frame(am = input$am, cyl = input$cyl, wt = input$wt / 1000, mpg = userPred(), source = "user")
      data = mtcars %>% 
          select(am, cyl, wt, mpg) %>% 
          mutate(source = rep("train", nrow(mtcars))) %>% 
          rbind(userData) %>% 
          mutate(am = ifelse(am == 0, "auto", "manual"))
      
      # use plot_ly 
      #p <- plot_ly() %>%
      #    add_trace(data = data, x = ~wt, y = ~mpg, size = ~cyl, color = ~am, colors = c("orange", "blue"), 
      #              symbol = ~source, symbols = c("circle", "square"),
      #              type = "scatter", mode = "markers")
      
      # use ggplot and then transfer to plot_ly
      p <- ggplot(data) + 
          geom_point(aes(x = wt, y = mpg, size = cyl, color = am, shape = source)) +
          scale_color_manual(name = "legend", breaks = c("auto", "manual"), values = c("orange", "blue"))  +
          scale_shape_manual(breaks = c("train", "user"), values = c(16, 3)) + 
          guides(shape = FALSE, size = FALSE)
      
      ggplotly(p)
  })
  
})
