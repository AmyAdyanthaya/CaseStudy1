
library(shiny)
library(caret)
library(ggplot2)
library(tidyverse)
library(dplyr)

# Define UI for application that plots features of movies
ui <- fluidPage(
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    # Inputs: Select variables to plot
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("beers", 
                label = "TBD",
                accept = ".csv"),
      
      selectInput("beers", label = h3("Please Select a State"), 
                  choices = beers$State, 
                  selected = 1),
      
      selectInput(inputId = "smooth", label = h3("Add Regression Line?"),
                  choices = c("Yes" = 1, "No" = 2))
      
#           checkboxInput(inputId = "smooth",
#                    label = "Add Regression line?",
#                    value = FALSE)
    ),
    # Output: Show scatterplot
    mainPanel(
      plotOutput(outputId = "scatterplot")
    )
  )
)

# Define server function required to create the scatterplot
server <- function(input, output) {
  # Create scatterplot 
  output$scatterplot <- renderPlot({
    
    beers = read.csv("/Users/amyadyanthaya/Desktop/DS6306/Unit8_9/mbeers.csv")
    beers <- beers[, c("ABV","IBU","State")]
    beers = beers %>% filter(!is.na(IBU))
    
    if(input$smooth == 1) {        
      ggplot(data = beers, aes_string(x = "ABV", y = "IBU")) +
        geom_point() + geom_smooth(method = "lm")
    }
    if(input$smooth == 2) {        
      ggplot(data = beers, aes_string(x = "ABV", y = "IBU")) +
        geom_point()
    }
    
  })
}

shinyApp(ui, server)
