#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(caret)
library(ggplot2)
library(tidyverse)

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Beer ABV and IBU Analysis"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
#      fileInput("beers", 
#                label = "TBD",
#                accept = ".csv"),
      
      #Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      
      selectInput("select", label = h3("ABV or IBU"), 
                  choices = list("AVB" = "abv", "IBU" = "ibu"), 
                  selected = 1),
      
      radioButtons("radio", label = h3("Histogram or Boxplot"),
                   choices = list("Histogram" = 1, "Boxplot" = 2), 
                   selected = 1),
      
      hr(),
      fluidRow(column(3, verbatimTextOutput("value")))
      
    ),
    
    # Main panel for displaying outputs ----
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
    )
    
  )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  
  
  output$distPlot <- renderPlot({
    
    beers <- read.csv("https://raw.githubusercontent.com/BivinSadler/MSDS_6306_Doing-Data-Science/Master/Unit%208%20and%209%20Case%20Study%201/Beers.csv", header = TRUE)
    beers = beers %>% filter(!is.na(IBU))
    if(input$radio == 1)
    {
      if(input$select == "abv")
      {
        x    <- beers$ABV
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        hist(x, breaks = bins, col = "#75AADB", border = "white",
             xlab = "ABV",
             main = "Histogram of ABVs")
      }
      if(input$select == "ibu")
      {
        x    <- beers$IBU
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        hist(x, breaks = bins, col = "#75AADB", border = "white",
             xlab = "IBU",
             main = "Histogram of IBU")
      }
    }
    if(input$radio == 2)
    {
      if(input$select == "abv")
      {
        x    <- beers$ABV
        
        boxplot(x, col = "#75AADB",
                main = "Histogram of ABVs")
      }
      if(input$select == "ibu")
      {
        x    <- beers$IBU
        
        boxplot(x, col = "#75AADB",
                main = "Boxplot of IBUs")
      }
    }
  })
  
}

shinyApp(ui, server)
