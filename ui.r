options(encoding = "UTF-8")
library(shiny)
library(leaflet)

shinyUI(fluidPage(
  includeCSS("black.css"),
  headerPanel("Couchsurf Recommend"),
  
  sidebarPanel(
    numericInput(inputId="id",
                 label = "Insert your id:",
                 value = "22704",
                 width = NULL),
    
    sliderInput("obs", "Number of countries:", min = 1, max = 5, value = 5, step = 1),
    actionButton("predict", "Predict")
  ),
  
  mainPanel(
    h2(textOutput("view")),
    leafletOutput("mymap")
  )
))