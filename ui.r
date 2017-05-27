library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Couchsurf Recommend"),
  
  sidebarPanel(
    numericInput(inputId="id", 
              label = "Введите свой id:", 
              value = "", 
              width = NULL),
    
    numericInput("obs", "Количество вариантов:", 5),
    
    actionButton("action", label = "Искать")
  ),
  
  mainPanel(
    textOutput("view"),
   leafletOutput("mymap")
  )
))
