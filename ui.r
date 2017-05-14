library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Couchsurf Recommend"),
  
  sidebarPanel(
    numericInput(inputId="id", 
              label = "Введите свой id:", 
              value = "", 
              width = NULL),
    
    numericInput("obs", "Количество вариантов:", 5)
  ),
  
  mainPanel(
    textOutput("view")
  )
))
