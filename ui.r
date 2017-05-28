options(encoding = "UTF-8")
library(shiny)
library(leaflet)

  shinyUI(pageWithSidebar(
      
        headerPanel("Couchsurf Recommend"),
      
        sidebarPanel(
            numericInput(inputId="id", 
                                        label = "Insert your id:", 
                                        value = "", 
                                        width = NULL),
            
              numericInput("obs", "Variant count:", 5)
          ),
      
        mainPanel(
            textOutput("view"), leafletOutput("mymap")
          )
    ))