library(shiny)

set.seed(9)
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(recommenderlab)
library(stringr)
library(readr)
library(rsconnect)
library(rgdal)
library(rPython)
library(magrittr)
options(encoding = "UTF-8")

map = readOGR(dsn = '.', 'TM_WORLD_BORDERS-0_3')
sample <- read_csv("sample.csv") 
host_info <- read_csv("host_info.csv")
ids <- sample$root_id
sample <- sample[217:449] 
rownames(sample) <- paste("id", ids, sep="_")
names <- colnames(sample) 
library(stringr) 
names <- str_replace_all(names, "countries.visited_", "") 
colnames(sample) <- names
sample <- as.matrix(sample)

x = 2005004093
z = 5

recommendCountries <- function(x,z){
  if ( x != "22704") {
    python.exec("from couchsurfing import Api")
    python.exec('api = Api("95schatt@gmail.com", "MIKKy1989")')
    x = as.character(x)
    python.assign("x", x)
    b = python.method.call("api", "get_profile_by_id", x)
    
    id = b$id
    countries = b$about$countries$visited
    df <- data.frame(matrix(unlist(countries), nrow=length(countries), byrow=T),stringsAsFactors=FALSE)
    user_matrix <- matrix(1, 1, length(countries))
    rownames(user_matrix) <- paste("id", id, sep = "_")
    colnames(user_matrix) <- df$X2
    
    full_user_matrix <- matrix(0, 1 , dim(sample)[2])
    rownames(full_user_matrix) <- paste("id", id, sep = "_")
    colnames(full_user_matrix) <- colnames(sample)
    
    cols <- colnames(full_user_matrix)[colnames(full_user_matrix) %in% colnames(user_matrix)]
    rows <- rownames(full_user_matrix)[rownames(full_user_matrix) %in% rownames(user_matrix)]
    full_user_matrix[rows, cols] <- user_matrix[rows, cols]
    
    recc_model <- readRDS("./recc_model.rds")
    full_user_matrix = as(full_user_matrix, "binaryRatingMatrix")
    recc_predicted <- predict(object = recc_model, newdata = full_user_matrix, n = z)
    print(recc_predicted)
    x = rownames(full_user_matrix)[1]
    a = recc_predicted@items[names(recc_predicted@items) == x]
    result <- recc_predicted@itemLabels[a[[1]]]
    print(typeof(result))
    new_res = str_replace_all(result, '\\.', ' ')
    print(new_res)
    return(new_res)
  }
  else if (x == "22704")
    {
    recc_predicted <- readRDS("./recc_predicted.rds")
    print(recc_predicted)
    x = python.get("x")
    x <- paste("id",x,sep="_")
    a = recc_predicted@items[names(recc_predicted@items) == x]
    result <- recc_predicted@itemLabels[a[[1]]]
    print(typeof(result))
    new_res = str_replace_all(result, '\\.', ' ')
    print(new_res)
    return(new_res)
    }
}




recommendHosts <- function(new_res) {
  if (z == 5) {
    df = dplyr::filter(host_info, country == new_res[1] | country == new_res[2] | country == new_res[3] | country == new_res[4] | country == new_res[5])
  }
  else if (z == 4) {
    df = dplyr::filter(host_info, country == new_res[1] | country == new_res[2] | country == new_res[3] | country == new_res[4])
  }
  else if (z == 3) {
    df = dplyr::filter(host_info, country == new_res[1] | country == new_res[2] | country == new_res[3] )
  }
  else if (z == 2) {
    df = dplyr::filter(host_info, country == new_res[1] | country == new_res[2])
  }
  else {
    df = dplyr::filter(host_info, country == new_res[1] )
  }
  hosts_table = df
  return(hosts_table)
}



shinyServer(function(input, output){
  output$view <- renderText({
    recommendCountries(input$id, input$obs)
  })
  
  output$mymap <- renderLeaflet({leaflet() %>% 
      
      addProviderTiles("Esri.WorldGrayCanvas") %>%
      fitBounds(50, 120, 20, 10) %>%
      addPolygons(data = map, 
                  fillColor = 'black', ## we want the polygon filled with 
                  ## one of the palette-colors
                  ## according to the value in student1$Anteil
                  fillOpacity = 0.6, ## how transparent do you want the polygon to be?
                  color = "darkgrey", ## color of borders between districts
                  weight = 1.5, ## width of borders
                  # popup = popup1, ## which popup?
                  group="<span style='color: #7f0000; font-size: 11pt'><strong>2000</strong></span>") %>%
      addPolygons(data = map[ which(map@data$NAME %in% data.frame(NAME = recommendCountries(input$id, input$obs))$NAME ), "NAME"], 
                  fillColor = 'red', ## we want the polygon filled with 
                  ## one of the palette-colors
                  ## according to the value in student1$Anteil
                  fillOpacity = 0.6, ## how transparent do you want the polygon to be?
                  color = "black", ## color of borders between districts
                  weight = 1, ## width of borders
                  # popup = popup1, ## which popup?
                  group="<span style='color: #7f0000; font-size: 11pt'><strong>2000</strong></span>")
  }) 
  
  
})

