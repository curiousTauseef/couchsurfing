library(shiny)

set.seed(9)
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(recommenderlab)

setwd("~/couchsurfing")
df <- fromJSON("C:/Users/Anna Schatt/Documents/couchsurfing/couch/infol1.json", flatten=TRUE)
df <- bind_rows(df, .id = 'id_friend')

attach(df)
df = select(df, id_friend, about.countries.visited)
df = filter(df, about.countries.visited != "list()" )


df = unnest(df, about.countries.visited)
df = select(df, id_friend, name)

data_without_id = select(df, name)
id = select(df, id_friend)


dmy = dummyVars("~.", data = data_without_id)
data.main = data.frame(predict(dmy, newdata = data_without_id))
data.main = bind_cols(id, data.main)
data.main = data.main %>% group_by(id_friend) %>% summarise_each(funs(sum))
data.main$id_friend = as.numeric(data.main$id_friend)

names(data.main) = substring(names(data.main), 5)
names(data.main)[1] = "id_friend"

data.main = select(data.main, -id_friend)
data.main = ifelse(data.main>=1,1,0)
data_matrix = as.matrix(data.main)
data_matrix = as(data_matrix, "binaryRatingMatrix")

test_ind <- sample(1:nrow(data_matrix), size = nrow(data_matrix)*0.2)
recc_data_train <- data_matrix[-test_ind, ]
recc_data_test <- data_matrix[test_ind, ]

recc_model <- Recommender(data = recc_data_train, method = "UBCF")



recommendCountries <- function(x,z){
  recc_predicted <- predict(object = recc_model, newdata = recc_data_test, n = z)
  result <- recc_predicted@itemLabels[recc_predicted@items[[x]]]
  return(result)
}


shinyServer(function(input, output){
  output$view <- renderText({
    recommendCountries(input$id, input$obs)
   # output = data.frame(NAME = c('Afghanistan', 'Germany', 'Russia')) 
   output = data.frame(output$view)
   colnames(output) = 'NAME'
    map_filtered = map[ which(map@data$NAME %in% output$NAME ), "NAME"]
    
    output$mymap <- leaflet() %>% 
      
      addProviderTiles("Esri.WorldGrayCanvas") %>% 
      fitBounds(50, 120, 20, 10) %>%
      addPolygons(data = map, 
                  fillColor = 'green', ## we want the polygon filled with 
                  ## one of the palette-colors
                  ## according to the value in student1$Anteil
                  fillOpacity = 0.6, ## how transparent do you want the polygon to be?
                  color = "darkgrey", ## color of borders between districts
                  weight = 1.5, ## width of borders
                  # popup = popup1, ## which popup?
                  group="<span style='color: #7f0000; font-size: 11pt'><strong>2000</strong></span>") %>%
      addPolygons(data = map_filtered, 
                  fillColor = 'red', ## we want the polygon filled with 
                  ## one of the palette-colors
                  ## according to the value in student1$Anteil
                  fillOpacity = 0.6, ## how transparent do you want the polygon to be?
                  color = "black", ## color of borders between districts
                  weight = 1, ## width of borders
                  # popup = popup1, ## which popup?
                  group="<span style='color: #7f0000; f
  })
})
