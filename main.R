library(tidyverse)
library(ggplot2)
library(maps)
library(leaflet)
library(dplyr)

airbnb <- read_csv("listings_1.csv")

table(airbnb["bed_type"])



summary(airbnb)


ggplot(data=airbnb) + geom_point(mapping = aes(x = latitude, y = longitude, color=room_type))


world_map <- map_data("world")
ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "white")

pal <- colorFactor(palette = c("red", "green", "blue", "purple", "yellow"), domain = airbnb$room_type)

leaflet(data = airbnb) %>% 
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>% 
  addCircleMarkers(~longitude, 
    ~latitude, 
    color=~pal(room_type), 
    weight = 1, 
    radius=1, 
    fillOpacity = 0.1, 
    opacity = 0.1,
    label = paste("Name:", airbnb$name)) %>% addLegend("bottomright", pal = pal, values = ~room_type,
            title = "Room types",
            opacity = 1)

