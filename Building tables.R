library(dplyr)
library(ggmap)
library(ggplot2)
library(tmap)
library(rgdal)
library(shiny)
library(leaflet)
library(shinydashboard)
library(shinythemes)
library(maps)
library(mapproj)
library(shinydashboard)
library(lubridate)
library(tidyr)
library(htmltools)
library(RSocrata)

#upload data from government portal and save as RDS to reduce computational overhead

data = as.data.frame(read.socrata('https://data.lacity.org/A-Safe-City/Crime-Data-from-2010-to-Present/y8tr-7khq'), stringsAsFactors =F)

saveRDS(data, "Crime 2.rds")

#upload saved dataframe

df = as.data.frame(readRDS("Crime 2.rds"), stringsAsFactors =F)

#clean data

df$Date = as.Date(as.character(df$Date.Occurred), "%m/%d/%Y")

df$Year = year(df$Date)

df = df %>% filter(Year > 2015)

df$Crime = df$Crime.Code.Description

df$Area = df$Area.Name

df$Weapon = df$Weapon.Description


#separate location into lat and long for map plotting
df = df%>% separate(Location, into = c("Latitude", "Longitude"), sep =",")


df$Latitude = gsub( "[(]", "", df$Latitude)

df$Longitude = gsub( "[)]", "", df$Longitude)

df$Latitude = round(as.numeric(as.character(df$Latitude)), 1)

df$Longitude = round(as.numeric(as.character(df$Longitude)),1)

df$Longitude[df$Longitude ==0] <- NA

df$Latitude[df$Latitude ==0] <- NA

#create aggregates by area for table

table = df%>% 
  group_by(Crime, Year, Area) %>% mutate(Count = n()) %>%
  arrange(desc(Year), desc(Count)) %>% 
  dplyr::select(Year, Area, Crime, Count) %>% 
  distinct()

#create aggregates by lat long for map
#What? Different groupings for the table and map?
#Just go with it.  It's going to look great.  

map = df%>% 
  group_by(Crime, Year, Latitude, Longitude) %>% mutate(Count = n()) %>%
  arrange(desc(Year), desc(Count)) %>% 
  dplyr::select(Year, Crime, Count, Latitude, Longitude) %>% 
  distinct()

#because we rounded to one digit to impress the ladies, some of the points end up in the ocean
#we can fix this by adding just a teensy weensy decimal to the latitude column
map$Latitude = map$Latitude + .035

#save your tables unless you have a sick server which I do not

saveRDS(table, "table.rds")

saveRDS(map, "map.rds")
