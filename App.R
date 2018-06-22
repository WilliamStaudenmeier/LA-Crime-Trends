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


colorList = c("red", "blue", "green", "orange", "gray", "purple")


table = readRDS("table.rds")


map = readRDS("map.rds")








#################################
ui = fluidPage( theme = shinytheme("simplex"),
                titlePanel("Los Angeles County Crime Data"),
                
                # Create a new Row in the UI for selectInputs
                
                fluidRow(
                  
                  
                  
                  
                  
                  column(4,
                         selectInput("Year",
                                     "Year:",
                                     
                                     c("All",unique(as.character(table$Year),
                                                    selected = "2018")))
                  ),
                  
                  
                  
                  column(4, selectInput("Crime", 
                                        "Crime:",
                                        c("All",unique(as.character(table$Crime),
                                                       selected = "BURGLARY"))
                  )
                  ),
                  
                  
                  
                  # Create a new row for the map.
                  fluidPage( 
                    leafletOutput("mymap"),
                    DT::dataTableOutput("table")
                    
                  )
                )
)





# Server logic ----

server <- function(input, output, session) {
  
  observe({
    
    
    ### 
    
    
    
      output$mymap = renderLeaflet({
        if (input$Year != "All") {
          map <- map[map$Year == input$Year,]
        }
        if (input$Crime != "All") {
          map <- map[map$Crime == input$Crime,]
        }
        
     
        
        
        
        leaflet(map) %>% setView(-118.24, 33.97, zoom = 9) %>%
          addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
          addCircles(lng = ~Longitude, lat = ~Latitude, weight = 2,
                     radius = ~Count*2,  
                     color=sample(colorList, 1, replace = TRUE), 
                     highlightOptions = highlightOptions(
                       color='yellow', opacity = 1, weight = 5.2, fillOpacity = .5,
                       bringToFront = TRUE, sendToBack = TRUE),
                     label=paste(map$Crime, ' - CASES REPORTED -  ', map$Count),
                     labelOptions= labelOptions(direction = 'auto')   )       })
      
   
       
   
    
    
    
    
    
    ####
    
    output$table <- DT::renderDataTable(DT::datatable({
      table <- table 
      if (input$Year != "All") {
        table <- table[table$Year == input$Year,]
      }
      if (input$Crime != "All") {
        table <- table[table$Crime == input$Crime,]
      }
      
      table
    }) )
    
    
  })
  
}
shinyApp(ui, server)




