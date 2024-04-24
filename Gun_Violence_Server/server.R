library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(leaflet.extras)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)
incidentdata<-cleantable

function(input, output, session) {
  
  ## Interactive Map ###########################################
  
  # Create the map
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })
  
  # A reactive expression that returns the set of zips that are
  # in bounds right now
  IncidentsInBoundsBounds <- reactive({
    if (is.null(input$map_bounds))
      return(incidentdata[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(incidentdata,
           latitude >= latRng[1] & latitude <= latRng[2] &
             longitude >= lngRng[1] & longitude <= lngRng[2])
    subset(incidentdata,
           year>= input$year[1] & year <= input$year[2])
  })
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  legend_html = '<div style="background-color: rgba(255, 255, 255, 0.4); padding: 5px; border-radius: 5px; border: 1px solid black; width: 120px; text-align: center;">
                  <h4 style="color: black;">Amount of People Killed</h4>
                  <div style="background: linear-gradient(to right, blue, green, yellow, red); height: 20px; border-radius: 5px;"></div>
                  <div>Low  Medium  High</div>
              </div>'
  
  observe({
    incidentType <-input$incident
    incidentdata$incident<-incidentdata[,incidentType]
    leafletProxy("map", data = incidentdata) %>%
      clearShapes() %>%
      addTiles() %>% setView(lng = -98.5795, lat = 39.8283, zoom = 3.5) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addHeatmap(
        lng = ~longitude,  
        lat = ~latitude,  
        intensity = ~incident,  
        blur = 20,    
        max = max(incidentdata$incident, na.rm = TRUE),      
        radius = 20,  
        gradient = c("blue", "green", "yellow", "red")  
      )
  })
  
  observe({
    cities <- if (is.null(input$states)) character(0) else {
      filter(cleantable, state %in% input$states) %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$cities[input$cities %in% cities])
    updateSelectizeInput(session, "cities", choices = cities,
                         selected = stillSelected, server = TRUE)
  })
}