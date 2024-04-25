library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(leaflet.extras)
library(ggplot2)

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
  IncidentsInBounds <- reactive({
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
  
  output$timeseries<-renderPlot({
    if (nrow(IncidentsInBounds()) == 0)
      return(NULL)
    data<-IncidentsInBounds()
    print(ggplot(data, aes(date, n_killed))+geom_line()+ylab("Total Deaths")+xlab("Date"))
  })
  
  legend_html = '<div style="background-color: rgba(255, 255, 255, 0.4); padding: 5px; border-radius: 5px; border: 1px solid black; width: 120px; text-align: center;">
                  <h4 style="color: black;">Amount of People Killed</h4>
                  <div style="background: linear-gradient(to right, blue, green, yellow, red); height: 20px; border-radius: 5px;"></div>
                  <div>Low  Medium  High</div>
              </div>'
  
  observe({
    incidentType <-input$incident
    incidentdata$incident<-incidentdata[,incidentType]
    incidentdata<-subset(incidentdata,
           year>= input$year[1] & year <= input$year[2])
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
  
  
  ## Data Observer
  observe({
    cities <- if (is.null(input$states)) character(0) else {
      filter(cleantable, State %in% input$states) %>%
        `$`('City') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$cities[input$cities %in% cities])
    updateSelectizeInput(session, "cities", choices = cities,
                         selected = stillSelected, server = TRUE)
  })
  
  observe({
    zipcodes <- if (is.null(input$states)) character(0) else {
      cleantable %>%
        filter(State %in% input$states,
               is.null(input$cities) | City %in% input$cities) %>%
        `$`('Zipcode') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
    updateSelectizeInput(session, "zipcodes", choices = zipcodes,
                         selected = stillSelected, server = TRUE)
  })
  
  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      zip <- input$goto$zip
      lat <- input$goto$lat
      lng <- input$goto$lng
      showZipcodePopup(zip, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })
  
  output$ziptable <- DT::renderDataTable({
    df <- cleantable %>%
      filter(
        Score >= input$minScore,
        Score <= input$maxScore,
        is.null(input$states) | State %in% input$states,
        is.null(input$cities) | City %in% input$cities,
        is.null(input$zipcodes) | Zipcode %in% input$zipcodes
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df, outputId = "ziptable")
    
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
}