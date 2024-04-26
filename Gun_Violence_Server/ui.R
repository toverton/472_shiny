library(shiny)
library(leaflet)
library(DT)

# Choices for drop-downs
vars <- c("Number Killed" = 'n_killed',
          "Number Injured" = 'n_injured',
          "Killed per Capita" = 'per_hthous_killed'
)

navbarPage("Gun Violence", id="nav",
  tabPanel("Interactive map",
    div(class="outer",
      tags$head(
      # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),
      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map", width="100%", height="100%"),
      # Shiny versions prior to 0.11 should use class = "modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",
        h2("Gun Violence Explorer"),
        selectInput("incident", "Incident Type", vars, selected = "n_killed"),
        sliderInput("year", "Year:",
                    min = 2013, max = 2018,
                    value = c(2013, 2018)
                    ),
        plotOutput("timeseries", height=200)
      ),
    )
  ),
  tabPanel("Data Explorer",
    fluidRow(
      column(3,
        selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
      )
    ),
    hr(),
    DT::dataTableOutput("cleantable")
  ),
)
