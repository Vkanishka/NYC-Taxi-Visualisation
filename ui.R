
library(shinydashboard)
library(leaflet)
library(geosphere)
library(dplyr)
library(plotly)
library(highcharter)

nytaxi <- read.csv("/Users/kanishkavenishetty/Documents/DV/MyData.csv")
nytaxi <- sample_n(nytaxi, 1000)



pick_coord <- nytaxi %>%
  dplyr::select(pickup_longitude, pickup_latitude)
drop_coord <- nytaxi %>%
  dplyr::select(dropoff_longitude, dropoff_latitude)
nytaxi$dist <- distCosine(pick_coord, drop_coord)

nytaxi$pickup_date<- format(as.POSIXct(nytaxi$pickup_datetime,format='%Y-%m-%d %H:%M:%S'),format='%m/%d/%Y')
nytaxi$dropoff_date<- format(as.POSIXct(nytaxi$dropoff_datetime,format='%Y-%m-%d %H:%M:%S'),format='%m/%d/%Y')

header <- dashboardHeader(title = "New York City Taxi")

body <- dashboardBody(tabItems(
  tabItem(tabName = "count",
          fluidRow(valueBox("Total Distance", sum(nytaxi$dist), width = 4, color = "blue"),
                   valueBox("No. of trips", length(nytaxi$trip_duration), width = 4, color = "green"),
                   valueBox("Average Distance", sum(nytaxi$dist)/100, width = 4, color = "red")),
          fluidRow(
            
            box(
              title = "Histogram",
              solidHeader = TRUE,
              collapsible = FALSE,
              selectInput(
                "count_select1",
                "Select the axis variable:",
                c(
                  "Passenger Count" = "passenger_count",
                  "Store and Forward Graph" = "store_and_fwd_flag",
                  "Vendor ID" = "vendor_id"
                )
              ),
              plotlyOutput("plot1")
              ,
              width = 12
            )
          ),
          fluidRow(
            box(
              title = "Histogram",
              solidHeader = TRUE,
              collapsible = FALSE,
              selectInput(
                "count_select2",
                "Select the axis variable:",
                c(
                  "Pickup longitude" = "pickup_longitude",
                  "Pickup latitude" = "pickup_latitude",
                  "Dropoff Longitude" = "dropoff_longitude",
                  "Dropoff Latitude" = "dropoff_latitude"
                )
              ),
              plotlyOutput("plot2")
              ,
              width = 12
            )
          )),
  
  
  tabItem(tabName = "map",
          fluidRow(valueBox("Total Distance", sum(nytaxi$dist), width = 4, color = "blue"),
                   valueBox("No. of trips", length(nytaxi$trip_duration), width = 4, color = "green"),
                   valueBox("Average Distance", sum(nytaxi$dist)/100, width = 4, color = "red")),
          
          fluidRow(
            column(width = 9,
                   box(
                     width = NULL,
                     solidHeader = TRUE,
                     leafletOutput("map", height = 800)
                   )),
            column(
              width = 3,
              box(
                width = NULL,
                status = "warning",
                uiOutput("routeSelect"),
                radioButtons(
                  "mapType",
                  "Map Type",
                  c(
                    "Show Pick-up Map" = "pick",
                    "Show Drop-OfF Map" = "drop",
                    "Cluster" = "cluster"
                  )
                ),
                p(
                  class = "text-muted",
                  paste(
                    "Note: a route number can have several different trips, each",
                    "with a different path. Only the most commonly-used path will",
                    "be displayed on the map."
                  )
                ),
                actionButton("zoomButton", "Zoom to fit Cars")
              )
              
            )
          )),
  tabItem(tabName = "scatter",
          fluidRow(valueBox("Total Distance", sum(nytaxi$dist), width = 4, color = "blue"),
                   valueBox("No. of trips", length(nytaxi$trip_duration), width = 4, color = "green"),
                   valueBox("Average Distance", sum(nytaxi$dist)/100, width = 4, color = "red")),
          fluidRow(
            box(
              title = "Scatter Plot",
              solidHeader = TRUE,
              collapsible = FALSE,
              selectInput(
                "count_select_scattet",
                "Select the axis variable:",
                c(
                  "Passenger Count" = "passenger_count",
                  "Distance" = "dist"
                )
              ),
              highchartOutput("plot3")
              ,
              width = 12
            )
          ),
          fluidRow(
            box(
              title = "Line Graph",
              solidHeader = TRUE,
              collapsible = FALSE,
              selectInput(
                "scatter_select",
                "Select the axis variable:",
                c(
                  "Pickup Date" = "pickup_date",
                  "Drop Off Date" = "dropoff_date"
                )
              ),
              highchartOutput("plot4")
              ,
              width = 12
            )
          )
          )
  
  
  
))



dashboardPage(header,
              dashboardSidebar(
                sidebarMenu(
                  style = "position: fixed; overflow: visible;",
                  menuItem("Count Plots", tabName = "count", icon = icon("list-ol")),
                  menuItem("Map Plot", tabName = "map", icon = icon("map")),
                  menuItem("Scatter and Line Graph", tabName = "scatter", icon = icon("map"))
                  
                )
              ),
              body)