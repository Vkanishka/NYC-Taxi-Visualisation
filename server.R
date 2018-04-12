#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$map <- renderLeaflet({
    if(input$mapType == "pick"){
      leaflet() %>%
        addTiles()%>%
        addMarkers(lng = nytaxi$pickup_longitude, lat= nytaxi$pickup_latitude, label= paste0("Trip Duartion: ", nytaxi$trip_duration, "; Vendor: ", nytaxi$vendor_id))
    }else{ if(input$mapType == "drop"){
      leaflet() %>%
        addTiles()%>%
        addMarkers(lng = nytaxi$dropoff_longitude, lat= nytaxi$dropoff_latitude, label= paste0("Trip Duartion: ", nytaxi$trip_duration, "; Vendor: ", nytaxi$vendor_id))
    }else{
      leaflet() %>%
        addTiles()%>%
        addMarkers(lng = nytaxi$dropoff_longitude, lat= nytaxi$dropoff_latitude, label= paste0("Trip Duartion: ", nytaxi$trip_duration, "; Vendor: ", nytaxi$vendor_id), clusterOptions = markerClusterOptions())
    }
    }
    
  })
  
  
  output$plot1 <- renderPlotly({
    abc <- input$count_select1
    if(abc == "passenger_count"){
      
      plot_ly(nytaxi, x = ~passenger_count)
      
    }else{
      if(abc == "store_and_fwd_flag"){
        
        plot_ly(nytaxi, x = ~store_and_fwd_flag)
        
      }else{
        if(abc == "vendor_id"){
          
          plot_ly(nytaxi, x = ~vendor_id)
          
        }
      }
    }
  })
  
  output$plot2 <- renderPlotly({
    xyz <- input$count_select2
    
    if(xyz == "pickup_longitude"){
      
      plot_ly(nytaxi, x = ~pickup_longitude)
      
    }else{
      if(xyz == "pickup_latitude"){
        
        plot_ly(nytaxi, x = ~pickup_latitude)
        
      }else{
        if(xyz == "dropoff_longitude"){
          
          plot_ly(nytaxi, x = ~dropoff_longitude)
          
        }else{
          if(xyz == "dropoff_latitude"){
            plot_ly(nytaxi, x = ~dropoff_latitude)
          }
        }
      }
    }
    
    #xyz <- input$count_select2
    #ggplot(nytaxi, aes_string(xyz)) +
      #geom_histogram(fill = "blue", bins = 40)
  })
  
  output$plot3 <- renderHighchart({
    if(input$count_select_scattet== "passenger_count"){
      highchart()  %>%
        hc_add_series_scatter(nytaxi$passenger_count, log10(nytaxi$trip_duration), showInLegend = FALSE) %>%
        hc_colors(color='black') %>%
        hc_yAxis(title=list(text='Passenger Count')) %>%
        hc_xAxis(title=list(text='Trip Distance'))%>%
        hc_tooltip(headerFormat = "", pointFormat = "Passenger Count: {point.x} <br> Distance: {point.y}")
    }else{
      highchart()  %>%
        hc_add_series_scatter(log10(nytaxi$dist), log10(nytaxi$trip_duration), showInLegend = FALSE) %>%
        hc_colors(color='black') %>%
        hc_yAxis(title=list(text='Tip Amount')) %>%
        hc_xAxis(title=list(text='Trip Distance'))%>%
        hc_tooltip(headerFormat = "", pointFormat = "Tip: {point.x} <br> Distance: {point.y}")
    }

    
  })
  
  output$plot4 <- renderHighchart({
    a <- input$scatter_select
    if(a == "pickup_date"){
      nytaxi_grp <- nytaxi %>% group_by(pickup_date) %>% dplyr::summarise(Total = n(), mean_dist= sum(dist)/Total)
      highchart() %>% 
        hc_xAxis(categories = nytaxi_grp$pickup_date) %>% 
        hc_add_series(name = "Average Distance", data = nytaxi_grp$mean_dist, color = "blue") 
    }else{
    if(a == "dropoff_date"){
      nytaxi_grp <- nytaxi %>% group_by(dropoff_date) %>% summarise(Total = n(), mean_dist= sum(dist)/Total)
      highchart() %>% 
        hc_xAxis(categories = nytaxi_grp$dropoff_date) %>% 
        hc_add_series(name = "Average Distance", data = nytaxi_grp$mean_dist, color = "blue") 
    }
    }
    
  })
  
})
