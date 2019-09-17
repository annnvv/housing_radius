  # Step 4: Data Visualization
  
  library(leaflet)
  library(rgdal)
  
  #https://www.r-graph-gallery.com/4-tricks-for-working-with-r-leaflet-and-shiny/
  
  # load zipcodes shapefile
  data <- readOGR("C:/Users/Anna V/Documents/GitHub/housing_radius/data/shapefiles/DMV_zipcodes_housing_data.shp")
  data <- spTransform(data, CRS("+init=epsg:4326"))
  
  pal <- colorNumeric("YlOrRd", merged_shp$median_sq_ft.Condo_Co_op)
  # create the visualization
  leaflet(data) %>%
    # centered on US Capitol
    setView(lng = -77.008989, lat = 38.890468, zoom = 9) %>% 
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.5,
                fillColor = ~colorQuantile("YlOrRd", mn__C_C_)(mn__C_C_),   
                highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                popup = data$labl_mn) %>% 
    addLegend("bottomright", pal = pal, values = ~mn__C_C_,
              title = "Condos Mean Price by Zipcode",
              labFormat = labelFormat(prefix = "$"),
              opacity = 1)
  
  leaflet(merged_shp) %>%
    # centered on US Capitol
    setView(lng = -77.008989, lat = 38.890468, zoom = 9) %>% 
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.5,
                fillColor = ~colorNumeric("YlOrRd", median_sq_ft.Condo_Co_op)(median_sq_ft.Condo_Co_op),   
                highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                popup = ~label_mean) %>% 
    addLegend("bottomright", pal = pal, values = ~median_sq_ft.Condo_Co_op,
              title = "Condos Mean Square feet by Zipcode",
              #labFormat = labelFormat(prefix = "$"),
              opacity = 1) %>%
    addLayersControl(overlayGroups = c("Battle", "Remote violence","Riots/Protests", "Violence against civilians"),
                     options = layersControlOptions(collapsed = FALSE))
  
  
  leaflet(data = acled) %>% 
    setView(26.17, 5.65, zoom = 2) %>%
    addProviderTiles("CartoDB.Positron", group="Greyscale") %>% 
    
    addCircleMarkers(lng = subset(acled, event_type=='Violence against civilians')$longitude, 
                     lat = subset(acled, event_type=='Violence against civilians')$latitude, 
                     group = "Violence against civilians", 
                     popup = subset(acled, event_type=='Violence against civilians')$label, 
                     radius = 5, color = '#e41a1c',
                     clusterOptions = markerClusterOptions()) %>%
    
    addCircleMarkers(lng = subset(acled, event_type=='Riots/Protests')$longitude, 
                     lat = subset(acled, event_type=='Riots/Protests')$latitude, 
                     group = "Riots/Protests", 
                     popup = subset(acled, event_type=='Riots/Protests')$label, 
                     radius = 5, color = '#377eb8', 
                     clusterOptions = markerClusterOptions()) %>%
    
    addCircleMarkers(lng = subset(acled, event_type == "Battle-Government regains territory" |  event_type == "Battle-No change of territory Battle-Non-state actor overtakes territory")$longitude, 
                     lat = subset(acled, event_type == "Battle-Government regains territory" |  event_type == "Battle-No change of territory Battle-Non-state actor overtakes territory")$latitude, 
                     group = "Battle", 
                     popup = subset(acled, event_type == "Battle-Government regains territory" |  event_type == "Battle-No change of territory Battle-Non-state actor overtakes territory")$label, 
                     radius = 5, color ='#4daf4a',
                     clusterOptions = markerClusterOptions()) %>%
    
    addCircleMarkers(lng = subset(acled, event_type=='Remote violence')$longitude, 
                     lat = subset(acled, event_type=='Remote violence')$latitude, 
                     group = "Remote violence", 
                     popup = subset(acled, event_type=='Remote violence')$label, radius = 5, color = '#984ea3', clusterOptions = markerClusterOptions()) %>%
    
    addLayersControl(
      overlayGroups = c("Battle", "Remote violence","Riots/Protests", "Violence against civilians"),
      options = layersControlOptions(collapsed = FALSE))