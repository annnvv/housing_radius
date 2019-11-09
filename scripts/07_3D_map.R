  # 3D barplot on a map
  library(mapdeck)

  data <- read.csv("data/02_redfin_clean_all.csv", header = TRUE, stringsAsFactors = FALSE)
  data <- data[ , c("property.type", "price", "square.feet", "price_per_sq_ft","latitude", "longitude", "url")]

  price <- data[ , c("property.type", "price", "latitude", "longitude")]  
  names(price)[1] <- "Property Type"
  price_u2m <- price[price$price < 2000000, ]

  sqft <- data[ , c("property.type", "price", "square.feet", "latitude", "longitude")]  
  names(sqft)[1] <- c("Property Type")
  
  sqft_u2m_price <- sqft[sqft$price < 2000000, ]
  sqft <- sqft[ , c("Property Type", "square.feet", "latitude", "longitude")]  
  
  #price_sq_ft <- data[ , c("property.type", "price_per_sq_ft", "latitude", "longitude")]  

  #### PRICE ####
  mapdeck(
    style = mapdeck_style('light')
  ) %>%
    add_column(
      data = price
      , lat = "latitude"
      , lon = "longitude"
      , radius = 75
      , elevation = "price"
      , elevation_scale = 0.001
      , layer_id = "price"
      , auto_highlight = TRUE
      , update_view = TRUE
      , fill_colour = "Property Type"
      , fill_opacity = 0.65
      , tooltip = "price"
      , legend = TRUE) %>%
    mapdeck_view(location = c(-77.008989, 38.890468), pitch = 60, zoom = 9)  

  mapdeck(
    style = mapdeck_style('light')) %>%
    add_column(
      data = price_u2m
      , lat = "latitude"
      , lon = "longitude"
      , radius = 75
      , elevation = "price"
      , elevation_scale = 0.001
      , layer_id = "price_under2m"
      , auto_highlight = TRUE
      , update_view = TRUE
      , fill_colour = "Property Type"
      , fill_opacity = 0.65
      , tooltip = "price"
      , legend = TRUE) %>%
    mapdeck_view(location = c(-77.008989, 38.890468), pitch = 60, zoom = 9)
  
  #### Square Feet ####
  mapdeck(
    style = mapdeck_style('light')) %>%
    add_column(
      data = sqft
      , lat = "latitude"
      , lon = "longitude"
      , radius = 75
      , elevation = "square.feet"
      , elevation_scale = 0.25
      , layer_id = "sqft"
      , auto_highlight = TRUE
      , update_view = TRUE
      , fill_colour = "Property Type"
      , fill_opacity = 0.65
      , tooltip = "square.feet"
      , legend = TRUE) %>%
    mapdeck_view(location = c(-77.008989, 38.890468), pitch = 120, zoom = 9)  
  

  mapdeck(
    style = mapdeck_style('light')) %>%
    add_column(
      data = sqft_u2m_price
      , lat = "latitude"
      , lon = "longitude"
      , radius = 75
      , elevation = "square.feet"
      , elevation_scale = 0.01
      , layer_id = "sqft_u2m_price"
      , auto_highlight = TRUE
      , update_view = TRUE
      , fill_colour = "Property Type"
      , fill_opacity = 0.65
      , tooltip = "square.feet"
      , legend = TRUE) %>%
    mapdeck_view(location = c(-77.008989, 38.890468), pitch = 60, zoom = 9)
  
  
  #   library(RColorBrewer)
  #   mapdeck(
  #   style = mapdeck_style('dark')
  #   # , pitch = 45
  #   # , zoom = 5
  # ) %>%
  #   add_hexagon(
  #     data = price[price$property.type == "Condo/Co-op", ]
  #     , lat = "latitude"
  #     , lon = "longitude"
  #     , radius = 250
  #     , elevation_scale = 100
  #     , layer_id = "price_condo"
  #     , auto_highlight = TRUE
  #     , update_view = TRUE
  #     , colour_range = RColorBrewer::brewer.pal(6, "YlOrRd")
  #   )   %>%
  #   add_hexagon(
  #     data = price[price$property.type == "Townhouse", ]
  #     , lat = "latitude"
  #     , lon = "longitude"
  #     , radius = 250
  #     , elevation_scale = 100
  #     , layer_id = "price_townhouse"
  #     , auto_highlight = TRUE
  #     , update_view = TRUE
  #     , colour_range = RColorBrewer::brewer.pal(6, "Blues")
  #   ) %>%
  #   add_hexagon(
  #     data = price[price$property.type == "Single Family Residential"|price$property.type == "Multi-Family (2-4 Unit)", ]
  #     , lat = "latitude"
  #     , lon = "longitude"
  #     , radius = 250
  #     , elevation_scale = 100
  #     , layer_id = "price_house"
  #     , auto_highlight = TRUE
  #     , update_view = TRUE
  #     , colour_range = RColorBrewer::brewer.pal(6, "Greens")
  #   ) %>%
  # mapdeck_view(location = c(-77.008989, 38.890468), pitch = 45, zoom=8)
      