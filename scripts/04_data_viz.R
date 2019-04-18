# Step 4: Data Visualization
library(leaflet)

# load zipcode shapefile
zip <- readOGR("C:/Users/Anna V/Documents/GitHub/housing_radius/data/shapefiles/tl_2015_us_zcta510.shp")
#zip <- zip[ ,c("OBJECTID", "ZIPCODE")]

#https://www.r-graph-gallery.com/4-tricks-for-working-with-r-leaflet-and-shiny/