  #Step 3: Aggregate data by zipcode and property type

  library(raster)
  library(sp) 
  library(rgdal)
  
  # load data
  data <- read.csv("02_redfin_clean_small.csv", header = TRUE, stringsAsFactors = FALSE)
  data$freq <- 1
  
  # creating datasets by zipcode and property type
  obs <- aggregate(data$freq, by = list(data$zip_code, data$property.type), 
                   FUN = "sum")
  names(obs) <- c("zip_code", "property_type", "properties")
  
  median_sq_ft <- aggregate(data$square.feet, 
                            by = list(data$zip_code, data$property.type), 
                            FUN = "median", na.rm = TRUE)
  names(median_sq_ft) <- c("zip_code", "property_type", "median_sq_ft")
  
  mean_sq_ft <- aggregate(data$square.feet, by = list(data$zip_code, data$property.type), 
                          FUN = "mean", na.rm = TRUE)
  names(mean_sq_ft) <- c("zip_code", "property_type", "mean_sq_ft")
  
  median_price <- aggregate(data$price, by = list(data$zip_code, data$property.type), 
                            FUN = "median", na.rm = TRUE)
  names(median_price) <- c("zip_code", "property_type", "median_price")
  
  mean_price <- aggregate(data$price, by = list(data$zip_code, data$property.type), 
                          FUN = "mean", na.rm = TRUE)
  names(mean_price) <- c("zip_code", "property_type", "mean_price")
  
  # merging all datasets into one (unit of analysis is zipcode-property type)
  merged <- merge(obs, median_sq_ft, 
                  by = c("zip_code", "property_type"), all = TRUE)
  merged <- merge(merged, mean_sq_ft, 
                  by = c("zip_code", "property_type"), all = TRUE)
  merged <- merge(merged, median_price, 
                  by = c("zip_code", "property_type"), all = TRUE)
  merged <- merge(merged, mean_price, 
                  by = c("zip_code", "property_type"), all = TRUE)
  
  rm(obs, median_sq_ft, median_price, mean_sq_ft, mean_price)
  
  # for zipcodes that have few than three properties make the data NA
  table(merged$properties<3, merged$property_type)
  merged$median_sq_ft[merged$properties<3] <- NA
  merged$mean_sq_ft[merged$properties<3] <- NA
  merged$median_price[merged$properties<3] <- NA
  merged$mean_price[merged$properties<3] <- NA
  
  # write .csv with the zipcode-property level data
  write.csv(merged, "03_zip_property_data.csv", row.names = FALSE)
  
  # reshape merged object
  merged2 <- merged[complete.cases(merged), ]
  merged2 <- merged2[merged2$property_type != "Multi-Family (2-4 Unit)", ]
  
  merged2$property_type[merged2$property_type == "Condo/Co-op"] <- "Condo_Co_op"
  merged2$property_type[merged2$property_type == "Single Family Residential"] <- "Single_Family"
  
  merged_wide <- reshape(merged2, 
                         idvar = "zip_code",
                         timevar = "property_type",
                         #varying = list("properties", "median_sq_ft", "mean_sq_ft", "median_price", "mean_price"),
                         direction = "wide")
  names(merged_wide)
  
  # creating labels to be merged with the shapefile and displayed on the leaflet map
  merged_wide$label_median <- paste(sep = "<br/>", paste0("<b>Zipcode</b> ", merged_wide$zip_code), 
                                    paste0("<b>Condo Median Price: </b> ", round(merged_wide$median_price.Condo_Co_op)), 
                                    paste0("<b>Condo Median Sq. ft.: </b> ", round(merged_wide$median_sq_ft.Condo_Co_op)), 
                                    paste0("Condo # of Properties: ", merged_wide$properties.Condo_Co_op),
                                    paste0("<b>Townhouse Median Price: </b> ", round(merged_wide$median_price.Townhouse)), 
                                    paste0("<b>Townhouse Median Sq. ft.: </b> ", round(merged_wide$median_sq_ft.Townhouse)), 
                                    paste0("Townhouse # of Properties: ", merged_wide$properties.Townhouse),
                                    paste0("<b>Single Family Median Price: </b> ", round(merged_wide$median_price.Single_Family)), 
                                    paste0("<b>Single Family Median Sq. ft.: </b> ", round(merged_wide$median_sq_ft.Single_Family)),
                                    paste0("Single Family # of Properties: ", merged_wide$properties.Single_Family))

  merged_wide$label_mean <- paste(sep = "<br/>", paste0("<b>Zipcode</b> ", merged_wide$zip_code), 
                                  paste0("<b>Condo Mean Price: </b> ", round(merged_wide$mean_price.Condo_Co_op)), 
                                  paste0("<b>Condo Mean Sq. ft.: </b> ", round(merged_wide$mean_sq_ft.Condo_Co_op)), 
                                  paste0("Condo # of Properties: ", merged_wide$properties.Condo_Co_op),
                                  paste0("<b>Townhouse Mean Price: </b> ", round(merged_wide$mean_price.Townhouse)), 
                                  paste0("<b>Townhouse Mean Sq. ft.: </b> ", round(merged_wide$mean_sq_ft.Townhouse)), 
                                  paste0("Townhouse # of Properties: ", merged_wide$properties.Townhouse),
                                  paste0("<b>Single Family Mean Price: </b> ", round(merged_wide$mean_price.Single_Family)), 
                                  paste0("<b>Single Family Mean Sq. ft.: </b> ", round(merged_wide$mean_sq_ft.Single_Family)),
                                  paste0("Single Family # of Properties: ", merged_wide$properties.Single_Family))
  
  names(merged_wide)[1] <- "zipcode"
  
  # Clean up zipcodes shapefile
  # load zipcode shapefiles
  shp_path <- "C:/Users/Anna V/Documents/GitHub/housing_radius/data/shapefiles"
  zip_dc <- readOGR(paste0(shp_path, "/Zip_Codes.shp"))
    zip_dc <- zip_dc[ ,c("ZIPCODE")]
    names(zip_dc) <- c("zipcode")
    zip_dc$county <- "DC"
    zip_dc <- spTransform(zip_dc, CRS("+init=epsg:4326"))
    crs(zip_dc)
    
  zip_pg <- readOGR(paste0(shp_path, "/Zip_Code_Py.shp"))
    zip_pg <- zip_pg[ ,c("ZIP_CODE")]
    names(zip_pg) <- c("zipcode")
    zip_pg$county <- "Prince George's"
    zip_pg <- spTransform(zip_pg, CRS("+init=epsg:4326"))
    crs(zip_pg)
    
  zip_mont <- readOGR(paste0(shp_path, "/mont_county.shp"))
    zip_mont <- zip_mont[ , c("zipcode")]
    names(zip_mont) <- c("zipcode")
    zip_mont$county <- "Montgomery"
    zip_mont <- spTransform(zip_mont, CRS("+init=epsg:4326"))
    crs(zip_mont)
    
  zip_fair <- readOGR(paste0(shp_path, "/fairfax_ZIP_Codes.shp"))
    zip_fair <- zip_fair[ ,c("ZIPCODE")]
    names(zip_fair) <- c("zipcode")
    zip_fair$county <- "Fairfax"
    zip_fair <- spTransform(zip_fair, CRS("+init=epsg:4326"))
    crs(zip_fair)
  
  zip_arg <- readOGR(paste0(shp_path, "/ZipCode_Polygons.shp"))
    zip_arg <- zip_arg[ ,c("ZIP5DIG")]
    names(zip_arg) <- c("zipcode")
    zip_arg$county <- "Arlington"
    zip_arg <- spTransform(zip_arg, CRS("+init=epsg:4326"))
    crs(zip_arg)
  
  # bind all of the county zipcode shapefiles together
  zips <- raster::bind(zip_dc, zip_mont)
  zips <- raster::bind(zips, zip_pg)
  zips <- raster::bind(zips, zip_fair)
  zips <- raster::bind(zips, zip_arg)
  
  zips <- spTransform(zips, CRS("+init=epsg:4326"))
  crs(zips)
  
  rm(zip_dc, zip_mont, zip_pg, zip_fair, zip_arg)
  
  # write the object to a shapefile
  writeOGR(zips, dsn = getwd(), layer = "DMV_zipcodes", 
           driver = "ESRI Shapefile", overwrite_layer = TRUE)
  
  # view the zipcode object
  plot(zips)
  # find the number of zipcodes represented here
  length(unique(zips$zipcode))
  
  # merge zipcode data and shapefile
  merged_shp <- merge(zips, merged_wide, by = "zipcode")
  
  # write object to a shapefile
  writeOGR(merged_shp, dsn = getwd(), layer = "DMV_zipcodes_housing_data", 
           driver = "ESRI Shapefile", overwrite_layer = TRUE)
  