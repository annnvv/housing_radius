  library(rgdal)
  library(raster)
  
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
  writeOGR(zips, dsn = paste0(getwd(), "/data/shapefiles"), 
           layer = "DMV_zipcodes", 
           driver = "ESRI Shapefile", overwrite_layer = TRUE)
  
  # view the zipcode object
  plot(zips)
  
  # find the number of zipcodes represented here
  zipcodes <- unique(zips$zipcode)
  
  write.csv(zipcodes, paste0(gsub("shapefiles", "", shp_path), "/01_md_va_dc_zipcodes.csv"), 
            row.names = FALSE)
  
  # # merge zipcode data and shapefile
  # merged_shp <- merge(zips, merged_wide, by = "zipcode")
  # 
  # # write object to a shapefile
  # writeOGR(merged_shp, dsn = getwd(), layer = "DMV_zipcodes_housing_data", 
  #          driver = "ESRI Shapefile", overwrite_layer = TRUE)