  # load data
  data <- read.csv("data/02_redfin_clean_small.csv", header = TRUE, stringsAsFactors = FALSE)
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
  write.csv(merged, "data/03_zip_property_data.csv", row.names = FALSE)
  
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
  