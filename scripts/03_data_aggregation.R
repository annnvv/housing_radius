  #Step 3: Aggregate data by zipcode and property type
  
  # load data
  data <- read.csv("redfin_clean_small.csv", header = TRUE, stringsAsFactors = FALSE)
  data$freq <- 1
  
  # creating datasets by zipcode and property type
  obs <- aggregate(data$freq, by = list(data$zip_code, data$property.type), FUN = "sum")
  names(obs) <- c("zip_code", "property_type", "properties")
  
  median_sq_ft <- aggregate(data$square.feet, by = list(data$zip_code, data$property.type), FUN = "median", na.rm = TRUE)
  names(median_sq_ft) <- c("zip_code", "property_type", "median_sq_ft")
  
  mean_sq_ft <- aggregate(data$square.feet, by = list(data$zip_code, data$property.type), FUN = "mean", na.rm = TRUE)
  names(mean_sq_ft) <- c("zip_code", "property_type", "mean_sq_ft")
  
  median_price <- aggregate(data$price, by = list(data$zip_code, data$property.type), FUN = "median", na.rm = TRUE)
  names(median_price) <- c("zip_code", "property_type", "median_price")
  
  mean_price <- aggregate(data$price, by = list(data$zip_code, data$property.type), FUN = "mean", na.rm = TRUE)
  names(mean_price) <- c("zip_code", "property_type", "mean_price")
  
  # merging all datasets into one (unit of analysis is zipcode-property type)
  merged <- merge(obs, median_sq_ft, by = c("zip_code", "property_type"), all = TRUE)
  merged <- merge(merged, mean_sq_ft, by = c("zip_code", "property_type"), all = TRUE)
  merged <- merge(merged, median_price, by = c("zip_code", "property_type"), all = TRUE)
  merged <- merge(merged, mean_price, by = c("zip_code", "property_type"), all = TRUE)
  
  rm(obs, median_sq_ft, median_price, mean_sq_ft, mean_price)
  
  # for zipcodes that have few than three properties make the data NA
  table(merged$properties<3, merged$property_type)
  merged$median_sq_ft[merged$properties<3] <- NA
  merged$mean_sq_ft[merged$properties<3] <- NA
  merged$median_price[merged$properties<3] <- NA
  merged$mean_price[merged$properties<3] <- NA
  
  # write .csv with the zipcode-property level data
  write.csv(merged, "zip_property_data.csv", row.names = FALSE)
  
