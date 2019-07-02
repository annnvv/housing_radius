  library(geosphere)
  library(ggplot2)
  
  #load small dataset
  data <- read.csv("02_redfin_clean_small.csv", 
                   header = TRUE, 
                   stringsAsFactors = FALSE)
  
  #coordinate for lincoln memorial
  lincoln <- c(-77.050219, 38.889218)

  # create distance matrix from each listing to the lincoln memorial (units = meters)
  dist_m <- distm(data[ c("longitude", "latitude")], lincoln, fun =  distGeo)
  
  # add to dataframe and convert to kilometers
  data$dist_2_lincoln <- (dist_m/1000)
  # add the meters (might need it, not sure)
  data$dist_2_lincoln_m <- dist_m

  rm(dist_m) 
  
  summary(data$price)

  # fit a linear regression
  linearMod_price <- lm(dist_2_lincoln_m ~ price, data = data)  # build linear regression model on full data
  summary(linearMod_price)
  
  # plot the data points
  ggplot(data = data, aes(x = data$dist_2_lincoln, y = data$price/100000, colour = data$state)) +
    geom_point() +
    theme_bw()
  
  summary(data$square.feet)
  
  # fit a linear regression
  linearMod_sqft <- lm(dist_2_lincoln_m ~ square.feet, data = data)  # build linear regression model on full data
  summary(linearMod_sqft)

  # plot the data points
  ggplot(data = data, aes(x = data$dist_2_lincoln, y = data$square.feet, colour = data$state)) +
    geom_point()+
    theme_bw()
  
  
   
 