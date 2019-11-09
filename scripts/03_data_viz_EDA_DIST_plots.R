  library(geosphere)
  library(ggplot2)
  
  #load small dataset
  data <- read.csv("data/02_redfin_clean_small.csv", 
                   header = TRUE, 
                   stringsAsFactors = FALSE)
  data$price_per_sqft <- data$price/data$square.feet
  #coordinate for US Capitol
  capitol <- c(-77.008991, 38.890089)

  # create distance matrix from each listing to the US Capitol (units = meters)
  dist_m <- distm(data[ c("longitude", "latitude")], capitol, fun =  distGeo)
  
  # add to dataframe and convert to kilometers
  data$dist_2_capitol <- (dist_m/1000)
  # add the meters (might need it, not sure)
  data$dist_2_capitol_m <- dist_m
  
  data$dist_2capitol_mi <- round(dist_m*0.000621371, digit = 4)
  
  rm(dist_m) 
  
  ### HEADING 1
  # fit a linear regression
  linearMod_price_sqft <- lm(square.feet ~ price, data = data)  # build linear regression model on full data
  summary(linearMod_price_sqft)
  
  # plot the data points
  ggplot(data = data, aes(x = data$square.feet, y = data$price/1000, 
                          shape = data$state, colour = data$property.type)) + 
    geom_point(alpha = 0.7) +
    theme_bw() +
    #ggtitle("")+
    xlab("Listed Square Feet") +
    ylab("Listed Price (in thousands of dollars)") +
    labs(shape = "'State'", colour = "Property Type") +
    scale_shape_manual(values = c(0, 16, 3))
  
  ### HEADING 2    
  summary(data$price)
  
  # fit a linear regression
  linearMod_price <- lm(dist_2_capitol_m ~ price, data = data)  # build linear regression model on full data
  summary(linearMod_price)
  
  # plot the data points
  ggplot(data = data, aes(x = data$dist_2_capitol, y = data$price/1000, 
                          shape = data$state, colour = data$property.type)) + 
    geom_point(alpha = 0.7) +
    theme_bw() +
    #ggtitle("")+
    xlab("Distance to US Capitol (in kilometers)") +
    ylab("Listed Price (in thousands of dollars)") +
    labs(shape = "'State'", colour = "Property Type") +
    scale_shape_manual(values = c(0, 16, 3)) +
    # limit graph to show only property below $1.5 million
    ylim(0, 1500)
  
  ### HEADING 3 
  summary(data$square.feet)
  
  # fit a linear regression
  linearMod_sqft <- lm(dist_2_capitol_m ~ square.feet, data = data)  # build linear regression model on full data
  summary(linearMod_sqft)
  
  # plot the data points
  ggplot(data = data, aes(x = data$dist_2_capitol, y = data$square.feet, 
                          shape = data$state, colour = data$property.type)) +
    geom_point(alpha = 0.7) +
    theme_bw() +
    #ggtitle("") +
    xlab("Distance to US Capitol (in kilometers)") +
    ylab("Listing square feet") +
    scale_shape_manual(values = c(0, 16, 3)) +
    labs(shape = "'State'", colour = "Property Type") +
    # limit the graph to show properties below 10,000 square feet
    ylim(0, 10000)
  
  ### HEADING 4 
  summary(data$price_per_sqft)
  
  # fit a linear regression
  linearMod_psqft <- lm(dist_2_capitol ~ price_per_sqft, data = data)  
  summary(linearMod_psqft)
  
  # plot the data points
  ggplot(data = data, aes(y = data$price_per_sqft, 
                          x = 1/data$dist_2_capitol, 
                          shape = data$state, 
                          colour = data$property.type)) +
    geom_point(alpha = 0.7)+
    theme_bw() +
    #ggtitle("")+
    xlab("Distance to US Capitol (in kilometers)") +
    ylab("Price per square foot") +
    labs(shape = "'State'", colour = "Property Type") +
    scale_shape_manual(values = c(0, 16, 3)) +
    facet_grid(. ~ state) +
    xlim(0, 0.75) + 
    ylim(0, 2500)
  
  ### HEADING 5 
  data2 <- data[data$price <= 1000000 & data$square.feet <= 5000, ]
  # fit a linear regression
  linearMod_price_sqft <- lm(square.feet ~ price, data = data2)  # build linear regression model on full data
  summary(linearMod_price_sqft)
  
  # plot the data points
  ggplot(data = data2, aes(x = data2$square.feet, y = data2$price/1000, 
                           shape = data2$state, colour = data2$property.type)) + 
    geom_point(alpha = 0.7) +
    theme_bw() +
    #ggtitle("")+
    xlab("Listed Square Feet") +
    ylab("Listed Price (in thousands of dollars)") +
    labs(shape = "'State'", colour = "Property Type") +
    scale_shape_manual(values = c(0, 16, 3))
