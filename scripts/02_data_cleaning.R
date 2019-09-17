  # Step 2: Data Cleaning
  library(leaflet)
  
  # get a list of all file in this directory
  redfin_files <- list.files("C:/Users/Anna V/Documents/GitHub/housing_radius/data/redfin_raw_20190916", 
                             pattern = ".csv$", full.names = TRUE)
  # data pulled on 2019-09-16
  
  # read in all of the .csv file into a dataframe
  redfin_raw <- c()
  for (i in redfin_files){
    one_file <- read.csv(i, stringsAsFactors = FALSE, header = TRUE)
    redfin_raw <- rbind(redfin_raw, one_file)
  }
  rm(i, one_file, redfin_files)
  
  #### DATA CLEANING ####
  redfin <- redfin_raw
  
  # rename some variables
  names(redfin)
  names(redfin) <- tolower(names(redfin))
  names(redfin)[6] <- "state"
  names(redfin)[7] <- "zip_code"
  names(redfin)[16] <- "price_per_sq_ft"
  names(redfin)[21] <- "url"
  
  length(unique(redfin$zip_code))
  
  #redfin$location <- toTitleCase(redfin$location )
  
  # remove variables that aren't necessary:
  # "sold.date", "interested", "favorite", "next.open.house.start.time", "next.open.house.end.time"
  redfin <- redfin[ , c(1, 3:18, 21:23, 26:27)]
  
  # drop duplicate listings (surprising number of duplicates)
  redfin <- redfin[!duplicated(redfin), ]
  
  nrow(redfin_raw) - nrow(redfin)
  #1488 duplicates
  
  # check status of missing observations
  missing_obs <- function(df){
    # Description:
    #   A function that calculates the number of missing observations for each variable in dataframe df
    # Args:
    #   df: a dataframe
    # Returns:
    #   Displays number of missing observations for each variable in dataframe df
    
    sapply(df, function(x) sum(is.na(x))) 
  }
  
  missing_obs(redfin)
  
  # keep only listings in DC/MD/VA
  table(redfin$state)
  redfin <- redfin[(redfin$state == "MD" | redfin$state == "VA" | redfin$state == "DC"), ]
  
  # drop certain listings property types, specifically parking, vacant land, and ranch
  table(redfin$property.type)
  # 21 parking, 2 ranch, 678 vacant land
  
  redfin <- redfin[(redfin$property.type != "Parking" & redfin$property.type != "Vacant Land"& redfin$property.type != "Ranch"), ]
  
  # check for reasonable square feet
  summary(redfin$square.feet, na.rm = TRUE)
  
  # make any listings with more than 25,000 sq.ft. (3 listings) and any property with less than 100 sq. ft be NA (10 listings)
  table(redfin$square.feet > 25000)
  redfin$square.feet[redfin$square.feet > 25000] <- NA
  table(redfin$square.feet < 100)
  redfin$square.feet[redfin$square.feet < 100] <- NA
  redfin$price_per_sq_ft[redfin$price_per_sq_ft > 17000] <- NA
  
  summary(redfin$square.feet, na.rm = TRUE)
  
  # look at the distribution of the listings on a map 
  # (i.e. make sure all appropriate zipcodes are actually there)
  leaflet() %>%
    # centered on US Capitol
    setView(lng = -77.008989, lat = 38.890468, zoom = 9) %>% 
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(lng = redfin$longitude, lat = redfin$latitude, radius = 0.5, color = "blue")
  
  # write a .csv with all the variables
  write.csv(redfin, "data/02_redfin_clean_all.csv", row.names = FALSE) 
  
  # write a .csv with only necessary variables
  write.csv(redfin[, c("property.type", "city", "state", "zip_code", "price", "square.feet", "latitude", "longitude")], 
            "data/02_redfin_clean_small.csv", row.names = FALSE)
