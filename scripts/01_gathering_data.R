  # Step 1: Scrape data
  library(rvest)

  dc_zipcodes_url <- "https://www.zillow.com/browse/homes/dc/district-of-columbia-county/" 
  arl_zip_url <- "https://www.zillow.com/browse/homes/va/arlington-county/"
  fairfax_zip_url <- "https://www.zillow.com/browse/homes/va/fairfax-county/"
  alex_zip_url <- "https://www.zillow.com/browse/homes/va/alexandria-city/"
  mont_zip_url <- "https://www.zillow.com/browse/homes/md/montgomery-county/"
  pg_zip_url <- "https://www.zillow.com/browse/homes/md/prince-georges-county/"
  
  zip_urls <- c(dc_zipcodes_url, arl_zip_url, fairfax_zip_url, alex_zip_url, mont_zip_url, pg_zip_url)
  rm(dc_zipcodes_url, arl_zip_url, fairfax_zip_url, mont_zip_url, pg_zip_url, alex_zip_url)
  
  ### define function to get the text of a url link
  get_text <- function(url, node){
    # A function to get text from webpage
    # Args:
    #       url: a url to a webpage
    #       node: the node where to look for the text
    # Returns:
    #       a vector with desired text 
    
    url_s <- read_html(url) %>% 
      rvest::html_nodes(node) %>% 
      rvest::html_text()
    
    return(url_s)
  }
  
  zipcodes <- c()
  for (i in zip_urls){
    #print(http_status(GET(i)))
    zip_one <- get_text(i, "a")
    zipcodes <- c(zipcodes, zip_one)
  }
  rm(i, zip_one)
  
  zipcodes <- zipcodes[grepl("^2", zipcodes)]
  write.csv(zipcodes, "data/01_zipcodes.csv", row.names = FALSE)
  
  redfin_host <- "https://www.redfin.com/zipcode/"
  
  redfin_zip <- c()
  for (i in zipcodes){
    one_url <- paste0(redfin_host, i)
    redfin_zip <- c(redfin_zip, one_url)
  }
  rm(i, one_url)
  
#### Download data from zipcodes
  ## define function
  get_data <- function(url){
    # A function to get the hyperlinks to the download all listings
    # Args:
    #   url: a url that has the urls to the download all listings
    #   node: the node where to look for the hyperlink to the download all listings
    #   attr: which attribute to extract from the node
    # Returns:
    #   a vector that has all of the hyperlinks to the profile pages
    
    url_s <- read_html(url) %>% 
      html_nodes(xpath = '//*[(@id = "download-and-save")]') %>%
      xml_attr("href")
  }
  
  download_links <- c()
  for (i in redfin_zip){
    one <- get_data(i)
    one <- paste0("https://redfin.com", one)
    download_links <- c(download_links, one)
  }
  rm(i, one)  
  
  download_links_clean <- download_links[!grepl("^https://redfin.com$", download_links)]
  write.csv(download_links_clean, "data/01_download_links_clean.csv", row.names = FALSE)
  
  # get data from download_links 
  # (this is how I would do it, but Redfin terms of service won't allow it)
  # so I downloaded all of the .csvs manually
  
  data <- c()
  for (i in download_links){
    #data_one <- read.csv(i, stringsAsFactors = FALSE, header = TRUE)
    #data <- rbind(data, data_one)
    #Sys.sleep(runif(n=1, min = 1, max = 5))
    print(i)
  }
  rm(i, data_one)
  