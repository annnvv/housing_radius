
  library(readr)
  library(here)
  library(ggplot2)

  df <- readr::read_csv(here('GitHub/housing_radius/data/02_redfin_clean_all.csv'))  
  df2 <- df[df$year.built >= 2000 & df$property.type == 'Condo/Co-op' , 
            c('square.feet', 'year.built', 'property.type', 'beds' , 'state')]
  
  df2$square.feet[df2$square.feet == 23584] <- NA
  df2$year.built[df2$year.built <= 1802] <- NA
  df2_condo <- df2[df2$property.type == 'Condo/Co-op', ]
  
  #### PLOTS
  plot_sqft_yr_built <- function(type){
    print(ggplot(data = df[df$property.type == type, ], 
                 aes(y = square.feet,  x = year.built, col = state)) +
      geom_point(alpha = 0.5) +
      labs(title = paste0(type))
    )
  }
  
  property_types <- unique(df$property.type)
  
  for(i in property_types){
    plot_sqft_yr_built(i)
  }
  
  #### LINEAR REGRESSIONS
  # YEAR BUILT VERSUS SQUARE-FEET
  lm <-  lm(square.feet~year.built, data = df)
  summary(lm) #statistically sigificant relationship
  
  
  plot(df[ , c('year.built', 'square.feet')], 
       pch = 16, col = "blue")
  abline(lm)

  
  # YEAR BUILT VERSUS SQUARE-FEET (control for state, property type)
  lm2 <-  lm(square.feet~year.built + state + property.type, data = df)
  summary(lm2) #statistically sigificant relationship
  
  plot(df[ , c('year.built', 'square.feet')], 
       pch = 16, col = "green")
  abline(lm2)
  
  # YEAR BUILT VERSUS SQUARE-FEET - DC CONDOS built after 1999
  lm_df_condo <-  lm(square.feet~year.built, data = df2)
  summary(lm_df_condo) #no statistically significant relationship
  
  
  plot(df2[ , c('year.built', 'square.feet')], 
       pch = 16, col = "red")
  abline(lm)
  
  