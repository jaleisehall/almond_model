#' Almond Function
#' 
#' This function calculates the California almond yield anomaly using the following regression function from the Lobell 2006 paper: Y = -0.015T~n,2~ - 0.0046T^2^~n,2~ - 0.07P~1~ + 0.0043P^2^~1~ + 0.28
#' @param clim Dataframe that contains the climate data including time (year, month, day), precipitation, and temperature
#' @param clim_var1 Minimum temperature (C) in Febuary constant in regression function
#' @param clim_var2 Minimum temperature (C) in Febuary constant in regression function
#' @param clim_var3 Precipitation (mm) in January constant in regression function
#' @param clim_var4 Precipitation (mm) in January constant in regression function
#' @param intercept The intercept in the regression function
#' @author Anna Abelman, Julia Dagum, Jaleise Hall
#' @example almond_model(clim)
#' @return California almond yield anomaly (ton acre^-1)

almond_model <- function(clim, clim_var1 = -0.015, clim_var2 = 0.0046, clim_var3 = 0.07, clim_var4 = 0.0043, intercept = 0.28){
  # find the average minimum temperature per year in February
  temperature <- clim %>% 
    filter(month == "2") %>% #filter to only have February 
    group_by(year) %>% 
    summarize(
      avg = mean(tmin_c) 
    ) 
  
  # find the total sum of precipiation in January for each year
  rain <- clim %>% 
    filter(month == "1") %>% #filter to only have January
    group_by(year) %>% 
    summarize(
      sum = sum(precip)
    )
  
  # combine into one data frames
  df <- data.frame(temperature, rain) %>% 
    select(-year.1)
  
  #calculate final yield anomaly using regression function
  final_model <- clim_var1*(df$avg) - clim_var2*((df$avg)^2) - clim_var3*(df$sum) + clim_var4*((df$sum)^2) + intercept

  #return(final_model)
  return(list(c(final_model,df)))
  
}
