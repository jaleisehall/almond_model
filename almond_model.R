# enter the almond function here


almond_model <- function(clim, clim_var1 = -0.015, clim_var2 = 0.0046, clim_var3 = 0.07, clim_var4 = 0.0043, intercept = 0.28){
  
  temperature <- clim %>% 
    filter(month == "2") %>% 
    group_by(year) %>% 
    summarize(
      avg = mean(tmin_c)
    ) 
  
  rain <- clim %>% 
    filter(month == "1") %>% 
    group_by(year) %>% 
    summarize(
      sum = sum(precip)
    )
  
  df <- data.frame(temperature, rain) %>% 
    select(-year.1)
  
  final_model <- clim_var1*(df$avg) - clim_var2*((df$avg)^2) - clim_var3*(df$sum) + clim_var4*((df$sum)^2) + intercept

  return(final_model)
}
