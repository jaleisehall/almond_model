# enter the almond function here


almond_model <- function(temp, prep, clim_var1 = -0.015, clim_var2 = 0.0046, clim_var3 = 0.07, clim_var4 = 0.0043, intercept = 0.28){
  
  final_model = clim_var1*(temp) - clim_var2*((temp)^2) - clim_var3*(prep) + clim_var4*((prep)^2) + intercept
  
  return(final_model)
  
}