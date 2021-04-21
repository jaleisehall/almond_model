#' compute_NPV
#' 
#' This function is calculating the net present value provided in class.
#' @param value/cost ($)
#' @param time in the future that cost/value occurs (years)
#' @param discount rate 
#' @return value in $


compute_NPV = function(value, time, discount=0.12) {
  result = value / (1 + discount)**time
  return(result)
}