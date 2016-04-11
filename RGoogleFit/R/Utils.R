FitHTTPHeader <- function(token){

  return(c(Authorization = paste("Bearer",token,sep = ' ')))

  }

ValidateDateRange <- function(startDate, endDate) {

  stopifnot(inherits(startDate,"Date"))

  stopifnot(inherits(endDate,"Date"))

}
