FitHTTPHeader <- function(token){

  return(c(Authorization = paste("Bearer",token,sep = ' ')))

  }

ValidateDateRange <- function(startDate, endDate) {

  stopifnot(inherits(startDate,"POSIXct"))

  stopifnot(inherits(endDate,"POSIXct"))

}

ValidateDatasource <- function(datasource) {
  
  stopifnot(length(datasource)==1)
  
}

EpochTime <- function(inDate) {
  
  return (as.integer64(as.numeric(inDate) * 1000000000))
  #return (paste(as.numeric(inDate),"000000000",sep = ""))
  
}

# ----------------------- Sat Oct 19 09:51:28 2019 ------------------------#
# Is legit token
# Credit to googlesheets
is_legit_token <- function (x, verbose = FALSE) 
{
  if (!inherits(x, "Token2.0")) {
    if (verbose) 
      message("Not a Token2.0 object.")
    return(FALSE)
  }
  if ("invalid_client" %in% unlist(x$credentials)) {
    if (verbose) {
      message("Authorization error. Please check client_id and client_secret.")
    }
    return(FALSE)
  }
  if ("invalid_request" %in% unlist(x$credentials)) {
    if (verbose) 
      message("Authorization error. No access token obtained.")
    return(FALSE)
  }
  TRUE
}

#' @title NanosToPOSIXct
#' @rdname NanosToPOSIXct
#' @export
#'
#' @param nanos - Nanoseconds from epoch
#' @description
#' Converts nanoseconds from epoch (as provided by Google Fit) to POSIXct
#' @examples
#' NanosToPOSIXct(1388534400000000000)

NanosToPOSIXct <- function(nanos) {
  
  return (as.POSIXct(round(as.integer64(nanos) /
                              as.integer64(1000000000)),
                     origin = "1970-01-01")
  )
  
}

assert <- function (expr, error) {
  if (! expr) stop(error, call. = FALSE)
}

