#' @title GetFitOauth2Token
#' @rdname GetFitOauth2Token
#' @description
#' Retrieves or refreshes an OAuth2 token. Two options must be set or provided as arguments:
#' \itemize{
#' \item \code{RGoogleFit.client_id}
#' \item \code{RGoogleFit.client_secret}
#' }
#' Requires setup of API Credentials. A good tutorial for setting up Google REST API credentials can be found here: [How to authenticate using OAuth2 through R](https://biolitika.si/how-to-authenticate-using-oauth2-through-r.html). Simply do these steps for the Google Fitness API.
#' @param client_id Client ID from console.developers.google.com
#' @param client_secret Client Secret from console.developers.google.com
#' @param scopes Scopes required for requests. List of scopes available on the [Google Fitness REST API Documentation](https://developers.google.com/fit/rest/v1/authorization)
#' @param use_oob if FALSE (the default), use a local webserver (http://localhost/) for the OAuth dance. Otherwise, provide a URL to the user and prompt for a validation code 
#' @param cache A logical value or a string. TRUE (the default) means to cache using the default cache file .httr-oauth, FALSE means don't cache, and NA means to guess using some sensible heuristics. A string means use the specified path as the cache file.
#' @export


GetFitOauth2Token <- function(client_id = NULL, client_secret = NULL, scopes = NULL, use_oob = F, cache = T) { 
  if (is.null(client_id)) client_id = getOption("RGoogleFit.client_id")
  if (is.null(client_secret)) client_secret = getOption("RGoogleFit.client_secret")
  if (is.null(scopes)) scopes = c("https://www.googleapis.com/auth/fitness.activity.read", 
                                  "https://www.googleapis.com/auth/fitness.location.read",
                                  "https://www.googleapis.com/auth/fitness.body.read")
  checkmate::assert(!is.null(client_id), "Please specify client_id or set 'RGoogleFit.client_id' option")
  checkmate::assert(!is.null(client_secret), "Please specify client_secret or set 'RGoogleFit.client_secret' option")
  checkmate::assert(!is.null(client_secret), "Please specify scope or set 'RGoogleFit.scope' option")
  
  oauth2_token <- httr::oauth2.0_token(endpoint = httr::oauth_endpoints("google"), 
                                       app = httr::oauth_app("google", key = client_id, 
                                                             secret = client_secret), 
                                       scope = scopes, 
                                       use_oob = use_oob,
                                       cache = cache)
  stopifnot(is_legit_token(oauth2_token, verbose = TRUE))
  
  if (!oauth2_token$validate()) {
    oauth2_token$refresh()
  }
  RGoogleFitObjects$oauth2_object <- oauth2_token
  return(oauth2_token)
}