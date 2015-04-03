#'@title check auth token and prompt for refresh if needed
#'@param updateIfStale should \code{\link{authenticateUser}} 
#'be called if token is stale?
#'@param ... additional arguments passed to \code{\link{authenticateUser}}
#'@return boolean for auth token state
#'@seealso \code{\link{authenticateUser}}
#'@keywords internal
checkAuth <- function(updateIfStale = TRUE, ...){
  if (is.null(pkg.env$authToken)){
    stop('user is not authenticated, see authenticateUser')
  }
  auth = paste('Bearer', pkg.env$authToken)
  resp <- GET(pkg.env$auth_check, add_headers('Authorization' = auth))
  
  if (resp$status_code == 200){
    return(TRUE)
  } else {
    if (updateIfStale){
      authenticateUser(...)
      return(TRUE)
    }
    return(FALSE)
  }
}