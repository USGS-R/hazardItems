#'@title get alias
#'@description returns a list of alias contents
#'@param aliasID a string for a valid alias identifier
#'@return a list
#'@importFrom httr GET accept_json content
#'@examples
#'\dontrun{
#'setBaseURL('dev')
#'getAlias('test')
#'}
#'@export 
getAlias <- function(aliasID){
  url = paste0(pkg.env$alias_json, aliasID)
  
  response <- GET(url = url, accept_json())
  
  if (response$status_code != 200){
    stop('failed GET on alias ', aliasID ,'. error code:', response$status_code)
  }
  
  alias <- content(response, as = 'parsed')
  
  return(alias)
}

#'@title alias exists
#'@description returns whether or not an alias with the specified ID exists
#'@param aliasID a string for a valid alias identifier
#'@return a boolean
#'@examples
#'\dontrun{
#'setBaseURL('dev')
#'aliasExists('test')
#'}
#'@export 
aliasExists <- function(aliasID){
  tryCatch({
    getAlias(aliasID)
    return(TRUE)
  }, error=function(e) {
    return(FALSE)
  })
}

