#'@title Checks to see if an item exists
#'@description returns a boolean success 
#'@param itemID id of item to check
#'@param ... extra parameters to pass to checkAuth
#'@return returns TRUE if the item exists, and false if it does not
#'@importFrom httr GET
#'
#'@examples 
#'\dontrun{
#'setBaseURL('dev')
#'checkItemExists('CCGftiy')
#'}
#'@export
checkItemExists <- function(itemID) {
  
  response <- GET(url = paste0(pkg.env$item_json, itemID))
  
  return(http_status(response)$category == "Success")
  
  
}