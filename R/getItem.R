#'@title get item or subset of fields
#'@description returns a list of item contents
#'@param itemID a string for a valid item identifier
#'@param field a subfield to return from the item. Default is NULL
#'@return a list or a character vector (depending on \code{field} argument)
#'@importFrom httr GET accept_json content
#'@examples
#'\dontrun{
#'setBaseURL('dev')
#'getItem('CAQw7M1')
#'getItem('CAQw7M1', field = 'children')
#'}
#'@export 
getItem <- function(itemID, field = NULL, subtree = FALSE){
  url = paste0(pkg.env$item_json, itemID)
  if (subtree) {
    url = paste0(url, "?subtree=true")
  }
  response <- GET(url = url, accept_json())
  
  if (response$status_code != 200){
    stop('failed GET on item ', itemID ,'. error code:', response$status_code)
  }
  item <- content(response, as = 'parsed')
  
  if (is.null(field)){
    return(item)
  } else {
    return(item[[field]])
  }
  
}
  