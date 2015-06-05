#'@title save item
#'@description adds or updates an item in the application
#'@param item list representing item to POST or PUT
#'@param ... extra params to pass to checkAuth
#'@return character vector representing id of new item
#'@importFrom httr POST PUT content
#'@importFrom jsonlite toJSON
#'@examples
#'\dontrun{
#'setBaseURL('dev')
#'postItem(list(
#'  itemType="aggregation",
#'  name="test",
#'  type="storms"
#'))
#'}
#'@export 
saveItem <- function(item, ...){
  checkAuth(...)
  url = pkg.env$item_json

  json <- toJSON(item, auto_unbox=TRUE)
  if (is.null(item$id)) {
    response <- POST(url=url, body=json,
                   content_type('application/json'),
                   add_headers('Authorization' = getAuth(), 'Connection'='keep-alive'))
    ok <- content(response, as = "parsed")
  } else {
    url = paste0(url, item$id)
    response <- PUT(url=url, body=json,ontent_type('application/json'),
                    add_headers('Authorization' = getAuth(), 'Connection'='keep-alive'))
    ok <- list(id=item$id)
  }
  if (response$status_code != 200) {
    stop('failed POST on item. error code:', response$status_code)
  }
 
  return(ok$id)
}
  