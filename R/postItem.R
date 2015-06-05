#'@title POST item
#'@description adds an item to application
#'@param item list representing item to POST
#'@param ... extra params to pass to checkAuth
#'@return character vector representing id of new item
#'@importFrom httr POST content
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
postItem <- function(item, ...){
  checkAuth(...)
  url = pkg.env$item_json

  json <- toJSON(item, auto_unbox=TRUE)
  response <- POST(url=url, body=json,
                   content_type('application/json'),
                   add_headers('Authorization' = getAuth(), 'Connection'='keep-alive'),
                   config = list('ssl.verifypeer' = FALSE))
  if (response$status_code != 200) {
    stop('failed POST on item. error code:', response$status_code)
  }
  ok <- content(response, as = "parsed")
  return(ok$id)
}
  