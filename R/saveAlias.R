#'@title save alias
#'@description adds or updates an alias in the application
#'@param alias list representing alias to POST or PUT
#'@param isNew whether or not the alias is new
#'@param ... extra params to pass to checkAuth
#'@return saved alias ID
#'@importFrom httr POST PUT content
#'@importFrom jsonlite toJSON
#'@examples
#'\dontrun{
#'setBaseURL('dev')
#'saveAlias(
#'  alias=list("id"="test", "item_id"="test-item"),
#'  isNew=TURE
#')
#'}
#'@export 
saveAlias <- function(alias, isNew, ...){
  checkAuth(...)
  url = pkg.env$alias_json

  json <- toJSON(alias, auto_unbox=TRUE)
  if (isNew) {
    response <- POST(url=url, body=json,
                   content_type('application/json'),
                   add_headers('Authorization' = getAuth(), 'Connection'='keep-alive'))
    ok <- content(response, as = "parsed")
  } else {
    url = paste0(url, alias$id)
    response <- PUT(url=url, body=json, content_type('application/json'),
                    add_headers('Authorization' = getAuth(), 'Connection'='keep-alive'))
    ok <- list(id=alias$id)
  }
  if (response$status_code != 200) {
    stop('failed POST or PUT on alias error code:', response$status_code)
  }
 
  return(ok$id)
}
  