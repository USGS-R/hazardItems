#'@title instantiate a template with new items based on new data
#'@description creates several items based on data passed to template
#'@param templateId id of the template to instantiate
#'@param items list of items to create (attr or item id and layerId)
#'@param token auth token to pass in for templating
#'@details This can be used to either create new items from a template
#' or replace existing items under a template
#'@return TRUE if it worked FALSE otherwise
#'@importFrom httr POST http_status
#'@importFrom jsonlite toJSON
#'@export
template = function(templateId, items, token) {
  url <- paste0(pkg.env$item_template, templateId)
  json <- toJSON(items, auto_unbox=TRUE)
  respose <- POST(url=url, body=json,
                  content_type('application/json'),
                  add_headers('Authorization' = auth, 'Connection'='keep-alive'))
  return(http_status(response)$category == "success")
}