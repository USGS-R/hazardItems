#'@title instantiate a template with new items based on new data
#'@description creates several items based on data passed to template
#'@param templateId id of the template to instantiate
#'@param items list of items to create (attr or item id and layerId)
#'@param ... extra params to pass to checkAuth
#'@details This can be used to either create new items from a template
#' or replace existing items under a template
#'@return TRUE if it worked FALSE otherwise
#'@importFrom httr POST http_status
#'@importFrom jsonlite toJSON
#'@examples
#'  # replace existing children items
#'  items <- list(children=list(
#'    list(
#'      id="DvwQhQEH",
#'      layerId="DvwcwCrK"
#'    ),
#'    list(
#'      id="DvwQjFYJ",
#'      layerId="DvwcwCrK"
#'    )
#'  ))
#'  # create new children from template
#'  items <- list(children=list(
#'    list(
#'      attr="MEAN",
#'      layerId="DvwcwCrK"
#'    ),
#'    list(
#'      attr="RUNUP",
#'      layerId="DvwcwCrK"
#'    )
#'  ))
#'\dontrun{
#'#templateID is manually created on the publish page
#'  template("CAQw7M1", items, authenticateUser(username))
#'}
#'@export
template = function(templateId, items, ...) {
  checkAuth(...)
  url <- paste0(pkg.env$item_template, templateId)
  json <- toJSON(items, auto_unbox=TRUE)
  response <- POST(url=url, body=json,
                  content_type('application/json'),
                  add_headers('Authorization' = getAuth(), 'Connection'='keep-alive'))
  return(http_status(response)$category == "success")
}