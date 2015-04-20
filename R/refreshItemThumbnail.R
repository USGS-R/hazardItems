#'@title Refreshes an item's thumbnail PNG 
#'@description returns a boolean success 
#'@param itemID id of item to refresh thumbnail for
#'@param ... extra parameters to pass to checkAuth
#'@return returns TRUE if the thumbnail is put successfully, and false if it fails
#'@importFrom base64enc base64encode
#'@importFrom httr PUT
#'
#'@examples 
#'\dontrun{
#'setBaseURL('dev')
#'refreshItemThumbnail('CCGftiy')
#'}
#'@export
refreshItemThumbnail <- function(itemID, ...) {
  checkAuth(...)
  
  itemThumbnailEndpoint <- paste0(pkg.env$item_thumbnail, itemID)
  
  pngFile <- thumb.service(paste0(pkg.env$item_json, itemID))
  
  enc <- base64encode(pngFile)
  
  # make a call to thumbnail service (item.thumbnail) with the itemID (actually full path) 
  # then encode the result in base64
  # then assign body to encoding
  # then put 
  # 
  
  response <- PUT(url=itemThumbnailEndpoint, body=enc, 
                   content_type('text/plain'),
                   add_headers('Authorization' = getAuth(), 'Connection'='keep-alive'))
  
  return(http_status(response)$category == "success")
  
}