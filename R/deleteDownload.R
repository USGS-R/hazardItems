#'@title delete the cache for an item download
#'@description deletes cached download, can then be re-seeded
#'@param itemId id of download to blow away
#'@param ... extra params to pass to checkAuth
#'@return TRUE if download is deleted
#'@importFrom httr DELETE add_headers http_status
#'
#'@examples
#'\dontrun{
#'  authenticateUser("bbadger")
#'  deleteDownload("CCNKrhr")
#'}
#'@export
deleteDownload = function(itemId, ...) {
  checkAuth(...)
  
  downloadUrl <- paste0(pkg.env$item_download, itemId)
  response <- DELETE(url=downloadUrl,
                   add_headers('Authorization' = getAuth(),
                               'Connection'='keep-alive'))
  return(http_status(response)$category == "success")
}