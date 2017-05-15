#'@title download item
#'@description downloads item from cache, or builds cache and gets download
#'@param itemId id to download 
#'@param ... extra params to pass to checkAuth
#'@return TRUE if download worked
#'@importFrom httr GET add_headers http_status content
#'
#'@examples
#'\dontrun{
#'  downloadItem("CCNKrhr")
#'}
#'@export
downloadItem = function(itemId, ...) {
  downloadUrl <- paste0(pkg.env$item_download, itemId)
  response <- GET(url=downloadUrl)
  return(tolower(http_status(response)$category) == "success")
}