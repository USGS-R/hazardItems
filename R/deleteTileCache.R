#'@title delete tile cache for application
#'@description destroys existing tile cache
#'@param ... extra params to pass to checkAuth
#'@return TRUE if delete worked
#'@importFrom httr POST add_headers http_status content
#'
#'@examples
#'\dontrun{
#'  setBaseURL('dev')
#'  deleteTileCache(username='bbadger')
#'}
#'@export
deleteTileCache = function(...) {
  checkAuth(...)
  tileCacheUrl <- pkg.env$tile_cache
  
  response <- DELETE(url=tileCacheUrl,
                     add_headers('Authorization' = getAuth(),
                                 'Connection'='keep-alive'), verbose())
  return(tolower(http_status(response)$category) == "success")
}