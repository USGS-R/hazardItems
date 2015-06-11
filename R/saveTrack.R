#'@title save NHC items
#'@description adds or updates the NHC items in the application
#'@param item list representing items to POST or PUT
#'@param ... extra params to pass to checkAuth
#'@return TRUE if it worked FALSE otherwise
#'@importFrom httr POST PUT content http_status
#'@importFrom jsonlite toJSON
#'@examples
#'\dontrun{
#'setBaseURL('dev')
#'  # create or replace NHC items
#'       
#'    
#'\dontrun{
#'setBaseURL('dev')
#'saveItem(items, authenticateUser("username"))
#'}
#'@export 
saveTrack <- function(items, ...){
  checkAuth(...)
  
  makeCone <- function()
    items <- getOption("hazardItems")$realtime.storms$track
    serviceTypeSource <- "source_wms"
    serviceTypeProxy <- "proxy_wms"
    
    
  
  makeTrack <- function()
    items <- getOption("hazardItems")$realtime.storms$track
    serviceTypeSource <- "source_wms"
    serviceTypeProxy <- "proxy_wms"
    
  makePoints <- function()
    items <- getOption("hazardItems")$realtime.storms$track
    serviceTypeSource <- "source_wms"
    serviceTypeProxy <- "proxy_wms"
    
  makeParent <- function()
    items <- getOption("hazardItems")$realtime.storms$track
    ribbonable <- "false"
    showChildren <- "false"
    enabled <- "true"
    activeStorm <- "false"
    serviceTypeSource <- "source_wms"
    serviceTypeProxy <- "proxy_wms"
  
  
}