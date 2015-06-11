#'@title save NHC items
#'@description adds or updates the NHC items in the application
#'@param item list representing items to POST or PUT
#'@param ... extra params to pass to checkAuth
#'@return TRUE if it worked FALSE otherwise
#'@examples
#'\dontrun{
#'setBaseURL('dev')
#'saveTrack()
#'}
#'
#'@export
saveTrack <- function(...){
  checkAuth(...)
  
  trackConfig <- getOption("hazardItems")$realtime.storms$track
  
  children <- sapply(trackConfig$children, makeChild, trackConfig$serviceEndpoint)
  item <- list(
    id = trackConfig$id,
    itemType = "aggregation",
    name = "track",
    type = "storms",
    ribbonable = FALSE,
    showChildren = FALSE,
    bbox = bbox,
    summary = summary,
    children = children,
    displayedChildren = children
  )
  trackId <- saveItem(item)
  return(trackId == trackConfig$id)
}

makeChild = function(serviceParam, serviceEndpoint, bbox, summary) {
  item <- list(itemType="data", name="track", type="storms", ribbonable=FALSE)
  item$attr <- serviceParam
  item$bbox <- bbox
  item$summary <- summary
  services = list()
  append(services, list(type="source_wms", endpoint=serviceEndpoint, serviceParameter=serviceParam))
  append(services, list(type="proxy_wms", endpoint=serviceEndpoint, serviceParameter=serviceParam))
  item$services <- services
  id <- saveItem(item)
  return(id)
}