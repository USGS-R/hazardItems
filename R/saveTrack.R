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

  children <- sapply(trackConfig$children, makeChild,
                     trackConfig$serviceEndpoint,
                     trackConfig$bbox,
                     trackConfig$summary)
  item <- list(
    id = trackConfig$id,
    itemType = "aggregation",
    name = "track",
    type = "storms",
    ribbonable = FALSE,
    showChildren = FALSE,
    bbox = trackConfig$bbox,
    summary = trackConfig$summary,
    children = children,
    displayedChildren = children
  )
  trackId <- saveItem(item)
  return(trackId == trackConfig$id)
}

makeChild = function(serviceParams, serviceEndpoint, bbox, summary) {
  param <- serviceParams$serviceParameter
  attr <- serviceParams$attr
  item <- list(itemType="data", name="track", type="storms", ribbonable=FALSE)
  item$attr <- attr
  item$bbox <- bbox
  item$summary <- summary
  services = list()
  services[[1]] <- list(type="source_wms", endpoint=serviceEndpoint, serviceParameter=param)
  services[[2]] <- list(type="proxy_wms", endpoint=serviceEndpoint, serviceParameter=param)
  item$services <- services
  id <- saveItem(item)
  return(id)
}