#'@title Update storm
#'@description Adds all attributes to an existing template
#'@param templateId id of the template to instantiate
#'@param filename a shapefile to post
#'@param ... extra params to pass to checkAuth
#'@details This will create all children associated with a storm
#' and aggregate them under the template.
#'@return TRUE if it worked FALSE otherwise
#'@examples
#'\dontrun{
#'updateStorm("CAQw7M1", system.file("extdata", "Sandy_CIDA.zip", 
#'    package="hazardItems"))
#'}
#'@export
updateStorm = function(templateId, filename, ...) {
  layerId = addLayer(filename, ...)
  template = getItem(templateId, subtree=TRUE)
  items = buildList(template, layerId)
  success = template(templateId, items, ...)
  return(success)
}

buildList = function(template, layerId) {
  result = list(children=list())
  existing = template$children
  for (i in 1:length(existing)) {
    child = existing[[i]]
    # TRACK is a magic value representing the NHC track aggregation
    attr = ifelse(child$id == getOption("hazardItems")$realtime.storms$track$id, "TRACK", child$attr)
    visible = attr %in% getOption("hazardItems")$realtime.storms$visible
    if (is.null(attr) | attr == "TRACK") {
      result$children[[i]] <- list(id=child$id, visible=visible)
    } else {
      result$children[[i]] <- list(id=child$id, layerId=layerId, visible=visible)
    }
  }
  return(result)
}