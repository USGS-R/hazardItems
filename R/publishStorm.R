#'@title Publish storm
#'@description Adds all attributes to an existing template
#'@param templateId id of the template to instantiate
#'@param filename a shapefile to post
#'@param ... extra params to pass to checkAuth
#'@details This will create all children associated with a storm
#' and aggregate them under the template.
#'@return TRUE if it worked FALSE otherwise
#'@examples
#'\dontrun{
#'publishStorm("CAQw7M1", system.file("extdata", "Sandy_CIDA.zip", 
#'    package="hazardItems"))
#'}
#'@export
publishStorm = function(templateId, filename, ...) {
  layerId = addLayer(filename, ...)
  items = list("allAttributes"=TRUE, "layerId"=layerId)
  success = template(templateId, items, ...)
  return(success)
}