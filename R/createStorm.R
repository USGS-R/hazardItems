#'@title Create modeled storm
#'@description Create items associated with model run of a storm
#'@param templateId id of the template to replace
#'@param filename a shapefile to post
#'@param ... extra params to pass to checkAuth
#'@details This will create all children associated with a storm
#' and aggregate them under the template.
#'@return character vector representing ID of new template
#'@examples
#'\dontrun{
#'createStorm("CAQw7M1", system.file("extdata", "Sandy_CIDA.zip", 
#'    package="hazardItems"))
#'}
#'@export
createStorm = function(templateId=NULL, filename, ...) {
  layerId = addLayer(filename, ...)
  newTemplate <- makeTemplateItem(layerId)
  newId <- saveItem(newTemplate)
  # TODO fix this part up to fully specify template
  items = list("children"=list("layerId"=layerId))
  success = template(templateId, items, ...)
  if (success) {
    uber <- getItem("uber")
    children <- uber$children
    if (!is.null(templateId)) {
      children <- replace(children, grep(templateId, children), newId)
    } else {
      children <- c(newId, children)
    }
    saveItem(uber)
    return(newId)
  } else {
    stop("unable to run template")
  }
  
}

makeTemplateItem = function(layerId) {
  layer = getLayer(layerId)
  csw = Filter(function(f){grepl("csw", f$type, ignore.case = TRUE)}, layer$services)[[1]]
  newTemplate <- list(
    "itemType"="template",
    "name"=paste0("storm_", format(Sys.time(), "%Y%m%d")),
    "type"="storms",
    "ribbonable"=TRUE,
    "showChildren"=TRUE,
    "activeStorm"=TRUE
  )
  
  summary <- fromJSON(realtime.service(csw$endpoint))
  newTemplate$summary <- summary
  return(newTemplate)
}