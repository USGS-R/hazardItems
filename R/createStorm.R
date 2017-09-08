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
#'createStorm("CAQw7M1", system.file("extdata", "Sandy_CIDA.zip", "hurricane-sandy",
#'    package="hazardItems"))
#'}
#'@export
createStorm = function(templateId=NULL, filename, aliasName=NULL, ...) {
  #Clean and validate alias, if provided
  if(!is.null(aliasName)){
    aliasName <- cleanAndValidateAlias(aliasName)
    
    if(is.null(aliasName)){
      stop("Invalid alias name. Valid characters: Lowercase Letters, Numbers, and `-`.")
    }
  }
  
  layerId = addLayer(filename, ...)
  newTemplate <- makeTemplateItem(layerId)
  newId <- saveItem(newTemplate)
  items = makeItemLayout(layerId)
  success = template(newId, items, ...)
  if (success) {
    #Save alias, if applicable
    if(!is.null(aliasName)){
      aliasObject <- list(
        "id" = aliasName,
        "item_id" = newId
      )
      saveAlias(aliasObject, !aliasExists(aliasName))
    }
   
    #Update uber
    uber <- getItem("uber")
    children <- uber$children
    if (!is.null(templateId)) {
      children <- replace(children, grep(templateId, children), newId)
    } else {
      children <- c(newId, children)
    }
    uber$children <- children
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

makeItemLayout = function(layerId) {
  items <- list("children"=list())
  layout <- getOption("hazardItems")$realtime.storms$layout
  trackId <- getOption("hazardItems")$realtime.storms$track$id
  for (i in 1:length(layout)) {
    attr = layout[[i]]
    # TRACK is a magic value representing the NHC track aggregation
    visible = attr %in% getOption("hazardItems")$realtime.storms$visible
    if (attr == "TRACK") {
      items$children[[i]] <- list(id=trackId, visible=visible)
    } else {
      items$children[[i]] <- list(attr=attr, layerId=layerId, visible=visible)
    }
  }
  return(items)
}