#'@title add a layer from a shapefile to CCH portal
#'@description returns url of the created layer
#'@param filename a shapefile to post
#'@param token token received from the authenticateUser function
#'@details This layer is used to track WFS, WMS, and CSW pointers
#'@return layer url for the new layer
#'@importFrom httr POST content_type add_headers headers http_status
#'@export
addLayer = function(filename, token) {
  file <- file(filename, "rb")
  size <- file.info(filename)$size
  rawData <- readBin(file, "raw", n=size)
  
  if (!missing(token)) {
    auth <- paste('Bearer', token)
  }
  layerUrl <- pkg.env$item_layer
  
  response <- POST(url=layerUrl, body=rawData, 
                   content_type('application/octet-stream'),
                   add_headers('Authorization' = auth, 'Connection'='keep-alive'))
  if (http_status(response)$category == "success") {
    location <- headers(response)$Location
  } else {
    error('Unable to post file, contact someone who can help')
  }
  return(location)
}