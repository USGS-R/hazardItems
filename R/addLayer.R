#'@title add a layer from a shapefile to CCH portal
#'@description returns url of the created layer
#'@param filename a shapefile to post
#'@param ... additional arguments passed to \code{\link{authenticateUser}}
#'@details This layer is used to track WFS, WMS, and CSW pointers
#'@return layer id for the new layer that was created
#'@importFrom httr POST content_type add_headers headers http_status
#'
#'@examples
#'\dontrun{
#'authenticateUser('bbadger')
#'addLayer(system.file("extdata", "Sandy_CIDA.zip", 
#'    package="hazardItems"))
#'}
#'@export
addLayer = function(filename, ...) {
  file <- file(filename, "rb")
  size <- file.info(filename)$size
  rawData <- readBin(file, "raw", n=size)
  close(file)
  
  checkAuth(...)
  
  layerUrl <- pkg.env$item_layer_post
  
  response <- POST(url=layerUrl, body=rawData, 
                   content_type('application/octet-stream'),
                   add_headers('Authorization' = getAuth(), 'Connection'='keep-alive'))
  
  if (http_status(response)$category == "Success") {
    location <- headers(response)$Location
    bits <- strsplit(location,"/")
    id <- tail(bits[[1]],n=1)
  } else {
    stop('Unable to post file, contact someone who can help')
  }
  return(id)
}

