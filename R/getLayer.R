#'@title get layer info
#'@description returns a list of layer contents
#'@param layerId a string for a valid layer identifier
#'@return a list representing layer information
#'@importFrom httr GET accept_json content
#'@examples
#'\dontrun{
#'setBaseURL('dev')
#'getLayer('DAozfKaK')
#'}
#'@export 
getLayer <- function(layerId){
  url = paste0(pkg.env$item_layer, layerId)
  response <- GET(url = url, accept_json())
  
  if (response$status_code != 200){
    stop('failed GET on item ', layerId ,'. error code:', response$status_code)
  }
  layer <- content(response, as = 'parsed')

  return(layer)
}
  