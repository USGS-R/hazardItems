layerPath <- function() {
  return("https://cida-test.er.usgs.gov/dev/coastalchangehazardsportal/data/layer")
}

addLayer = function(filename) {
  file <- file(filename, "rb")
  size <- file.info(filename)$size
  rawData <- readBin(file, "raw", n=size)
  result <- POST(url=layerPath(), body=rawData, content_type("application/octet-stream"))
  # pull out Location header from response
  # return ID of created layer
}