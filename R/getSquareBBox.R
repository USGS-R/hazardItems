getSquareBBox <- function(item.json){
  # item json is in R data.frame
  	# returns a square BBox based on item id


	BBox <- item.json$bbox # in the form minX,minY,maxX,maxY
  	max.dim <- getMaxDim(BBox)
  
  if (max.dim$max.dim=='long'){
    # need to elongate the lat dim
    BBox.cent = mean(c(BBox[2],BBox[4]))
    half.width <- BBox[4]-BBox.cent
    BBox[2] <- BBox.cent-half.width*max.dim$ratio
    BBox[4] <- BBox.cent+half.width*max.dim$ratio
  } else {
    # need to elongate the long dim
    BBox.cent = mean(c(BBox[1],BBox[3]))
    half.width <- BBox[3]-BBox.cent
    BBox[1] <- BBox.cent-half.width*max.dim$ratio
    BBox[3] <- BBox.cent+half.width*max.dim$ratio
  }
    return(BBox)
}

getMaxDim <- function(BBox){
  dist.lat <- geodetic.distance(c(BBox[1],BBox[2]),c(BBox[1],BBox[4]))
  dist.long <- geodetic.distance(c(BBox[1],BBox[2]),c(BBox[3],BBox[2]))
  
  max.dim = 'lat'
  bbox.ratio <- dist.lat/dist.long
  if (dist.long>dist.lat){
    max.dim = 'long'
    bbox.ratio <- dist.long/dist.lat
  }
  
  return(list(max.dim=max.dim,ratio=bbox.ratio))
}

geodetic.distance <- function(point1, point2) 
{ 
  R <- 6371 # radius 
  p1rad <- point1 * pi/180 
  p2rad <- point2 * pi/180 
  d <- sin(p1rad[2])*sin(p2rad[2])+cos(p1rad[2])*cos(p2rad[2])*cos(abs(p1rad[1]-p2rad[1]))  
  d <- acos(d) 
  R*d 
} 

