#'@title create thumbnail for an item
#'@description takes json url and creates summary image for item
#'@param json.url a valid JSON url
#'@return A strong location for the created png image
#'@importFrom jsonlite fromJSON
#'@import maps mapdata scales png
#'@importFrom httr GET write_disk
#'@examples
#'serviceEndpoint <- 'http://marine.usgs.gov/coastalchangehazardsportal/data/item/CAkR645'
#'thumb.service(serviceEndpoint)
#'@export
thumb.service <- function(json.url){
  
	dim.x <- 150 # px
	dim.y <- 150 # px
  if (dim.y!=dim.x){stop("non-square image not currently supported")}
  
	ima	<-	array(dim=c(dim.y,dim.x,3),data=1) 
	wms.version <- "1.3.0"
  response <- GET(json.url, accept_json())
  item.json <- content(response, as = 'parsed')
  
  item.id <- item.json$id
  
	bbox = getSquareBBox(item.json)
	png(filename = paste("thumb_",item.id,".png",sep=''), width = dim.x, height = dim.y, units = "px")
	map("worldHires",xlim=c(bbox[1],bbox[3]), ylim=c(bbox[2],bbox[4]), col="floralwhite",
    	lwd = 0.01,fill=TRUE,boundary = TRUE,
		mar=c(0,0,0,0),mai=c(0,0,0,0),oma=c(0,0,0,0),xpd = NA)

	lim <- par() # get limits from map image
	kids	<-	getVisibleChildren(json.url)
	num.kids	<-	length(kids$json)
  
  parent.char.bbox <- paste(as.character(bbox),collapse=',')
  parent.char.x <- as.character(dim.x)
  parent.char.y <- as.character(dim.y)
  
  # get unique bounding boxes and indexes for the kids
	bbox.idx <- getUniqueBBoxIDs(as.character(kids$json))

	r.c <- vector(length=length(unique(bbox.idx))) # ribbon count              
	for (i in 1:num.kids){
    child.json.url <- as.character(kids$json[i])
    child.sld.url <- as.character(kids$sld[i])
    response <- GET(child.json.url, accept_json())
    child.json <- content(response, as = 'parsed')
		child.services <- child.json$services
			for (k in 1:length(child.services)){
				if (child.services[[k]]$type=="proxy_wms"){
					child.wms	<-	child.services[[k]]$endpoint
					child.layer	<-	child.services[[k]]$serviceParameter
					break
				}
			}
    
    
    if (!item.json$ribbonable){
      ribbon = "1"
    } else if (item.json$ribbonable & !child.json$ribbonable){
      ribbon = "1"
    } else if (item.json$ribbonable & child.json$ribbonable){
      # this child will be ribboned...
      
      r.c[bbox.idx[i]] <- r.c[bbox.idx[i]]+1 # only incremented per number of ribboned kids
      ribbon = as.character(r.c[bbox.idx[i]])
    }
		get.layer <- paste(child.wms,"?version=",wms.version,"&service=wms","&request=GetMap","&layers=",child.layer,
		                   "&bbox=",parent.char.bbox,
		                   "&STYLES=cch",
                       "&TRANSPARENT=FALSE",
		                   "&width=",parent.char.x,"&height=",parent.char.y,
		                   "&format=image%2Fpng","&SLD=",child.sld.url,"?ribbon=",ribbon,
							         sep='')
							
		GET(get.layer,write_disk("thumb_temp.png", overwrite = TRUE))
		temp.ima <- readPNG("thumb_temp.png")
		ima[temp.ima!=1] = temp.ima[temp.ima!=1] # valid? no need to loop
	 }
	rasterImage(ima, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])

	map("worldHires",
		xlim=c(bbox[1],bbox[3]), ylim=c(bbox[2],bbox[4]), col=c(alpha("gray10",0.25),alpha("gray10",0.25)),
		interior=FALSE,fill=TRUE,boundary = TRUE,add=TRUE,lwd = 0.1,
		mar=c(0,0,0,0),mai=c(0,0,0,0),oma=c(0,0,0,0),xpd = NA)

  dev.off()
	unlink(x="thumb_temp.png") # remove temporary png
  return(paste("thumb_",item.id,".png",sep=''))
}
