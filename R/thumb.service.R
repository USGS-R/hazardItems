library(png)
library(scales)
library(maps)
library(mapdata)
library(rjson) 

source('getSquareBBox.R') # work this into the package...

dim.x <- 150 # px
dim.y <- 150 # px
host <- "http://cida-test.er.usgs.gov/coastalchangehazardsportal"
wms.version <- "1.1.1"

# from item JSON:
item.id <- "C68abcd"
item.json	<-	fromJSON(file=paste(host,'/data/item/',item.id,sep=''))

item.bbox <- item.json$bbox # in the form minX,minY,maxX,maxY
bbox = getSquareBBox(item.bbox)
png(file = paste("thumb_",item.id,".png",sep=''), width = dim.x, height = dim.y, units = "px")
map("worldHires",xlim=c(bbox[1],bbox[3]), ylim=c(bbox[2],bbox[4]), col="floralwhite",
    lwd = 0.01,
    fill=TRUE,boundary = TRUE,mar=c(0,0,0,0),mai=c(0,0,0,0),oma=c(0,0,0,0),xpd = NA)
#map("worldHires","US", xlim=c(bbox[1],bbox[3]), ylim=c(bbox[2],bbox[4]), col="gray100",
 #   lwd = 0.01,
  #  fill=TRUE,boundary = TRUE,mar=c(0,0,0,0),mai=c(0,0,0,0),oma=c(0,0,0,0),xpd = NA)
lim <- par() # get limits from map image

if (item.json$itemType == "data"){
	# has no children
	item.wms <- item.json$wmsService$endpoint
	item.layer <- item.json$wmsService$layers
	get.layer <- paste(item.wms,"?version=",wms.version,"&request=GetMap","&layers=",item.layer,
	                   "&bbox=",paste(as.character(bbox),collapse=','),
	                   "&width=",as.character(dim.x),"&height=",as.character(dim.y),
	                   "&&format=image%2Fpng","&SLD=",host,'/data/sld/',item.id,sep='')

	download.file(get.layer,destfile="thumb_temp.png")
	ima <- readPNG("thumb_temp.png")
	rasterImage(ima, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
} else {
	# has children, is aggregation
	num.kids	<-	length(item.json$children)
	ima	<-	array(dim=c(150,150,3),data=1) # dims correct? Initialize blank array for png overlay w/ 1.0
	for (i in 1:num.kids){
		# open children elements (chould these children be aggregations?)
		child.json	<-	fromJSON(file=paste(host,'/data/item/',item.json$children[i],sep=''))
		child.wms <- child.json$wmsService$endpoint
		child.layer <- child.json$wmsService$layers
		get.layer <- paste(child.wms,"?version=",wms.version,"&request=GetMap","&layers=",child.layer,
		                   "&bbox=",paste(as.character(bbox),collapse=','),
		                   "&width=",as.character(dim.x),"&height=",as.character(dim.y),
		                   "&format=image%2Fpng","&SLD=",host,'/data/sld/',item.json$children[i],"?ribbon=",
                        as.character(i),sep='')
		download.file(get.layer,destfile="thumb_temp.png")
		temp.ima <- readPNG("thumb_temp.png")
		ima[temp.ima!=1] = temp.ima[temp.ima!=1] # valid? no need to loop
		# now...loop through the 
	}
	rasterImage(ima, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
}


map("worldHires",regions=c('US','Canada','Mexico','South America','Puerto Rico','Cuba'),
    xlim=c(bbox[1],bbox[3]), ylim=c(bbox[2],bbox[4]), col=c(alpha("gray10",0.25),alpha("gray10",0.25)),
    interior=FALSE,fill=TRUE,boundary = TRUE,add=TRUE,lwd = 0.1,
    mar=c(0,0,0,0),mai=c(0,0,0,0),oma=c(0,0,0,0),xpd = NA)

dev.off()