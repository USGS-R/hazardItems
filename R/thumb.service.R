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
item.id <- "C67pzz9"
item.bbox <- c(-75.943502,37.094015,-75.051999,38.634369) # in the form minX,minY,maxX,maxY
item.wms <- "http://cida-test.er.usgs.gov/coastalchangehazardsportal/geoserver/proxied/wms"
item.layer <- "proxied:Sandy_Sandy_potential_erosion_hazards_Hurricane_Sandy_Potential_Erosion_Hazards"


bbox = getSquareBBox(item.bbox)
get.layer <- paste(item.wms,"?version=",wms.version,"&request=GetMap","&layers=",item.layer,
                   "&bbox=",paste(as.character(bbox),collapse=','),
                   "&width=",as.character(dim.x),"&height=",as.character(dim.y),
                   "&&format=image%2Fpng","&SLD=",host,'/data/sld/',item.id,sep='')

download.file(get.layer,destfile="thumb_temp.png")



png(file = paste("thumb_",item.id,".png",sep=''), width = dim.x, height = dim.y, units = "px")


map("worldHires","US", xlim=c(bbox[1],bbox[3]), ylim=c(bbox[2],bbox[4]), col="gray100",
    lwd = 0.01,
    fill=TRUE,boundary = TRUE,mar=c(0,0,0,0),mai=c(0,0,0,0),oma=c(0,0,0,0),xpd = NA)

ima <- readPNG("thumb_temp.png")

#Get the plot information so the image will fill the plot box, and draw it
lim <- par() # get limits from map image

rasterImage(ima, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
map("worldHires","US", xlim=c(bbox[1],bbox[3]), ylim=c(bbox[2],bbox[4]), col=c(alpha("gray10",0.25),alpha("gray10",0.25)),
    interior=FALSE,fill=TRUE,boundary = TRUE,add=TRUE,lwd = 0.2,
    mar=c(0,0,0,0),mai=c(0,0,0,0),oma=c(0,0,0,0),xpd = NA)
dev.off()