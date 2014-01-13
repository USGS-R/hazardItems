thumb.service <- function(item.id){
	
	dim.x <- 150 # px
	dim.y <- 150 # px
	ima	<-	array(dim=c(dim.y,dim.x,3),data=1) # dims correct? Initialize blank array for png overlay w/ 1.0
	item.host	<-	"http://cida-wiwsc-cchdev.er.usgs.gov:8080/coastal-hazards-portal"
	wms.version <- "1.3.0"

	bbox = getSquareBBox(item.id,item.host=item.host)
	png(file = paste("thumb_",item.id,".png",sep=''), width = dim.x, height = dim.y, units = "px")
	map("worldHires",xlim=c(bbox[1],bbox[3]), ylim=c(bbox[2],bbox[4]), col="floralwhite",
    	lwd = 0.01,fill=TRUE,boundary = TRUE,
		mar=c(0,0,0,0),mai=c(0,0,0,0),oma=c(0,0,0,0),xpd = NA)

	lim <- par() # get limits from map image
	kids	<-	getVisibleChildren(parent.id=item.id,item.host=item.host)
	num.kids	<-	length(kids)
	for (i in 1:num.kids){
		# open children elements (chould these children be aggregations?)
		child.json	<-	fromJSON(file=paste(item.host,'/data/item/',kids[i],sep=''))
		child.services <- child.json$services
			for (k in 1:length(child.services)){
				if (child.services[[k]]$type=="proxy_wms"){
					child.wms	<-	child.services[[k]]$endpoint
					child.layer	<-	child.services[[k]]$serviceParameter
					break
				}
			}
		get.layer <- paste(child.wms,"?version=",wms.version,"&service=wms","&request=GetMap","&layers=",child.layer,
		                   "&bbox=",paste(as.character(bbox),collapse=','),
		                   "&width=",as.character(dim.x),"&height=",as.character(dim.y),
		                   "&format=image%2Fpng","&SLD=",item.host,'/data/sld/',kids[i],"?ribbon=",
							as.character(i),sep='')
							
		download.file(get.layer,destfile="thumb_temp.png")
		temp.ima <- readPNG("thumb_temp.png")
		temp.ima <- readPNG(get.layer)
		ima[temp.ima!=1] = temp.ima[temp.ima!=1] # valid? no need to loop
	 }
	rasterImage(ima, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])

	map("worldHires",
		xlim=c(bbox[1],bbox[3]), ylim=c(bbox[2],bbox[4]), col=c(alpha("gray10",0.25),alpha("gray10",0.25)),
		interior=FALSE,fill=TRUE,boundary = TRUE,add=TRUE,lwd = 0.1,
		mar=c(0,0,0,0),mai=c(0,0,0,0),oma=c(0,0,0,0),xpd = NA)

  dev.off()
}