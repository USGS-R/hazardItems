getVisibleChildren	<-	function(json.url){

  # returns character array of children json.urls
  
  json.sep <- '/data/item/'
  sld.sep <- '/data/sld/'
  parent.id <-  tail(strsplit(json.url,'/')[[1]],1)
  json.rest <- paste(strsplit(json.url,json.sep)[[1]][1],json.sep,sep='')
  sld.rest <- paste(strsplit(json.url,json.sep)[[1]][1],sld.sep,sep='')
	
	child.table	<-	data.frame(bottom=c(FALSE),children=c(parent.id))
	which(!child.table$bottom)[1]
	while (any(!child.table$bottom)){
		peel.idx	<-	which(!child.table$bottom)[1]
		peel.back	<-	itemPeeler(json.rest, item.id=as.character(child.table$children[peel.idx]))
		child.table	<-	child.table[-peel.idx,]	# remove parent id
		child.table	<-	rbind(child.table,peel.back)	# replace with children
	}
  kids <- data.frame('json'=paste(json.rest,as.character(child.table$children),sep=''),
                     'sld'=paste(sld.rest,as.character(child.table$children),sep=''))
	return(kids)
}


itemPeeler	<-	function(item.id,json.rest){
	
	# takes ONE item id, moves down one level, returns children
	item.json	<-	suppressWarnings(fromJSON(file=paste(json.rest,item.id,sep='')))
	if(item.json$itemType == "data"){
		item.children	<-	item.json$id
		bottom	<-	TRUE
	} else {
		# how many children?
		item.children	<-	item.json$displayedChildren
		bottom	<-	vector(length=length(item.children))
	}			
	return(data.frame(bottom=bottom,children=item.children))
}

getUniqueBBoxIDs <- function(json.urls){
  # json.urls is an array of urls for children. 
  # returns indexes for unique bounding boxes
  bboxes = matrix(nrow=length(json.urls),ncol=4)
  for (j in 1:length(json.urls)){
    item.json  <-	suppressWarnings(fromJSON(file = json.urls[j]))
    bboxes[j,] <- item.json$bbox
  }
  un.bbox <- unique(bboxes)
  bbox.idx = vector(length=length(json.urls))
  # need better way for table match in R!!
  for (j in 1:length(json.urls)){
    for (k in 1:nrow(un.bbox)){
      if (all(bboxes[j,]==un.bbox[k,])){
        bbox.idx[j]<-k
      }
    }
    
  }

  return(bbox.idx)
}