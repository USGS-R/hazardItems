getVisibleChildren	<-	function(parent.id,item.host="http://cida-wiwsc-cchdev.er.usgs.gov:8080/coastal-hazards-portal"){
	
	# returns character array of children
	child.table	<-	data.frame(bottom=c(FALSE),children=c(parent.id))
	which(!child.table$bottom)[1]
	while (any(!child.table$bottom)){
		peel.idx	<-	which(!child.table$bottom)[1]
		peel.back	<-	itemPeeler(item.id=child.table$children[peel.idx],item.host=item.host)
		child.table	<-	child.table[-peel.idx,]	# remove parent id
		child.table	<-	rbind(child.table,peel.back)	# replace with children
	}

	
	return(as.character(child.table$children))
}


itemPeeler	<-	function(item.host,	item.id){
	
	# takes ONE item id, moves down one level, returns children
	library(rjson) 
	item.json	<-	fromJSON(file=paste(item.host,'/data/item/',item.id,sep=''))
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