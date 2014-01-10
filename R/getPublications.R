getPublications	<-	function(doc){
	# get all linked publications
	onlinks	<- list()
	onlinks$data <- getNodeSet(doc,'//citation/citeinfo/onlink')
	onlinks$publications	<-	getNodeSet(doc,'//lworkcit/citeinfo/onlink')
	onlinks$resources	<-	c(getNodeSet(doc,'//crossref/citeinfo/onlink'),getNodeSet(doc,'//srccite/citeinfo/onlink'))
	publications	<- list(data=list(),publications=list(),resources=list())
	names	<-	NULL
	publications$data	<-	getSubPub(onlinks$data )
	publications$publications	<-	getSubPub(onlinks$publications )
	publications$resources	<-	getSubPub(onlinks$resources)
	
	
	return(publications) 
}

