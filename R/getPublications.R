getPublications = function(doc){
	onlinks	<-	getNodeSet(doc,'//citeinfo/onlink')
	publications	<- list()
	names	<-	NULL
	for (i in 1:length(onlinks)){
		parentNode	<-	getNodeSet(onlinks[[i]],'parent::node()')
		name.node	<-	getNodeSet(parentNode[[1]],'title')
		if (length(name.node)!=0){
			names	<- c(names,xmlValue(name.node[[1]]))
			newPub	<-	list(title=xmlValue(getNodeSet(parentNode[[1]],'title')[[1]]),
				link=xmlValue(onlinks[[i]]))
			publications[[i]]	<-	newPub
		}
	}
	return(publications) 
}