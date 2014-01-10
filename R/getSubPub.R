getSubPub	<-	function(onlink.cat){
	subPub	<-	list()
	if (length(onlink.cat)>0){
		for (i in 1:length(onlink.cat)){
			parentNode	<-	getNodeSet(onlink.cat[[i]],'parent::node()')
			name.node	<-	getNodeSet(parentNode[[1]],'title')
			if (length(name.node)!=0){
				names	<- c(names,xmlValue(name.node[[1]]))
				newPub	<-	list(title=xmlValue(getNodeSet(parentNode[[1]],'title')[[1]]),
					link=xmlValue(onlink.cat[[i]]))
				subPub[[i]]	<-	newPub
			}
		}
	} else {
		subPub	<-	NULL
	}
	
	return(subPub)
}