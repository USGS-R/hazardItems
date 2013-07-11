
getKeywords = function(doc,subType){
	
	
	placekeys	<-  sapply(getNodeSet(doc,'//placekey'),xmlValue)
	themekeys	<-  sapply(getNodeSet(doc,'//themekey'),xmlValue)
	attrkeys	<-	attrKeywords[[subType]]
		
	
	keywords	<-	c(placekeys,attrkeys,themekeys)
	keywords	<-	paste(unique(keywords),collapse='|')
	return(keywords)
}