
storm.service = function(serviceEndpoint,attribute){
	subType	<-	attribute

	subTypeDataSrc <- names(sourceSynonyms)

	doc <- xmlInternalTreeParse(serviceEndpoint)

	title <- xmlValue(getNodeSet(doc,'//citation/citeinfo/title')[[1]])

	dataSrc <-  sapply(getNodeSet(doc,'//dataqual/lineage/srcinfo/srccite/citeinfo/title'),xmlValue)
	sourceContent <-  paste(dataSrc,collapse='|')
	
	baseType<- substring(subType,1,4)
	stormNum <- substring(subType,5)

	overview <-  paste(c('This datasets includes an element of ', title),collapse='')
	attrDefinition  <- attrDefinitions[baseType]
	processDetail <- paste(c('These probabilities apply to a generic representation of a category',
	                  stormNum,'hurricane'),collapse=' ')
	sourceString	<-	getSourceString(sourceContent)
	
	medium.summary <- paste(c(overview,attrDefinition,processDetail,sourceString),collapse='. ')
	summaryJSON	<- toJSON(list('summary'=list(
		'tiny'=list('text'='xxx'),
		'medium'=list('title'='XXX','text'=medium.summary),
		'full'=list('title'='XXX','text'='XXX','publications'='XXX'))), method="C" )
	return(summaryJSON)
}