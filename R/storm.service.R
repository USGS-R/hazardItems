
storm.service = function(serviceEndpoint,attribute){
	subType	<-	attribute

	subTypeDataSrc <- names(sourceSynonyms)

	doc <- xmlInternalTreeParse(serviceEndpoint)

	title <- xmlValue(getNodeSet(doc,'//citation/citeinfo/title')[[1]])
	abstract	<-	xmlValue(getNodeSet(doc,'//descript/abstract')[[1]])
	dataSrc <-  sapply(getNodeSet(doc,'//dataqual/lineage/srcinfo/srccite/citeinfo/title'),xmlValue)
	sourceContent <-  paste(dataSrc,collapse='|')
	
	baseType<- substring(subType,1,4)
	stormNum <- substring(subType,5)

	overview <-  paste(c('This datasets includes an element of ', title),collapse='')
	attrDefinition  <- attrDefinitions[baseType]
	processDetail <- paste(c('These probabilities apply to a generic representation of a category',
	                  stormNum,'hurricane'),collapse=' ')
	sourceString	<-	getSourceString(sourceContent)
	
	medium.summary	<-	paste(c(overview,attrDefinition,processDetail,sourceString),collapse='. ')
	
	# create medium title for storm item
	location	<-	getLocationString(overview) # default call is to medium service
	medium.title	<-	paste(c(titleMap[['medium']][[baseType]],'during a category',stormNum,'storm in',location),collapse=' ')
	
	location	<-	getLocationString(overview,size='tiny')
	tiny.text	<-	paste(c(titleMap[['tiny']][[baseType]],', CAT',stormNum,' storm in ',location),collapse='')
	
	full.title	<-	paste(c('The Category ',stormNum,' ',titleMap[['full']][[baseType]],' element of ',title),collapse='')
	full.text	<-	sub('\n','',paste(c(abstract),collapse=''))
	
	linkedPubs	<-	sapply(getNodeSet(doc,'//srcinfo/srccite/citeinfo/onlink'),xmlValue)
	linkedTitles	<-	sapply(getNodeSet(doc,'//srcinfo/srccite/citeinfo/onlink/preceding-sibling::node()[1]'),xmlValue)	
	full.publications	<- list()
	for (i in 1:length(linkedPubs)){
		full.publications[linkedTitles[i]]	<- linkedPubs[i]
	}
	names(full.publications)	<- linkedTitles
	
	summaryJSON	<- toJSON(list('summary'=list(
		'tiny'=list('text'=tiny.text),
		'medium'=list('title'=medium.title,'text'=medium.summary),
		'full'=list('title'=full.title,'text'=full.text,'publications'=full.publications))), method="C" )
	return(summaryJSON)
}