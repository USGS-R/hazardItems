
storm.service = function(serviceEndpoint,attribute){
	subType	<-	tolower(attribute)

	subTypeDataSrc <- names(sourceSynonyms)

	doc <- xmlInternalTreeParse(serviceEndpoint)

	title <- xmlValue(getNodeSet(doc,'//citation/citeinfo/title')[[1]])
	abstract	<-	xmlValue(getNodeSet(doc,'//descript/abstract')[[1]])
	dataSrc <-  sapply(getNodeSet(doc,'//dataqual/lineage/srcinfo/srccite/citeinfo/title'),xmlValue)
	sourceContent <-  paste(dataSrc,collapse='|')
	
	is.generic	<-	FALSE
	if (!is.na(as.numeric(substr(subType,nchar(subType),nchar(subType))))){
		is.generic	<-	TRUE
		stormNum	<- substring(subType,5)
		subType	<-		substring(subType,1,4)
		processDetail <- paste(c('These probabilities apply to a generic representation of a category',
		                  stormNum,'hurricane'),collapse=' ')
	}

	

	overview <-  paste(c('This datasets includes an element of ', title),collapse='')
	attrDefinition  <- attrDefinitions[subType]
	
	sourceString	<-	getSourceString(sourceContent)
	
	medium.summary	<-	paste(c(overview,attrDefinition,processDetail,sourceString),collapse='. ')
	
	# create medium title for storm item
	location	<-	getLocationString(overview) # default call is to medium service
	medium.title	<-	paste(c(titleMap[['medium']][[subType]],'during a category',stormNum,'storm in',location),collapse=' ')
	
	location	<-	getLocationString(overview,size='tiny')
	tiny.text	<-	paste(c(titleMap[['tiny']][[subType]],', CAT',stormNum,' storm in ',location),collapse='')
	
	full.title	<-	paste(c('The Category ',stormNum,' ',titleMap[['full']][[subType]],' element of ',title),collapse='')
	full.text	<-	sub('\n','',paste(c(abstract),collapse=''))
	
	onlinks	<-	getNodeSet(doc,'//citeinfo/onlink')
	full.publications	<- list()
	names	<-	NULL
	for (i in 1:length(onlinks)){
		parentNode	<-	getNodeSet(onlinks[[i]],'parent::node()')
		names	<- c(names,xmlValue(getNodeSet(parentNode[[1]],'title')[[1]]))
		newPub	<-	list(title=xmlValue(getNodeSet(parentNode[[1]],'title')[[1]]),
			link=xmlValue(onlinks[[i]]))
		full.publications[[i]]	<-	newPub
	}
	
	summaryJSON	<- toJSON(list(
		'tiny'=list('text'=tiny.text),
		'medium'=list('title'=medium.title,'text'=medium.summary),
		'full'=list('title'=full.title,'text'=full.text,'publications'=full.publications)), method="C" )
	return(summaryJSON)
}