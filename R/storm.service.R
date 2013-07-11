
storm.service = function(serviceEndpoint,attribute){
	subType	<-	tolower(attribute)

	subTypeDataSrc <- names(sourceSynonyms)

	doc <- xmlInternalTreeParse(serviceEndpoint)

	title <- xmlValue(getNodeSet(doc,'//citation/citeinfo/title')[[1]])
	abstract	<-	xmlValue(getNodeSet(doc,'//descript/abstract')[[1]])
	dataSrc <-  sapply(getNodeSet(doc,'//dataqual/lineage/srcinfo/srccite/citeinfo/title'),xmlValue)
	sourceContent <-  paste(dataSrc,collapse='|')
	overview <-  paste(c('This datasets includes an element of ', title),collapse='')
	
	is.generic	<-	FALSE
	if (!is.na(suppressWarnings(as.numeric(substr(subType,nchar(subType),nchar(subType)))))){
		is.generic	<-	TRUE
		stormNum	<-	substring(subType,5)
		subType	<-		substring(subType,1,4)
		processDetail <- paste(c('These probabilities apply to a generic representation of a category',
		                  stormNum,'hurricane'),collapse=' ')
		tinyDesc	<-	paste(c(' CAT',stormNum,' storm'),collapse='')
		medDesc	<-	paste(c('during a category',stormNum,'storm in'),collapse=' ')
		fullDesc	<-	paste(c('The Category ',stormNum,' '),collapse='')
		locationHld	<-	overview
		
	} else {
		processDetail <- NULL		
		fullDesc	<-	'The '
		tinyDesc	<-	paste(c(',',strsplit(title,' ')[[1]][1:2]),collapse=' ')
		medDesc	<-	paste(c('during',strsplit(title,' ')[[1]][1:2],'in'),collapse=' ')
		locationHld	<-	sapply(getNodeSet(doc,'/metadata/idinfo/descript/abstract'),xmlValue)
	}

	

	
	attrDefinition  <- attrDefinitions[subType]
	
	sourceString	<-	getSourceString(sourceContent)
	
	medium.summary	<-	paste(c(overview,attrDefinition,processDetail,sourceString),collapse='. ')
	
	# create medium title for storm item
	location	<-	getLocationString(locationHld) # default call is to medium service
	medium.title	<-	paste(c(titleMap[['medium']][[subType]],medDesc,location),collapse=' ')

	
	location	<-	getLocationString(locationHld,size='tiny')
	tiny.text	<-	paste(c(titleMap[['tiny']][[subType]],tinyDesc,' in ',location),collapse='')
	
	full.title	<-	paste(c(fullDesc,titleMap[['full']][[subType]],' element of ',title),collapse='')
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