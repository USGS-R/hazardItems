#'@title create summary json for an individual storm item
#'@description This service parses the metadata record and creates a summary specific to the STORM theme, 
#'with subtype being defined by the attribute name
#'@param serviceEndpoint valid xml, either local or a url
#'@param attribute an attribute to use form the shapefile corresponding to 
#'\code{serviceEndpoint}
#'@return Serialized JSON for summary
#'@importFrom jsonlite toJSON
#'@import XML
#'@examples
#'serviceEndpoint  <-	'http://olga.er.usgs.gov/data/NACCH/GOM_erosion_hazards_metadata.xml'
#'attribute	<-	'PCOL3'
#'summary	<-	storm.service(serviceEndpoint,attribute)
#'print(summary)
#'@export
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
		stormNum	<-	substr(subType,nchar(subType),nchar(subType))
		subType	<-		substring(subType,1,(nchar(subType)-1))
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
		locationHld	<-	sapply(getNodeSet(doc,'//metadata/idinfo/descript/abstract'),xmlValue)
	}

	

	
	attrDefinition  <- attrDefinitions[subType]
	
	sourceString	<-	getSourceString(sourceContent)
	
	medium.summary	<-	paste(c(overview,attrDefinition,processDetail,sourceString),collapse='. ')
	
	# create medium title for storm item
	location	<-	getLocationString(locationHld,singleVal=TRUE) # default call is to medium service
	medium.title	<-	paste(c(titleMap[['medium']][[subType]],medDesc,location),collapse=' ')

	
	location	<-	getLocationString(locationHld,size='tiny',singleVal=TRUE)
	tiny.text	<-	paste(c(titleMap[['tiny']][[subType]],tinyDesc,' in ',location),collapse='')
	
	full.title	<-	paste(c(fullDesc,titleMap[['full']][[subType]],' element of ',title),collapse='')
	full.text	<-	sub('\n','',paste(c(abstract),collapse=''))
	
	full.publications	<-	getPublications(doc)
	
	keywords	<-	getKeywords (doc,subType)
	
	summaryJSON	<- toJSON(list(
		'version'=as.character(packageVersion(getPackageName())),
		'tiny'=list('text'=tiny.text),
		'medium'=list('title'=medium.title,'text'=medium.summary),
		'full'=list('title'=full.title,'text'=full.text,'publications'=full.publications),
		'keywords'=keywords), auto_unbox = TRUE )
	summaryJSON	<-	sub('NULL. ','',summaryJSON)
	return(summaryJSON)
}