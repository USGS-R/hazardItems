
historical.service = function(serviceEndpoint,attribute){
	
	subType <- tolower(attribute)
	
	subTypeDataSrc <- names(sourceSynonyms)

	doc <- xmlInternalTreeParse(serviceEndpoint)
	
	datasetTitle	<-	xmlValue(getNodeSet(doc,'//citation/citeinfo/title')[[1]])
	purpose <- strsplit(xmlValue(getNodeSet(doc,'//descript/purpose')[[1]]),'.  ') # purpose in text form
	sourceContent	<- purpose[[1]][2]
	
	overview	<-	purpose[[1]][1]
	processDetail	<-	NULL
	sourceString	<-	getSourceString(sourceContent)
	
	if (subType!='date' && subType!='date_'){ # additional line and details needed
		# get process source (could be DSASweb in the future) and version***
		sourceString	<-	NULL	# no sources for derivative calculations
	  	for (j in 1:length(purpose[[1]])){
	    	if (grepl("(DSAS)",purpose[[1]][j])){
	      		stI <- regexpr('version ',purpose[[1]][j])[1]
	      		processDetail<- paste(c(subType,' is a shoreline change metric calculated using the ',
	        	'Digital Shoreline Analysis System v',substring(purpose[[1]][j],stI+nchar('version '))),
	        	collapse='')
	      		break
	    	}
	  	}
	}
	if (subType=='lrr'){
		if (grepl("long-term",overview)){
			itemTitle	<-	'Long-term rate of change'
		} else if (grepl("short-term",overview)){
			itemTitle	<-	'Short-term rate of change'
		} else {itemTitle	<- titleMap[['medium']][[subType]]	}
		
	} else {itemTitle	<- titleMap[['medium']][[subType]]	}

	
	medium.summary	<-	paste(c(overview,processDetail,sourceString),collapse='. ')
	
	location	<-	getLocationString(overview) # default call is to medium service
	medium.title	<-	paste(c(itemTitle,'of shorelines in',location),collapse=' ')
	
	location	<-	getLocationString(overview,size='tiny')
	tiny.text	<-	paste(c(itemTitle,'of shorelines in',location),collapse=' ')
	
	full.title	<-	paste(c('The',titleMap[['full']][[subType]],'element of',datasetTitle),collapse=' ')
	full.text	<-	sub('\\n\\n','',sub('\n','',paste(c(purpose[[1]]),collapse='. ')))
	
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

	keywords	<-	getKeywords (doc,subType)
	
	summaryJSON	<- toJSON(list(
		'tiny'=list('text'=tiny.text),
		'medium'=list('title'=medium.title,'text'=medium.summary),
		'full'=list('title'=full.title,'text'=full.text,'publications'=full.publications),
		'keywords'=keywords), method="C" )
	summaryJSON	<-	sub('NULL. ','',summaryJSON)
	return(summaryJSON)
}
