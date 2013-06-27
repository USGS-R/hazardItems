
historical.service = function(serviceEndpoint,attribute){
	subType <- attribute
	
	subTypeDataSrc <- names(sourceSynonyms)

	doc <- xmlInternalTreeParse(serviceEndpoint)
	
	datasetTitle	<-	xmlValue(getNodeSet(doc,'//citation/citeinfo/title')[[1]])
	purpose <- strsplit(xmlValue(getNodeSet(doc,'//descript/purpose')[[1]]),'.  ') # purpose in text form
	sourceContent	<- purpose[[1]][2]
	
	overview	<-	purpose[[1]][1]
	processDetail	<-	NULL
	sourceString	<-	getSourceString(sourceContent)
	
	if (subType!='date'){ # additional line and details needed
	  # get process source (could be DSASweb in the future) and version***
	  for (j in 1:length(purpose[[1]])){
	    if (grepl("(DSAS)",purpose[[1]][j])){
	      stI <- regexpr('version ',purpose[[1]][j])[1]
	      procDetail<- paste(c(subType,' is a shoreline change metric calculated using the ',
	        'Digital Shoreline Analysis System v',substring(purpose[[1]][j],stI+nchar('version '))),
	        collapse='')
	      break
	    }
	  }
	}

	
	medium.summary	<-	paste(c(overview,processDetail,sourceString),collapse='. ')
	
	location	<-	getLocationString(overview) # default call is to medium service
	medium.title	<-	paste(c(titleMap[['medium']][[subType]],'of shorelines in',location),collapse=' ')
	
	location	<-	getLocationString(overview,size='tiny')
	tiny.text	<-	paste(c(titleMap[['tiny']][[subType]],'for shorelines in',location),collapse=' ')
	
	full.title	<-	paste(c('The',titleMap[['full']][[subType]],'element of',datasetTitle),collapse=' ')
	full.text	<-	sub('\n','',paste(c(purpose[[1]]),collapse='. '))
	
	linkedPubs	<-	sapply(getNodeSet(doc,'//srcinfo/srccite/citeinfo/onlink'),xmlValue)
	linkedTitles	<-	sapply(getNodeSet(doc,'//citeinfo/onlink/parent::node()/title[1]') ,xmlValue)	
	full.publications	<- list()
	for (i in 1:length(linkedPubs)){
		full.publications[linkedTitles[i]]	<- linkedPubs[i]
	}

	summaryJSON	<- toJSON(list('summary'=list(
		'tiny'=list('text'=tiny.text),
		'medium'=list('title'=medium.title,'text'=medium.summary),
		'full'=list('title'=full.title,'text'=full.text,'publications'=full.publications))), method="C" )
	return(summaryJSON)
}
