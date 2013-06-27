
historical.service = function(serviceEndpoint,attribute){
	subType <- attribute
	
	subTypeDataSrc <- names(sourceSynonyms)

	doc <- xmlInternalTreeParse(serviceEndpoint)

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

	
	medium.summary <- paste(c(overview,processDetail,sourceString),collapse='. ')
	summaryJSON	<- toJSON(list('summary'=list(
		'tiny'=list('text'='xxx'),
		'medium'=list('title'='XXX','text'=medium.summary),
		'full'=list('title'='XXX','text'='XXX','publications'='XXX'))), method="C" )
	return(summaryJSON)
}
