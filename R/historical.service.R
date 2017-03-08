
#'@title create summary json for an individual historical item
#'@description This service parses the metadata record and creates a summary specific to the 
#'HISTORICAL theme, with subtype being defined by the attribute name.
#'@param serviceEndpoint valid xml, either local or a url
#'@param attribute an attribute to use form the shapefile corresponding to 
#'\code{serviceEndpoint}
#'@return Serialized JSON for summary
#'@importFrom jsonlite toJSON
#'@import XML
#'@examples
#'serviceEndpoint  <-	system.file('extdata',
#'"NewJerseyS_shorelines.shp.xml", package = 'hazardItems')
#'attribute	<-	'date'
#'summary	<-	historical.service(serviceEndpoint,attribute)
#'print(summary)
#'sink('output.json')
#'cat(summary)
#'sink()
#'@export
historical.service = function(serviceEndpoint,attribute){
	
	subType <- tolower(attribute)
	
	subTypeDataSrc <- names(sourceSynonyms)

	doc <- grabXML(serviceEndpoint)
	
	datasetTitle	<-	xmlValue(getNodeSet(doc,'//citation/citeinfo/title')[[1]])
	abstract	<-	xmlValue(getNodeSet(doc,'//descript/abstract')[[1]])
	purpose <- strsplit(xmlValue(getNodeSet(doc,'//descript/purpose')[[1]]),'.  ',fixed=TRUE) # purpose in text form
	process	<-	xmlValue(getNodeSet(doc,'//procstep/procdesc')[[1]])
	sourceContent	<- process
	
	overview	<-	purpose[[1]][1]
	processDetail	<-	NULL
	sourceString	<-	getSourceString(sourceContent)
	
	if (subType!='date' && subType!='date_'){ # additional line and details needed
		# get process source (could be DSASweb in the future) and version***
		sourceString	<-	NULL	# no sources for derivative calculations
	  	for (j in 1:length(purpose[[1]])){
	    	if (grepl("DSAS",purpose[[1]][j])){
	      		stI <- regexpr('version ',purpose[[1]][j])[1]
	      		processDetail<- paste(c(toupper(subType),' is a shoreline change metric calculated using the ',
	        	'Digital Shoreline Analysis System v',substring(purpose[[1]][j],stI+nchar('version '))),
	        	collapse='')
	      		break
	    	}
	  	}
		if (is.null(sourceString)){
			# check abstract for version
			abstract.break    <-	strsplit(abstract,'. ',fixed=TRUE)
			for (j in 1:length(abstract.break[[1]])){
		    	if (grepl("DSAS",abstract.break[[1]][j])){
		      		stI <- regexpr('version ',abstract.break[[1]][j])[1]
		      		processDetail<- paste(c(toupper(subType),' is a shoreline change metric calculated using the ',
		        	'Digital Shoreline Analysis System v',substring(abstract.break[[1]][j],stI+nchar('version '))),
		        	collapse='')
		      		break
		    	}
		  	}
		}
	}
	if (subType %in% c('lrr','epr','wlr')){
		if (grepl("long-term",datasetTitle,ignore.case = TRUE)){
			itemTitle	<-	'Long-term change rates'
			overview	<-	datasetTitle
		} else if (grepl("short-term",datasetTitle,ignore.case = TRUE)){
			itemTitle	<-	'Short-term change rates'
			overview	<-	datasetTitle
		} else {itemTitle	<- titleMap[['medium']][[subType]]	}
		
	} else {itemTitle	<- titleMap[['medium']][[subType]]	}

	
	medium.text	<-	clean.text(paste(c(overview,processDetail,sourceString),collapse='. '),add.period=TRUE)
	
	location	<-	getLocationString(datasetTitle) # default call is to medium service
	medium.title	<-	paste(c(itemTitle,'of shorelines in',location),collapse=' ')
	
	location	<-	getLocationString(datasetTitle,size='tiny')
	tiny.text	<-	paste(c(itemTitle,'of shorelines in',location),collapse=' ')
	
	full.title	<-	paste(c('The',titleMap[['full']][[subType]],'element of',datasetTitle),collapse=' ')
	full.text	<-	clean.text(paste(c(purpose[[1]],abstract),collapse='. '),add.period=TRUE)
	
	full.publications	<-	getPublications(doc)

	keywords	<-	getKeywords (doc,subType)
	
	summaryJSON	<- toJSON(list(
		'version'=as.character(packageVersion(getPackageName())),
		'tiny'=list('text'=tiny.text),
		'medium'=list('title'=medium.title,'text'=medium.text),
		'legend'=medium.title,
		'full'=list('title'=full.title,'text'=full.text,'publications'=full.publications),
		'keywords'=keywords), auto_unbox = TRUE )
	summaryJSON	<-	sub('NULL. ','',summaryJSON)
	return(summaryJSON)
}
