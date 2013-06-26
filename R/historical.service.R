
historical.service = function(serviceEndpoint,attribute){
	subType <- attribute
	rootDir<- '/Users/jread/Documents/R/coastal-hazards/coastal-hazards-wps/target/test-classes/gov/usgs/cida/coastalhazards/jersey/'

	supportSubTypes <- c('Shorelines','Linear Regression Rate') # more to come...


	if (!any(grepl(subType,supportSubTypes))){stop(c(subType,' is not supported for this type'))}

	# need additional synonyms------
	synDataSrc <- list('lidar'=c('lidar','EAARL Topography'),
	                   'aerial photographs'=c('air photos','aerial photographs'),
	                   'NOAA SLOSH model'=c('SLOSH','Storm Surge Maximum of the Maximum'),
	                   'SWAN model'=c('Simulating WAves Nearshore','SWAN'),
	                   'coastal survey maps'=c('T-sheets','coastal survey maps'))        
	subTypeDataSrc <- names(synDataSrc)

	doc <- xmlInternalTreeParse(serviceEndpoint)

	purpose <- strsplit(xmlValue(getNodeSet(doc,'//descript/purpose')[[1]]),'.  ') # purpose in text form

	# FIND data sources, match to dictionary layperson terms
	useI = vector(length = length(subTypeDataSrc))
	for (i in 1:length(synDataSrc)){
	  for (k in 1:length(synDataSrc[[i]])){
	    if (grepl(as.character(synDataSrc[[i]][k]),purpose[[1]][2])){
	      useI[i] = TRUE
	    }
	  }
	}

	detail <- NULL
	if (subType!='Shorelines'){ # additional line and details needed
	  # get process source (could be DSASweb in the future) and version***
	  for (j in 1:length(purpose[[1]])){
	    if (grepl("(DSAS)",purpose[[1]][j])){
	      print(purpose[[1]][j])
	      stI <- regexpr('version ',purpose[[1]][j])[1]
	      detail<- paste(c(subType,' is a shoreline change metric calculated using the ',
	        'Digital Shoreline Analysis System v',substring(purpose[[1]][j],stI+nchar('version '))),
	                     collapse='')
	      break
	    }
	  }
	}


	dataSources <- paste(c('Data sources:',paste(c(subTypeDataSrc[useI]),collapse=', ')),collapse=' ')
	summary <- paste(c(purpose[[1]][1],detail,dataSources),collapse='. ')
	summaryJSON	<- toJSON(list('summary'=list(
		'tiny'=list('text'='xxx'),
		'medium'=list('title'='XXX','text'=summary),
		'full'=list('title'='XXX','text'='XXX','publications'='XXX'))), method="C" )
	return(summaryJSON)
}
