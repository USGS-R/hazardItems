
storm.service = function(serviceEndpoint,attribute){
	subType	<-	attribute

	subTypeDataSrc <- names(synDataSrc)

	doc <- xmlInternalTreeParse(serviceEndpoint)

	title <- xmlValue(getNodeSet(doc,'//citation/citeinfo/title')[[1]])

	dataSrc <-  sapply(getNodeSet(doc,'//dataqual/lineage/srcinfo/srccite/citeinfo/title'),xmlValue)
	sourceStr <-  paste(dataSrc,collapse='|')

	useI = vector(length = length(subTypeDataSrc))

	for (i in 1:length(synDataSrc)){
	  for (k in 1:length(synDataSrc[[i]])){
	    if (grepl(as.character(synDataSrc[[i]][k]),sourceStr)){
	      useI[i] = TRUE
	    }
	  }
	}

	baseType<- substring(subType,1,4)
	stormNum <- substring(subType,5)

	detail <- paste(c('These probabilities apply to a generic representation of a category',
	                  stormNum,'hurricane'),collapse=' ')
	definition  <- typeDef[baseType]
	firstLine <-  paste(c('This datasets includes an element of ', title),collapse='')
	dataSources <- paste(c('Data sources:',paste(c(subTypeDataSrc[useI]),collapse=', ')),collapse=' ')
	summary <- paste(c(firstLine,definition,detail,dataSources),collapse='. ')
	
	summaryJSON	<- toJSON(list('summary'=list(
		'tiny'=list('text'='xxx'),
		'medium'=list('title'='XXX','text'=summary),
		'full'=list('title'='XXX','text'='XXX','publications'='XXX'))), method="C" )
	return(summaryJSON)
}