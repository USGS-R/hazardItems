getSourceString = function(sourceContent){
	
	subTypeDataSrc <- names(sourceSynonyms)
	useI	<-	vector(length = length(subTypeDataSrc))

	for (i in 1:length(sourceSynonyms)){
	  for (k in 1:length(sourceSynonyms[[i]])){
	    if (grepl(as.character(sourceSynonyms[[i]][k]),sourceContent,ignore.case=TRUE)){
	      useI[i] = TRUE
	    }
	  }
	}
	sourceString	<-	paste(c('Data sources:',
		paste(c(subTypeDataSrc[useI]),collapse=', ')),collapse=' ')
	return(sourceString)
}