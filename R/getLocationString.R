getLocationString = function(sourceContent,size='medium',singleVal=FALSE){
	
	locationNames <- names(locationSynonyms[[size]])
	useI	<-	vector(length = length(locationNames))

	for (i in 1:length(locationSynonyms[[size]])){
	  for (k in 1:length(locationSynonyms[[size]][[i]])){
	    if (grepl(as.character(locationSynonyms[[size]][[i]][k]),sourceContent,ignore.case = TRUE)){
			useI[i]	<-	TRUE
			if (singleVal){return(locationNames[i])}
	    }
	  }
	}
	locationString	<- locationNames[useI]
	return(locationString)
}