getLocationString = function(sourceContent){
	
	locationNames <- names(locationSynonyms)
	useI	<-	vector(length = length(locationNames))

	for (i in 1:length(locationSynonyms)){
	  for (k in 1:length(locationSynonyms[[i]])){
	    if (grepl(as.character(locationSynonyms[[i]][k]),sourceContent)){
	      useI[i] = TRUE
	    }
	  }
	}
	locationString	<-	locationNames 
	return(locationString)
}