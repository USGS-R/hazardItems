getLocationString = function(sourceContent,size='medium'){
	
	locationNames <- names(locationSynonyms[[size]])
	useI	<-	vector(length = length(locationNames))

	for (i in 1:length(locationSynonyms[[size]])){
	  for (k in 1:length(locationSynonyms[[size]][[i]])){
	    if (grepl(as.character(locationSynonyms[[size]][[i]][k]),sourceContent)){
			locationString	<-	locationNames[i] 
			break
	    }
	  }
	}
	
	return(locationString)
}