getLocationString = function(locationContent,size='medium',singleVal=FALSE){
	
	locationNames <- names(locationSynonyms[[size]])
	useI	<-	vector(length = length(locationNames))

	for (i in 1:length(locationSynonyms[[size]])){
	  for (k in 1:length(locationSynonyms[[size]][[i]])){
	    if (grepl(as.character(locationSynonyms[[size]][[i]][k]),locationContent,ignore.case=TRUE)){
			useI[i]	<-	TRUE
			if (singleVal){return(locationNames[i])}
	    }
	  }
	}
	locationString	<- locationNames[useI]
	lenS	<-	length(locationString)
	if (lenS==2){
		locationOut	<-	paste(locationString,collapse=' & ')
	} 
	else if (lenS>2){
		locationOut	<-	paste(c(paste(locationString[1:(lenS-1)],collapse=', '),locationString[lenS]),collapse=' & ')
	} else {locationOut	<-	locationString}
	

	return(locationOut)
}