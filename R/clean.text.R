clean.text	=	function(text,add.period=FALSE){
	
	text	<-	sub("\\n\\n","",text)
	text	<-	sub("\n\n","",text)
	text	<-	sub("\n","",text)
	text	<-	sub("\\. . ","\\. ",text,fixed = TRUE)
	text	<-	sub("\\.. ","\\. ",text,fixed = TRUE)
	text	<-	sub(".. ",". ",text,fixed = TRUE)
	if (tail(text,1)!='.' && add.period){
		text	<-	paste(text,'.',sep='')
	}
	return(text)
}