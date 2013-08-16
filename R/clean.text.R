clean.text	=	function(text){
	
	text	<-	sub("\\n\\n","",text)
	text	<-	sub("\n\n","",text)
	text	<-	sub("\n","",text)
	text	<-	sub("\\. . ","\\. ",text,fixed = TRUE)
	text	<-	sub("\\.. ","\\. ",text,fixed = TRUE)
	text	<-	sub(".. ",". ",text,fixed = TRUE)
	return(text)
}